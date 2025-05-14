# app/routes.py
import numpy as np
import json
from datetime import datetime
import shutil
import os
import uuid
import logging
import pandas as pd
import re
import nltk
from nltk.corpus import stopwords
from datetime import datetime
from flask import Blueprint, render_template, request, jsonify, current_app, session, send_from_directory
from werkzeug.utils import secure_filename
import openai

from .models.data_handler import DataHandler
import app.models.visualization as viz
import app.models.report_generator as report_gen
from .kb import get_knowledge
# Add this import at the top of routes.py alongside other imports
from .models.visualization import is_id_column
from flask import Blueprint, render_template, request, jsonify, current_app, session, send_from_directory  # ensure send_from_directory is imported
import sqlite3

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Create blueprint
main_bp = Blueprint('main', __name__)

# Helper function to get the interaction logger
def get_interaction_logger():
    """Get the interaction logger from app config"""
    return current_app.config.get('INTERACTION_LOGGER')

# --- Load Stopwords (Do this once when the module loads) ---
try:
    nltk_stopwords = set(stopwords.words('english'))
    logger.info(f"Loaded {len(nltk_stopwords)} English stopwords from NLTK.")
except LookupError:
    logger.warning("NLTK stopwords corpus not found. Downloading it now...")
    try:
        nltk.download('stopwords', quiet=True)
        nltk_stopwords = set(stopwords.words('english'))
        logger.info(f"Successfully downloaded and loaded {len(nltk_stopwords)} NLTK stopwords.")
    except Exception as e:
        logger.error(f"Failed to download NLTK stopwords: {e}")
        # Provide a minimal fallback list if download failed
        nltk_stopwords = set(['i', 'me', 'my', 'myself', 'we', 'our', 'ours', 'ourselves', 'you', 'your', 'yours', 'yourself', 'yourselves', 'he', 'him', 'his', 'himself', 'she', 'her', 'hers', 'herself', 'it', 'its', 'itself', 'they', 'them', 'their', 'theirs', 'themselves', 'what', 'which', 'who', 'whom', 'this', 'that', 'these', 'those', 'am', 'is', 'are', 'was', 'were', 'be', 'been', 'being', 'have', 'has', 'had', 'having', 'do', 'does', 'did', 'doing', 'a', 'an', 'the', 'and', 'but', 'if', 'or', 'because', 'as', 'until', 'while', 'of', 'at', 'by', 'for', 'with', 'about', 'against', 'between', 'into', 'through', 'during', 'before', 'after', 'above', 'below', 'to', 'from', 'up', 'down', 'in', 'out', 'on', 'off', 'over', 'under', 'again', 'further', 'then', 'once', 'here', 'there', 'when', 'where', 'why', 'how', 'all', 'any', 'both', 'each', 'few', 'more', 'most', 'other', 'some', 'such', 'no', 'nor', 'not', 'only', 'own', 'same', 'so', 'than', 'too', 'very', 's', 't', 'can', 'will', 'just', 'don', 'should', 'now'])

# Add domain-specific words and common chat words to exclude from variable extraction
DOMAIN_STOPWORDS = set([
    'analysis', 'analyze', 'analyzed', 'composite', 'score', 'calculation', 'calculate', 'map', 'plot', 'variable',
    'variables', 'data', 'model', 'rank', 'ranking', 'rankings', 'risk', 'vulnerability', 'vulnerable',
    'show', 'tell', 'give', 'list', 'use', 'used', 'using', 'generate', 'create', 'run', 'start',
    'normalized', 'normalization', 'urban', 'extent', 'threshold', 'report', 'download',
    'ward', 'wards', 'wardname', 'file', 'upload', 'please', 'thank', 'thanks', 'ok', 'okay',
    'yes', 'yeah', 'sure', 'hello', 'hi', 'hey', 'help', 'assistant', 'tool',
    'did',  # Keep critical function words if nltk list failed
    'what', 'which', 'how', 'why', 'when', 'where',  # Question words
])

# Combine NLTK and domain stopwords
STOP_WORDS = nltk_stopwords.union(DOMAIN_STOPWORDS)
logger.info(f"Total stopwords including domain-specific: {len(STOP_WORDS)}")

# --- Define patterns for parsing ---
QUESTION_PATTERNS = [
    r'^\s*what\s+', r'^\s*which\s+', r'^\s*how\s+', r'^\s*why\s+',
    r'did you use', r'were used', r'variables\s+used', r'explain\s+the\s+variables',
    r'details\s+on\s+variables', r'can you explain', r'tell me about', r'explanation of'
]
RERUN_KEYWORDS = ['rerun', 'run again', 're-run', 're analyze', 're-analyze']

# Allowed file extensions
ALLOWED_EXTENSIONS_CSV = {'csv', 'xlsx', 'xls'}
ALLOWED_EXTENSIONS_SHP = {'zip'}

def allowed_file(filename, allowed_extensions):
    return '.' in filename and filename.rsplit('.', 1)[1].lower() in allowed_extensions

@main_bp.before_request
def log_session_start():
    """Log session start for new sessions"""
    if request.endpoint and not request.endpoint.startswith('static'):
        session_id = session.get('session_id')
        if session_id:
            # Get browser and IP info
            browser_info = request.user_agent.string
            ip_address = request.remote_addr
            
            # Log session start/activity
            logger = get_interaction_logger()
            if logger:
                logger.log_session_start(session_id, browser_info, ip_address)

@main_bp.route('/')
def index():
    """Render the main page"""
    # Initialize session data if needed
    if 'session_id' not in session:
        session['session_id'] = str(uuid.uuid4())
        session['conversation_history'] = []
        session['data_loaded'] = False
        session['analysis_complete'] = False
        session['csv_loaded'] = False
        session['shapefile_loaded'] = False
        session['current_language'] = 'en'
        # Initialize dialogue state tracking for Phase 3
        session['pending_action'] = None
        session['pending_variables'] = None
        session['last_visualization'] = None
        session['dialogue_context'] = {}
        
        # Log new session
        logger = get_interaction_logger()
        if logger:
            logger.log_session_start(
                session['session_id'], 
                request.user_agent.string, 
                request.remote_addr
            )
    
    return render_template('index.html')


@main_bp.route('/upload_both_files', methods=['POST'])
def upload_both_files():
    """Handle simultaneous upload of both CSV and shapefile files"""
    response = {'status': 'error', 'message': 'No files received'}
    
    # Check if files were provided
    csv_file = None
    shapefile = None
    
    if 'csv_file' in request.files:
        csv_file = request.files['csv_file']
        if csv_file.filename == '':
            csv_file = None
    
    if 'shapefile' in request.files:
        shapefile = request.files['shapefile']
        if shapefile.filename == '':
            shapefile = None
    
    if not csv_file and not shapefile:
        return jsonify({'status': 'error', 'message': 'No files selected'}), 400
    
    # Create session folder
    session_folder = os.path.join(current_app.config['UPLOAD_FOLDER'], session.get('session_id', 'default'))
    os.makedirs(session_folder, exist_ok=True)
    
    # Process CSV file if provided
    csv_result = None
    if csv_file and allowed_file(csv_file.filename, ALLOWED_EXTENSIONS_CSV):
        csv_filename = secure_filename(csv_file.filename)
        csv_path = os.path.join(session_folder, csv_filename)
        csv_file.save(csv_path)
        
        # Process the CSV file
        data_handler = DataHandler(session_folder)
        csv_result = data_handler.load_csv(csv_path)
        
        if csv_result['status'] == 'success':
            session['csv_loaded'] = True
            session['csv_filename'] = csv_filename
            session['csv_rows'] = csv_result.get('rows', 0)
            session['csv_columns'] = csv_result.get('columns', 0)
            
            # Extract and store all available variables
            available_variables = get_available_variables(data_handler)
            session['available_variables'] = available_variables
            # Also store variable metadata for better matching
            session['variable_metadata'] = extract_variable_metadata(data_handler)
            
            # Log the file upload
            logger = get_interaction_logger()
            if logger and session.get('session_id'):
                metadata = {
                    'rows': csv_result.get('rows', 0),
                    'columns': csv_result.get('columns', 0),
                    'missing_values': len(csv_result.get('missing_columns', [])),
                    'available_variables': available_variables[:10]  # Just log first 10 to avoid huge logs
                }
                logger.log_file_upload(
                    session.get('session_id'),
                    'csv',
                    csv_filename,
                    os.path.getsize(csv_path),
                    metadata
                )
        else:
            # Log the error
            logger = get_interaction_logger()
            if logger and session.get('session_id'):
                logger.log_error(
                    session.get('session_id'),
                    'csv_upload_error',
                    csv_result.get('message', 'Unknown error processing CSV file')
                )
    elif csv_file:
        csv_result = {'status': 'error', 'message': 'Invalid CSV file type'}
        
    # Process shapefile if provided
    shp_result = None
    if shapefile and allowed_file(shapefile.filename, ALLOWED_EXTENSIONS_SHP):
        shp_filename = secure_filename(shapefile.filename)
        shp_path = os.path.join(session_folder, shp_filename)
        shapefile.save(shp_path)
        
        # Process the shapefile
        data_handler = DataHandler(session_folder)
        shp_result = data_handler.load_shapefile(shp_path)
        
        if shp_result['status'] == 'success':
            session['shapefile_loaded'] = True
            session['shapefile_filename'] = shp_filename
            session['shapefile_features'] = shp_result.get('features', 0)
            
            # Log the file upload
            logger = get_interaction_logger()
            if logger and session.get('session_id'):
                metadata = {
                    'features': shp_result.get('features', 0),
                    'crs': shp_result.get('crs', ''),
                    'has_mismatches': shp_result.get('mismatches') is not None
                }
                logger.log_file_upload(
                    session.get('session_id'),
                    'shapefile',
                    shp_filename,
                    os.path.getsize(shp_path),
                    metadata
                )
        else:
            # Log the error
            logger = get_interaction_logger()
            if logger and session.get('session_id'):
                logger.log_error(
                    session.get('session_id'),
                    'shapefile_upload_error',
                    shp_result.get('message', 'Unknown error processing shapefile')
                )
    elif shapefile:
        shp_result = {'status': 'error', 'message': 'Invalid shapefile type'}
        
    # If both files are uploaded, check for ward name mismatches
    if session.get('csv_loaded', False) and session.get('shapefile_loaded', False):
        # We need to use either the data_handler from CSV or shapefile processing
        # Prioritize using the one that was just processed
        mismatches = None
        if csv_result and shp_result:
            # If both were uploaded at once, recreate a fresh data handler
            data_handler = DataHandler(session_folder)
            # Load both files again to ensure consistency
            data_handler.load_csv(os.path.join(session_folder, session['csv_filename']))
            data_handler.load_shapefile(os.path.join(session_folder, session['shapefile_filename']))
            mismatches = data_handler.check_wardname_mismatches()
        elif csv_result:
            # CSV was just uploaded, load the shapefile data into the handler
            data_handler.load_shapefile(os.path.join(session_folder, session['shapefile_filename']))
            mismatches = data_handler.check_wardname_mismatches()
        elif shp_result:
            # Shapefile was just uploaded, load the CSV data into the handler
            data_handler.load_csv(os.path.join(session_folder, session['csv_filename']))
            mismatches = data_handler.check_wardname_mismatches()
        
        if mismatches and len(mismatches) > 0:
            if shp_result:
                shp_result['mismatches'] = mismatches
                shp_result['status'] = 'warning'
                shp_result['message'] = f'Shapefile loaded but found {len(mismatches)} ward name mismatches'
            elif csv_result:
                csv_result['mismatches'] = mismatches
                csv_result['status'] = 'warning'
                csv_result['message'] = f'CSV loaded but found {len(mismatches)} ward name mismatches'
        
        # Create analysis prompt
        analysis_prompt = f"""
        <p><strong>Excellent! All files are now loaded successfully!</strong></p>
        <p>Your data includes:</p>
        <ul>
            <li>📊 CSV data: {session.get('csv_rows', 0)} rows with {session.get('csv_columns', 0)} columns</li>
            <li>🗺️ Shapefile data: {session.get('shapefile_features', 0)} features</li>
        </ul>
        <div class="analysis-ready-prompt">
            <p><strong>🚀 Everything is ready for analysis!</strong></p>
            <p>Type "Run the analysis" to begin processing your data.</p>
            <button class="btn btn-primary mt-2" onclick="document.getElementById('message-input').value='Run the analysis'; document.getElementById('send-message').click();">
                Start Analysis
            </button>
        </div>
        """
    
    # Prepare final response based on which files were processed
    if csv_result and shp_result:
        # Both files were uploaded
        if csv_result['status'] == 'success' and shp_result['status'] in ['success', 'warning']:
            response = {
                'status': 'success',
                'message': 'Both files uploaded successfully',
                'csv_result': csv_result,
                'shp_result': shp_result
            }
            if session.get('csv_loaded', False) and session.get('shapefile_loaded', False):
                response['analysis_prompt'] = analysis_prompt
        else:
            # At least one upload failed
            response = {
                'status': 'error',
                'message': 'One or more file uploads failed',
                'csv_result': csv_result,
                'shp_result': shp_result
            }
    elif csv_result:
        # Only CSV was uploaded
        response = {
            'status': csv_result['status'],
            'message': csv_result['message'],
            'csv_result': csv_result
        }
        if csv_result['status'] == 'success' and not session.get('shapefile_loaded', False):
            response['note'] = 'CSV loaded successfully. Please upload a shapefile.'
    elif shp_result:
        # Only shapefile was uploaded
        response = {
            'status': shp_result['status'],
            'message': shp_result['message'],
            'shp_result': shp_result
        }
        if shp_result['status'] in ['success', 'warning'] and not session.get('csv_loaded', False):
            response['note'] = 'Shapefile loaded successfully. Please upload a CSV file.'
    
    return jsonify(response)


@main_bp.route('/upload_csv', methods=['POST'])
def upload_csv():
    """Handle CSV file upload"""
    if 'file' not in request.files:
        return jsonify({'status': 'error', 'message': 'No file part'}), 400
    
    file = request.files['file']
    
    if file.filename == '':
        return jsonify({'status': 'error', 'message': 'No file selected'}), 400
    
    if file and allowed_file(file.filename, ALLOWED_EXTENSIONS_CSV):
        filename = secure_filename(file.filename)
        session_folder = os.path.join(current_app.config['UPLOAD_FOLDER'], session.get('session_id', 'default'))
        os.makedirs(session_folder, exist_ok=True)
        
        file_path = os.path.join(session_folder, filename)
        file.save(file_path)
        
        # Process the CSV file
        data_handler = DataHandler(session_folder)
        result = data_handler.load_csv(file_path)
        
        if result['status'] == 'success':
            session['csv_loaded'] = True
            session['csv_filename'] = filename
            session['csv_rows'] = result.get('rows', 0)
            session['csv_columns'] = result.get('columns', 0)
            
            # Extract and store all available variables
            available_variables = get_available_variables(data_handler)
            session['available_variables'] = available_variables
            # Also store variable metadata for better matching
            session['variable_metadata'] = extract_variable_metadata(data_handler)
            
            # Log the file upload
            logger = get_interaction_logger()
            if logger and session.get('session_id'):
                metadata = {
                    'rows': result.get('rows', 0),
                    'columns': result.get('columns', 0),
                    'missing_values': len(result.get('missing_columns', [])),
                    'available_variables': available_variables[:10]  # Just log first 10 to avoid huge logs
                }
                logger.log_file_upload(
                    session.get('session_id'),
                    'csv',
                    filename,
                    os.path.getsize(file_path),
                    metadata
                )
            
            return jsonify({
                'status': 'success', 
                'message': f'CSV file {filename} uploaded successfully',
                'rows': result.get('rows', 0),
                'columns': result.get('columns', 0),
                'missing_values': result.get('missing_values', 0),
                'available_variables': available_variables
            })
        else:
            # Log the error
            logger = get_interaction_logger()
            if logger and session.get('session_id'):
                logger.log_error(
                    session.get('session_id'),
                    'csv_upload_error',
                    result.get('message', 'Unknown error processing CSV file')
                )
            return jsonify({'status': 'error', 'message': result.get('message', 'Failed to process CSV file')}), 400
    
    return jsonify({'status': 'error', 'message': 'Invalid file type'}), 400

@main_bp.route('/upload_shapefile', methods=['POST'])
def upload_shapefile():
    """Handle shapefile (ZIP) upload"""
    if 'file' not in request.files:
        return jsonify({'status': 'error', 'message': 'No file part'}), 400
    
    file = request.files['file']
    
    if file.filename == '':
        return jsonify({'status': 'error', 'message': 'No file selected'}), 400
    
    if file and allowed_file(file.filename, ALLOWED_EXTENSIONS_SHP):
        filename = secure_filename(file.filename)
        session_folder = os.path.join(current_app.config['UPLOAD_FOLDER'], session.get('session_id', 'default'))
        os.makedirs(session_folder, exist_ok=True)
        
        file_path = os.path.join(session_folder, filename)
        file.save(file_path)
        
        # Process the shapefile
        data_handler = DataHandler(session_folder)
        result = data_handler.load_shapefile(file_path)
        
        if result['status'] == 'success':
            session['shapefile_loaded'] = True
            session['shapefile_filename'] = filename
            session['shapefile_features'] = result.get('features', 0)
            
            # Log the file upload
            logger = get_interaction_logger()
            if logger and session.get('session_id'):
                metadata = {
                    'features': result.get('features', 0),
                    'crs': result.get('crs', ''),
                    'has_mismatches': result.get('mismatches') is not None
                }
                logger.log_file_upload(
                    session.get('session_id'),
                    'shapefile',
                    filename,
                    os.path.getsize(file_path),
                    metadata
                )
            
            # Check for ward name mismatches if CSV is already loaded
            if session.get('csv_loaded', False):
                mismatches = data_handler.check_wardname_mismatches()
                if mismatches and len(mismatches) > 0:
                    return jsonify({
                        'status': 'warning', 
                        'message': f'Shapefile loaded but found {len(mismatches)} ward name mismatches',
                        'features': result.get('features', 0),
                        'mismatches': mismatches
                    })
            
            # Check if both files are loaded
            if session.get('csv_loaded', False) and session.get('shapefile_loaded', False):
                analysis_prompt = f"""
                <p><strong>Excellent! All files are now loaded successfully!</strong></p>
                <p>Your data includes:</p>
                <ul>
                    <li>📊 CSV data: {session.get('csv_rows', 0)} rows with {session.get('csv_columns', 0)} columns</li>
                    <li>🗺️ Shapefile data: {result.get('features', 0)} features</li>
                </ul>
                <div class="analysis-ready-prompt">
                    <p><strong>🚀 Everything is ready for analysis!</strong></p>
                    <p>Type "Run the analysis" to begin processing your data.</p>
                    <button class="btn btn-primary mt-2" onclick="document.getElementById('message-input').value='Run the analysis'; document.getElementById('send-message').click();">
                        Start Analysis
                    </button>
                </div>
                """
                
                return jsonify({
                    'status': 'success', 
                    'message': f'Shapefile {filename} uploaded successfully',
                    'features': result.get('features', 0),
                    'analysis_prompt': analysis_prompt
                })
            else:
                return jsonify({
                    'status': 'success', 
                    'message': f'Shapefile {filename} uploaded successfully',
                    'features': result.get('features', 0),
                    'note': 'Waiting for CSV file...'
                })
        else:
            # Log the error
            logger = get_interaction_logger()
            if logger and session.get('session_id'):
                logger.log_error(
                    session.get('session_id'),
                    'shapefile_upload_error',
                    result.get('message', 'Unknown error processing shapefile')
                )
            return jsonify({'status': 'error', 'message': result.get('message', 'Failed to process shapefile')}), 400
    
    return jsonify({'status': 'error', 'message': 'Invalid file type'}), 400

@main_bp.route('/run_analysis', methods=['POST'])
def run_analysis():
    """Run the analysis directly (used for API calls, not main chat flow)"""
    try:
        # Get session folder path
        session_id = session.get('session_id', 'default')
        session_folder = os.path.join(current_app.config['UPLOAD_FOLDER'], session_id)
        
        # Get custom parameters - now only supports selected_variables
        data = request.json or {}
        selected_variables = data.get('selected_variables', None)
        
        # Initialize data handler with session folder
        data_handler = DataHandler(session_folder)
        
        # Check if both files are loaded
        if not session.get('csv_loaded', False) or not session.get('shapefile_loaded', False):
            return jsonify({
                'status': 'error',
                'message': 'Please upload both CSV and shapefile data before running analysis'
            }), 400
        
        # Load the CSV data
        csv_filename = session.get('csv_filename', '')
        if csv_filename:
            csv_result = data_handler.load_csv(os.path.join(session_folder, csv_filename))
            if csv_result['status'] != 'success':
                return jsonify({'status': 'error', 'message': 'Failed to load CSV data'})
        
        # Load the shapefile data 
        shapefile_filename = session.get('shapefile_filename', '')
        if shapefile_filename:
            shp_result = data_handler.load_shapefile(os.path.join(session_folder, shapefile_filename))
            if shp_result['status'] != 'success':
                return jsonify({'status': 'error', 'message': 'Failed to load shapefile data'})
        
        # Run the full analysis pipeline with custom variables if provided
        logging.getLogger(__name__).info("Starting full analysis pipeline...")  # CHANGED THIS LINE
        if selected_variables:
            logging.getLogger(__name__).info(f"Using custom variables: {selected_variables}")  # CHANGED THIS LINE
            
            # Clean up variable names
            cleaned_variables = clean_and_validate_variables(data_handler, selected_variables)
            
            if not cleaned_variables or len(cleaned_variables) < 2:
                return jsonify({
                    'status': 'error',
                    'message': 'Could not find valid variables matching your selections. Please specify at least 2 valid variables.',
                    'available_variables': get_available_variables(data_handler)
                }), 400
            
            # Update with cleaned names
            selected_variables = cleaned_variables
            
            # Run analysis with only selected variables
            result = data_handler.run_full_analysis(selected_variables=selected_variables)
        else:
            result = data_handler.run_full_analysis()
        
        if result['status'] == 'success':
            # Store data handler in session config
            current_app.config.setdefault('SESSION_DATA', {})
            current_app.config['SESSION_DATA'][session_id] = {
                'data_handler': data_handler,
                'timestamp': datetime.now()
            }
            
            # Store JSON-serializable data in session
            session['analysis_complete'] = True
            session['analysis_result'] = {
                'variables_used': result.get('variables_used', []),
                'vulnerable_wards': result.get('vulnerable_wards', [])[:5],
                'steps': {
                    'clean': result.get('steps', {}).get('clean', {}).get('message', ''),
                    'normalize': result.get('steps', {}).get('normalize', {}).get('message', ''),
                    'composite': result.get('steps', {}).get('composite', {}).get('message', ''),
                    'ranking': result.get('steps', {}).get('ranking', {}).get('message', ''),
                    'urban': result.get('steps', {}).get('urban', {}).get('message', '')
                }
            }
            
            # Log the analysis event
            interaction_logger = get_interaction_logger()
            if interaction_logger and session.get('session_id'):
                details = {
                    'variables_used': result.get('variables_used', []),
                    'vulnerable_wards': result.get('vulnerable_wards', [])[:5],
                    'custom_variables': selected_variables is not None,
                    'num_variables': len(result.get('variables_used', []))
                }
                interaction_logger.log_analysis_event(
                    session.get('session_id'),
                    'run_analysis',
                    details,
                    True
                )
            
            # Return success response
            return jsonify({
                'status': 'success',
                'message': result.get('message', 'Analysis completed successfully'),
                'steps': result.get('steps', {}),
                'variables_used': result.get('variables_used', []),
                'vulnerable_wards': result.get('vulnerable_wards', [])[:5]
            })
        else:
            # Log the error
            interaction_logger = get_interaction_logger()
            if interaction_logger and session.get('session_id'):
                interaction_logger.log_error(
                    session.get('session_id'),
                    'analysis_error',
                    result.get('message', 'Unknown error running analysis')
                )
            return jsonify({
                'status': 'error',
                'message': result.get('message', 'Error running analysis')
            }), 400
    
    except Exception as e:
        logging.getLogger(__name__).error(f"Error running analysis: {str(e)}")  # CHANGED THIS LINE
        # Log the error
        interaction_logger = get_interaction_logger()
        if interaction_logger and session.get('session_id'):
            import traceback
            interaction_logger.log_error(
                session.get('session_id'),
                'analysis_exception',
                str(e),
                traceback.format_exc()
            )
        return jsonify({
            'status': 'error',
            'message': f'Error running analysis: {str(e)}'
        }), 500

@main_bp.route('/load_sample_data', methods=['POST'])
def load_sample_data():
    """Load pre-packaged sample data into the user's session."""
    try:
        session_id = session.get('session_id')
        if not session_id:
            # Should not happen if session is initialized, but handle anyway
            session['session_id'] = str(uuid.uuid4())
            session_id = session['session_id']
            logger.warning("Session ID not found, generated a new one.")

        logger.info(f"Loading sample data for session: {session_id}")

        # Define paths
        sample_data_dir = os.path.join(current_app.root_path, 'sample_data')
        source_csv_path = os.path.join(sample_data_dir, 'sample_data_template.csv')
        source_zip_path = os.path.join(sample_data_dir, 'sample_boundary_template.zip')

        session_folder = os.path.join(current_app.config['UPLOAD_FOLDER'], session_id)
        os.makedirs(session_folder, exist_ok=True) # Ensure folder exists

        target_csv_path = os.path.join(session_folder, 'sample_data.csv')
        target_zip_path = os.path.join(session_folder, 'sample_boundary.zip')

        # --- Check if sample files exist ---
        if not os.path.exists(source_csv_path) or not os.path.exists(source_zip_path):
             logger.error("Sample data template files not found in app/sample_data/")
             return jsonify({'status': 'error', 'message': 'Sample data files are missing on the server.'}), 500

        # --- Copy sample files to session folder ---
        shutil.copy(source_csv_path, target_csv_path)
        shutil.copy(source_zip_path, target_zip_path)
        logger.info("Sample files copied to session folder.")

        # --- Process copied files using DataHandler ---
        # NOTE: We create a temporary handler just for loading metadata into the session.
        # The main get_data_handler() will be used for actual analysis later.
        temp_data_handler = DataHandler(session_folder)

        # Load CSV and update session
        csv_result = temp_data_handler.load_csv(target_csv_path)
        if csv_result['status'] != 'success':
            logger.error(f"Failed to process sample CSV: {csv_result.get('message')}")
            return jsonify({'status': 'error', 'message': f"Failed to process sample CSV: {csv_result.get('message')}"}), 500

        session['csv_loaded'] = True
        session['csv_filename'] = 'sample_data.csv'
        session['csv_rows'] = csv_result.get('rows', 0)
        session['csv_columns'] = csv_result.get('columns', 0)
        # Extract and store available variables & metadata 
        available_variables = get_available_variables(temp_data_handler)
        session['available_variables'] = available_variables
        session['variable_metadata'] = extract_variable_metadata(temp_data_handler)
        logger.info("Sample CSV processed and session updated.")

        # Load Shapefile and update session
        shp_result = temp_data_handler.load_shapefile(target_zip_path)
        if shp_result['status'] != 'success':
             logger.error(f"Failed to process sample Shapefile: {shp_result.get('message')}")
             return jsonify({'status': 'error', 'message': f"Failed to process sample Shapefile: {shp_result.get('message')}"}), 500

        session['shapefile_loaded'] = True
        session['shapefile_filename'] = 'sample_boundary.zip'
        session['shapefile_features'] = shp_result.get('features', 0)
        logger.info("Sample Shapefile processed and session updated.")
        
        # Log sample data loading
        logger = get_interaction_logger()
        if logger:
            # Log CSV sample
            logger.log_file_upload(
                session_id,
                'sample_csv',
                'sample_data.csv',
                os.path.getsize(target_csv_path),
                {'rows': csv_result.get('rows', 0), 'columns': csv_result.get('columns', 0)}
            )
            
            # Log shapefile sample
            logger.log_file_upload(
                session_id,
                'sample_shapefile',
                'sample_boundary.zip',
                os.path.getsize(target_zip_path),
                {'features': shp_result.get('features', 0)}
            )
            
            # Log the event
            logger.log_analysis_event(
                session_id,
                'load_sample_data',
                {'success': True},
                True
            )

        # --- Prepare response ---
        # Generate the 'analysis ready' prompt
        analysis_prompt = f"""
        <p><strong>Sample data loaded successfully!</strong></p>
        <p>The sample dataset includes:</p>
        <ul>
            <li>📊 CSV data: {session.get('csv_rows', 0)} rows with {session.get('csv_columns', 0)} columns</li>
            <li>🗺️ Shapefile data: {session.get('shapefile_features', 0)} features</li>
        </ul>
        <div class="analysis-ready-prompt">
            <p><strong>🚀 Everything is ready for analysis!</strong></p>
            <p>Type "Run the analysis" or click the button below to begin processing the sample data.</p>
            <button class="btn btn-primary mt-2" onclick="document.getElementById('message-input').value='Run the analysis'; document.getElementById('send-message').click();">
                Start Analysis on Sample Data
            </button>
        </div>
        """

        return jsonify({
            'status': 'success',
            'message': 'Sample data loaded successfully.',
            'rows': session.get('csv_rows', 0),
            'columns': session.get('csv_columns', 0),
            'features': session.get('shapefile_features', 0),
            'analysis_prompt': analysis_prompt
        })

    except Exception as e:
        logger.error(f"Error loading sample data: {str(e)}", exc_info=True)
        # Log the error
        logger = get_interaction_logger()
        if logger and session.get('session_id'):
            import traceback
            logger.log_error(
                session.get('session_id'),
                'sample_data_error',
                str(e),
                traceback.format_exc()
            )
        # Clean up potentially partially copied files? Maybe not necessary.
        return jsonify({'status': 'error', 'message': f'An internal error occurred while loading sample data: {str(e)}'}), 500

@main_bp.route('/serve_viz_file/<session_id>/<path:filename>')
def serve_viz_file(session_id, filename):
    """Serve visualization files (HTML) from the session's upload folder in the instance path."""
    # UPLOAD_FOLDER now points to instance_path/uploads
    directory = os.path.join(current_app.config['UPLOAD_FOLDER'], session_id)
    
    # Security check: ensure the filename is safe and doesn't try to escape the directory
    safe_path = os.path.abspath(os.path.join(directory, filename))
    if not safe_path.startswith(os.path.abspath(directory)):
        logger.error(f"Attempt to access unsafe path: {filename}")
        return jsonify({'status': 'error', 'message': 'Invalid file path.'}), 400
    
    if not os.path.exists(safe_path):
        logger.error(f"Visualization file not found: {safe_path}")
        return jsonify({'status': 'error', 'message': 'Visualization file not found.'}), 404
    try:
        return send_from_directory(directory, filename)
    except Exception as e:
        logger.error(f"Error serving viz file {filename} for session {session_id}: {e}")
        return jsonify({'status': 'error', 'message': 'Could not serve visualization file.'}), 500

@main_bp.route('/get_visualization', methods=['POST'])
def get_visualization():
    """Handle visualization requests directly"""
    data = request.json
    viz_type = data.get('type', '')
    variable = data.get('variable', None)
    threshold = data.get('threshold', 30)
    
    # Get data handler from session
    data_handler = get_data_handler()
    
    # Check if analysis is complete, except for variable maps which can be viewed anytime
    if not session.get('analysis_complete', False) and viz_type not in ['variable_map']:
        return jsonify({
            'status': 'error',
            'message': 'Analysis has not been run yet. Please run the analysis first.',
            'ai_response': "I need to run the analysis before I can show you visualizations. Would you like me to run the analysis now?"
        })
    
    # Update session to track last visualization for context
    session['last_visualization'] = {
        'type': viz_type,
        'variable': variable,
        'threshold': threshold,
        'timestamp': datetime.now().isoformat()
    }
    
    # Handle different visualization types properly
    try:
        result = get_visualization_result({
            'type': viz_type,
            'variable': variable,
            'threshold': threshold
        }, data_handler)
        
        # Log the visualization request
        interaction_logger = get_interaction_logger()
        if interaction_logger and session.get('session_id'):
            details = {
                'viz_type': viz_type,
                'variable': variable,
                'threshold': threshold,
                'success': result.get('status') == 'success'
            }
            interaction_logger.log_analysis_event(
                session.get('session_id'),
                'visualization',
                details,
                result.get('status') == 'success'
            )
        
        # Double-check that required fields are present in successful responses
        if result.get('status') == 'success' and 'image_path' not in result:
            result['status'] = 'error'
            result['message'] = 'Visualization was created but the file path is missing in the response'
            # Use the regular Python logger for detailed info (not the interaction logger)
            logging.getLogger(__name__).error(f"Visualization missing image_path: {result}")
        
        # Ensure the result is serializable
        serializable_result = convert_to_json_serializable(result)
        
        # Debug print the response - use the regular Python logger
        logging.getLogger(__name__).info(f"Visualization response: {json.dumps(serializable_result)[:500]}...")
        
        return jsonify(serializable_result)

    except Exception as e:
        # Use the regular Python logger for detailed error info
        logging.getLogger(__name__).error(f"Error generating visualization: {str(e)}", exc_info=True)
        
        # Log the error using the interaction logger
        interaction_logger = get_interaction_logger()
        if interaction_logger and session.get('session_id'):
            import traceback
            interaction_logger.log_error(
                session.get('session_id'),
                'visualization_error',
                str(e),
                traceback.format_exc()
            )
        
        # Return an error response, ensuring it's also serializable
        error_result = {
            'status': 'error',
            'message': f'Error generating visualization: {str(e)}',
            'ai_response': f"I encountered an error while creating the visualization. Please check the logs or try again."
        }
        return jsonify(convert_to_json_serializable(error_result))
    

@main_bp.route('/navigate_composite_map', methods=['POST'])
def navigate_composite_map():
   """Handle pagination for composite maps"""
   data = request.json
   direction = data.get('direction', '')
   
   if not direction or direction not in ['next', 'prev']:
       return jsonify({'status': 'error', 'message': 'Invalid navigation direction'}), 400
   
   # Get data handler
   data_handler = get_data_handler()
   
   # Get current page from request or session
   current_page = data.get('current_page', session.get('current_composite_map_page', 1))
   
   # Determine new page based on direction
   if direction == 'next':
       new_page = current_page + 1
   else:  # prev
       new_page = max(1, current_page - 1)
   
   # Get the composite map for the new page
   result = viz.create_composite_map(data_handler, new_page)
   
   if result['status'] == 'success':
       # Update session with new page info
       session['current_composite_map_page'] = result.get('current_page', 1)
       
       # Log the navigation event
       logger = get_interaction_logger()
       if logger and session.get('session_id'):
           details = {
               'map_type': 'composite_map',
               'direction': direction,
               'new_page': new_page
           }
           logger.log_analysis_event(
               session.get('session_id'),
               'map_navigation',
               details,
               True
           )
       
       # Ensure all values in the result dictionary are JSON serializable
       result = convert_to_json_serializable(result)
       
       return jsonify(result)
   else:
       # Log the error
       logger = get_interaction_logger()
       if logger and session.get('session_id'):
           logger.log_error(
               session.get('session_id'),
               'map_navigation_error',
               result.get('message', 'Error navigating composite maps')
           )
       return jsonify({
           'status': 'error',
           'message': result.get('message', 'Error navigating composite maps')
       }), 400

@main_bp.route('/navigate_boxplot', methods=['POST'])
def navigate_boxplot():
   """Handle pagination for box and whisker plots"""
   data = request.json
   direction = data.get('direction', '')
   
   if not direction or direction not in ['next', 'prev']:
       return jsonify({'status': 'error', 'message': 'Invalid navigation direction'}), 400
   
   # Get data handler
   data_handler = get_data_handler()
   
   # Check if box plot data is available
   if not hasattr(data_handler, 'boxwhisker_plot') or not data_handler.boxwhisker_plot:
       return jsonify({
           'status': 'error',
           'message': 'Box plot data not available'
       }), 400
   
   # Get current page from request or session
   current_page = data.get('current_page', session.get('current_boxplot_page', 1))
   total_pages = len(data_handler.boxwhisker_plot['plots'])
   
   # Determine new page based on direction
   if direction == 'next':
       new_page = min(current_page + 1, total_pages)
   else:  # prev
       new_page = max(1, current_page - 1)
   
   # Get the plot for the new page
   if 1 <= new_page <= total_pages:
       plot_fig = data_handler.boxwhisker_plot['plots'][new_page - 1]
   else:
       return jsonify({
           'status': 'error',
           'message': f'Invalid page number: {new_page}. Valid range is 1-{total_pages}'
       }), 400
   
   # Save as HTML
   html_path = viz.create_plotly_html(plot_fig, f"vulnerability_plot_page{new_page}.html")
   
   # Update session
   session['current_boxplot_page'] = new_page
   
   # Log the navigation event
   logger = get_interaction_logger()
   if logger and session.get('session_id'):
       details = {
           'plot_type': 'vulnerability_plot',
           'direction': direction,
           'new_page': new_page,
           'total_pages': total_pages
       }
       logger.log_analysis_event(
           session.get('session_id'),
           'plot_navigation',
           details,
           True
       )
   
   # Return result
   result = {
       'status': 'success',
       'message': f'Successfully navigated to box plot page {new_page}',
       'image_path': html_path,
       'current_page': int(new_page),
       'total_pages': int(total_pages),
       'viz_type': 'vulnerability_plot'
   }
   
   # Ensure all values in the result dictionary are JSON serializable
   result = convert_to_json_serializable(result)
   
   return jsonify(result)

@main_bp.route('/update_boxplot_pagination', methods=['POST'])
def update_boxplot_pagination():
   """Update box plot pagination with new wards per page"""
   data = request.json
   wards_per_page = data.get('wards_per_page', 20)
   
   # Get data handler
   data_handler = get_data_handler()
   
   # Check if composite scores are available
   if not hasattr(data_handler, 'composite_scores') or not data_handler.composite_scores:
       return jsonify({
           'status': 'error',
           'message': 'Composite scores not available'
       }), 400
   
   # Generate new box plot with updated wards per page
   box_plot_result = viz.box_plot_function(data_handler.composite_scores['scores'], wards_per_page)
   
   if box_plot_result['status'] == 'success':
       # Store the box plot data for pagination
       data_handler.boxwhisker_plot = box_plot_result
       # Get the first plot
       plot_fig = box_plot_result['plots'][0]
       # Save as HTML
       html_path = viz.create_plotly_html(plot_fig, "vulnerability_plot.html")
       
       # Update session
       session['current_boxplot_page'] = 1
       
       # Log the pagination update
       logger = get_interaction_logger()
       if logger and session.get('session_id'):
           details = {
               'plot_type': 'vulnerability_plot',
               'wards_per_page': wards_per_page,
               'total_pages': box_plot_result['total_pages']
           }
           logger.log_analysis_event(
               session.get('session_id'),
               'update_boxplot_pagination',
               details,
               True
           )
       
       # Return result
       result = {
           'status': 'success',
           'message': 'Successfully updated box plot pagination',
           'image_path': html_path,
           'current_page': 1,
           'total_pages': box_plot_result['total_pages'],
           'viz_type': 'vulnerability_plot'
       }
       
       # Ensure all values in the result dictionary are JSON serializable
       result = convert_to_json_serializable(result)
       
       return jsonify(result)
   else:
       # Log the error
       logger = get_interaction_logger()
       if logger and session.get('session_id'):
           logger.log_error(
               session.get('session_id'),
               'update_boxplot_pagination_error',
               box_plot_result.get('message', 'Error updating box plot pagination')
           )
       return jsonify({
           'status': 'error',
           'message': box_plot_result.get('message', 'Error updating box plot pagination')
       }), 400

@main_bp.route('/download_report/<filename>')
def download_report(filename):
   """Handle report downloads"""
   # REPORTS_FOLDER now points to instance_path/reports
   session_folder = os.path.join(current_app.config['REPORTS_FOLDER'], session.get('session_id', 'default'))
   
   # Security check
   safe_path = os.path.abspath(os.path.join(session_folder, filename))
   if not safe_path.startswith(os.path.abspath(session_folder)):
       logger.error(f"Attempt to access unsafe report path: {filename}")
       return jsonify({'status': 'error', 'message': 'Invalid file path.'}), 400

   if not os.path.exists(safe_path):
       logger.error(f"Report file not found: {safe_path}")
       return jsonify({'status': 'error', 'message': 'Report file not found.'}), 404
   try:
       # Log the report download
       logger = get_interaction_logger()
       if logger and session.get('session_id'):
           details = {
               'report_file': filename,
               'file_size': os.path.getsize(safe_path)
           }
           logger.log_analysis_event(
               session.get('session_id'),
               'report_download',
               details,
               True
           )
       return send_from_directory(session_folder, filename, as_attachment=True)
   except Exception as e:
       logger.error(f"Error serving report file {filename} for session {session.get('session_id', 'default')}: {e}")
       # Log the error
       if logger and session.get('session_id'):
           logger.log_error(
               session.get('session_id'),
               'report_download_error',
               str(e)
           )
       return jsonify({'status': 'error', 'message': 'Could not serve report file.'}), 500

@main_bp.route('/send_message', methods=['POST'])
def send_message():
    """
    Handle chat messages and AI responses (Phase 3 enhanced with dialogue state tracking).
    Manages all conversational flows including pending actions, confirmations, and explanations.
    """
    data = request.json
    user_message = data.get('message', '')
    if not user_message: 
        return jsonify({'status': 'error', 'message': 'No message provided'}), 400

    # Log the user message
    interaction_logger = get_interaction_logger()
    if interaction_logger and session.get('session_id'):
        interaction_logger.log_message(session.get('session_id'), 'user', user_message)

    # Get current session state and data handler
    data_handler = get_data_handler()
    session_state = {
        'csv_loaded': session.get('csv_loaded', False),
        'shapefile_loaded': session.get('shapefile_loaded', False),
        'analysis_complete': session.get('analysis_complete', False),
        'current_language': session.get('current_language', 'en')
    }
    
    # Get available variables for validation
    available_vars = session.get('available_variables', []) # Get actual vars for validation
    if not available_vars and data_handler: # Fallback if not in session
         available_vars = get_available_variables(data_handler) # Assumes this helper exists
         session['available_variables'] = available_vars # Store for next time
    
    # Get variable metadata for better matching
    variable_metadata = session.get('variable_metadata', None)
    if not variable_metadata and data_handler: # Fallback
         variable_metadata = extract_variable_metadata(data_handler) # Assumes this helper exists
         session['variable_metadata'] = variable_metadata # Store for next time
    
    # --- PHASE 3: Dialogue State Tracking ---
    # Check if there's a pending action that requires user confirmation
    pending_action = session.get('pending_action', None)
    pending_variables = session.get('pending_variables', None)
    last_visualization = session.get('last_visualization', None)
    dialogue_context = session.get('dialogue_context', {})

    # --- Handle Custom Analysis Confirmation Flow ---
    if pending_action == 'confirm_custom_analysis' and pending_variables:
        # Check if user confirmed or denied
        user_confirmation = is_confirmation(user_message)
        
        if user_confirmation is True:  # User confirmed
            logger.info(f"User confirmed custom analysis with variables: {pending_variables}")
            
            # Reset pending state
            session['pending_action'] = None
            session['pending_variables'] = None
            
            # Run the custom analysis with the pending variables
            try:
                # Check if both files are loaded
                if not all([session_state['csv_loaded'], session_state['shapefile_loaded']]):
                    return jsonify({
                        'status': 'error', 
                        'response': "Please upload both data files first before running analysis.",
                        'action': 'error'
                    })
                
                result = data_handler.run_full_analysis(selected_variables=pending_variables)
                
                if result['status'] == 'success':
                    # Update session state
                    session['analysis_complete'] = True
                    session['analysis_result'] = {
                        'variables_used': result.get('variables_used', []),
                        'vulnerable_wards': result.get('vulnerable_wards', [])[:5],
                        'variable_selection_method': 'custom',
                        'steps': {k: v.get('message', '') for k, v in result.get('steps', {}).items()}
                    }
                    
                    # Log the custom analysis
                    if interaction_logger:
                        details = {
                            'custom_analysis': True,
                            'variables_used': result.get('variables_used', []),
                            'num_variables': len(result.get('variables_used', [])),
                            'confirmation': True
                        }
                        interaction_logger.log_analysis_event(
                            session.get('session_id'),
                            'custom_analysis_confirmed',
                            details,
                            True
                        )
                    
                    # Prepare success response
                    ai_response = generate_analysis_success_message(result, is_custom=True)
                    
                    # Log assistant response
                    if interaction_logger and session.get('session_id'):
                        interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
                    
                    return jsonify({
                        'status': 'success', 
                        'response': ai_response, 
                        'action': 'analysis_complete'
                    })
                else:
                    # Log the error
                    if interaction_logger:
                        interaction_logger.log_error(
                            session.get('session_id'),
                            'custom_analysis_error',
                            result.get('message', 'Unknown error')
                        )
                    
                    return jsonify({
                        'status': 'error', 
                        'response': f"Error running custom analysis: {result.get('message', 'Unknown error')}", 
                        'action': 'error'
                    })
            except Exception as e:
                logger.error(f"Error running custom analysis after confirmation: {e}", exc_info=True)
                # Log the error
                if interaction_logger:
                    import traceback
                    interaction_logger.log_error(
                        session.get('session_id'),
                        'custom_analysis_exception',
                        str(e),
                        traceback.format_exc()
                    )
                return jsonify({
                    'status': 'error', 
                    'response': f"Error running custom analysis: {str(e)}", 
                    'action': 'error'
                })
                
        elif user_confirmation is False:  # User denied
            logger.info("User cancelled custom analysis")
            
            # Reset pending state
            session['pending_action'] = None
            session['pending_variables'] = None
            
            # Log the cancellation
            if interaction_logger:
                interaction_logger.log_analysis_event(
                    session.get('session_id'),
                    'custom_analysis_cancelled',
                    {'variables': pending_variables},
                    True
                )
            
            # Return cancellation response
            ai_response = "Custom analysis cancelled. Would you like to run the standard analysis instead?"
            
            # Log assistant response
            if interaction_logger and session.get('session_id'):
                interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
            
            return jsonify({
                'status': 'success', 
                'response': ai_response,
                'action': None
            })
        
        else:  # User didn't clearly confirm or deny
            # Keep the pending state and ask for clarification
            ai_response = "I'm not sure if you want to proceed with the custom analysis. Please respond with 'yes' to confirm or 'no' to cancel."
            
            # Log assistant response
            if interaction_logger and session.get('session_id'):
                interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
            
            return jsonify({
                'status': 'success', 
                'response': ai_response,
                'action': None
            })

    # --- Handle Possible Clarification Turn ---
    # If dialogue_context has a 'expecting_clarification' and refers to a certain entity
    if dialogue_context.get('expecting_clarification', False):
        clarification_type = dialogue_context.get('clarification_type')
        
        if clarification_type == 'variable_specification':
            # User was asked to specify a variable - extract and validate it
            extracted_vars = extract_variables(user_message, available_vars, variable_metadata)
            
            if extracted_vars and len(extracted_vars) > 0:
                variable = extracted_vars[0]  # Take the first one if multiple
                viz_type = dialogue_context.get('viz_type', 'variable_map')
                
                # Clear the clarification state
                dialogue_context.pop('expecting_clarification', None)
                dialogue_context.pop('clarification_type', None)
                session['dialogue_context'] = dialogue_context
                
                # Log the clarification
                if interaction_logger:
                    interaction_logger.log_analysis_event(
                        session.get('session_id'),
                        'variable_clarification',
                        {'variable': variable, 'viz_type': viz_type},
                        True
                    )
                
                # Generate the visualization with the clarified variable
                return get_visualization_response(data_handler, viz_type, variable)
            else:
                # Still couldn't get a valid variable, maybe they didn't understand
                dialogue_context.pop('expecting_clarification', None)  # Give up on this clarification
                session['dialogue_context'] = dialogue_context
                
                # Log the failed clarification
                if interaction_logger:
                    interaction_logger.log_analysis_event(
                        session.get('session_id'),
                        'variable_clarification_failed',
                        {'original_message': user_message},
                        False
                    )
                
                available_vars_examples = ", ".join(available_vars[:5]) + ("..." if len(available_vars) > 5 else "")
                ai_response = f"I'm sorry, I still couldn't identify a valid variable from your input. Available variables include: {available_vars_examples}. You could try asking for a specific visualization like 'Show me a map of rainfall' or 'Show me a composite map'."
                
                # Log assistant response
                if interaction_logger and session.get('session_id'):
                    interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
                
                return jsonify({
                    'status': 'success',
                    'response': ai_response
                })

    # --- Phase 2/3: Use LLM for Natural Language Understanding ---
    nlu_result = get_llm_nlu_response(user_message, session_state, available_vars, last_visualization)

    # Fallback to rule-based if LLM NLU fails
    if nlu_result is None:
        logger.warning("LLM NLU failed, falling back to rule-based intent parsing.")
        intent_data = parse_message_intent_fallback(user_message, session_state, data_handler)
        intent = intent_data.get('type', 'general_query')
        entities = intent_data  # Pass the whole dict as potential entities
    else:
        intent = nlu_result.get('intent', 'general_query')
        entities = nlu_result.get('entities', {})

    # --- Process intents based on NLU result ---
    logger.info(f"Detected intent: {intent} with entities: {entities}")

    # --- EXPLANATION INTENTS ---
    # Knowledge base lookups for methodology and variable explanations
    if intent == 'explain_methodology':
        # Get explanation from knowledge base 
        methodology = entities.get('methodology_type')
        if methodology:
            kb_content = get_knowledge('methodology', methodology)
            if kb_content:
                # Log the knowledge base lookup
                if interaction_logger:
                    interaction_logger.log_analysis_event(
                        session.get('session_id'),
                        'explain_methodology',
                        {'methodology_type': methodology},
                        True
                    )
                
                # Log assistant response
                if interaction_logger and session.get('session_id'):
                    interaction_logger.log_message(session.get('session_id'), 'assistant', kb_content)
                
                return jsonify({
                    'status': 'success',
                    'response': kb_content
                })
        
        # If not found or not specified, provide an overview
        ai_response = "The MRPT tool uses several methodologies to analyze malaria risk:\n\n1. **Data Cleaning** - Handles missing values through spatial imputation and other methods\n2. **Normalization** - Scales variables to 0-1 range based on their relationship with risk\n3. **Composite Scoring** - Combines variables to create risk models\n4. **Vulnerability Ranking** - Orders wards by risk priority\n5. **Urban Extent Analysis** - Classifies areas for intervention planning\n\nWhich aspect would you like me to explain in more detail?"
        
        # Log assistant response
        if interaction_logger and session.get('session_id'):
            interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
        
        return jsonify({
            'status': 'success',
            'response': ai_response
        })
    
    elif intent == 'explain_variable':
        # Look up the variable explanation in the knowledge base
        variable = entities.get('variable_name')
        if variable:
            # Try to match to a standard variable name
            matched_vars = match_variables_to_dataset([variable], available_vars, variable_metadata)
            var_to_explain = matched_vars[0] if matched_vars else variable
            
            kb_content = get_knowledge(var_to_explain)
            if kb_content:
                # Log the knowledge base lookup
                if interaction_logger:
                    interaction_logger.log_analysis_event(
                        session.get('session_id'),
                        'explain_variable',
                        {'variable_name': var_to_explain},
                        True
                    )
                
                # Log assistant response
                if interaction_logger and session.get('session_id'):
                    interaction_logger.log_message(session.get('session_id'), 'assistant', kb_content)
                
                return jsonify({
                    'status': 'success',
                    'response': kb_content
                })
            else:
                # Variable not found in KB, generate a generic response
                ai_response = f"The variable '{var_to_explain}' is used in malaria risk analysis, but I don't have specific details about its relationship with malaria transmission. Generally, variables in our dataset relate to environmental conditions, demographics, or epidemiological measures."
                
                # Log assistant response
                if interaction_logger and session.get('session_id'):
                    interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
                
                return jsonify({
                    'status': 'success',
                    'response': ai_response
                })
        else:
            # No specific variable mentioned, provide overview of variable categories
            ai_response = "Variables in malaria risk analysis typically fall into three categories:\n\n1. **Environmental** - Rainfall, temperature, vegetation indices, elevation, distance to water, etc.\n2. **Demographic** - Population density, housing quality, urban/rural classification, etc.\n3. **Epidemiological** - Parasite rates, test positivity, case counts, etc.\n\nWhich type of variable would you like to learn more about?"
            
            # Log assistant response
            if interaction_logger and session.get('session_id'):
                interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
            
            return jsonify({
                'status': 'success',
                'response': ai_response
            })
    
    elif intent == 'explain_variable_category':
        # Look up explanation for a category of variables
        category = entities.get('variable_category', '')
        kb_content = get_knowledge('variables', category)
        if kb_content:
            # Log the knowledge base lookup
            if interaction_logger:
                interaction_logger.log_analysis_event(
                    session.get('session_id'),
                    'explain_variable_category',
                    {'variable_category': category},
                    True
                )
            
            # Log assistant response
            if interaction_logger and session.get('session_id'):
                interaction_logger.log_message(session.get('session_id'), 'assistant', kb_content)
            
            return jsonify({
                'status': 'success',
                'response': kb_content
            })
        else:
            ai_response = "I can explain three categories of variables used in malaria risk analysis:\n\n1. **Environmental variables** - Physical factors like rainfall, temperature, and elevation\n2. **Demographic variables** - Human factors like population density and housing quality\n3. **Epidemiological variables** - Disease measures like parasite rates and case counts\n\nWhich would you like to learn about?"
            
            # Log assistant response
            if interaction_logger and session.get('session_id'):
                interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
            
            return jsonify({
                'status': 'success',
                'response': ai_response
            })
    
    # --- QUERY ANALYSIS DETAILS ---
    elif intent == 'query_analysis_details':
        if session_state['analysis_complete']:
             analysis_result = session.get('analysis_result', {})
             variables_used = analysis_result.get('variables_used', [])
             
             if variables_used:
                vars_list_str = ", ".join(f"**{var}**" for var in variables_used)
                ai_response = f"The last analysis used these variables for the composite score: {vars_list_str}."
                
                # Provide additional context about variable selection method
                selection_method = analysis_result.get('variable_selection_method', 'default')
                if selection_method == 'custom':
                    ai_response += " These were the custom variables you specified."
                else:
                    ai_response += " These were selected based on their known relationship with malaria risk."
                
                # Log the query
                if interaction_logger:
                    interaction_logger.log_analysis_event(
                        session.get('session_id'),
                        'query_analysis_details',
                        {'variables_used': variables_used, 'selection_method': selection_method},
                        True
                    )
                
                # Log assistant response
                if interaction_logger and session.get('session_id'):
                    interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
                
                return jsonify({'status': 'success', 'response': ai_response})
             else:
                ai_response = "Analysis is complete, but the specific variables used weren't recorded for this session."
                
                # Log assistant response
                if interaction_logger and session.get('session_id'):
                    interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
                
                return jsonify({'status': 'success', 'response': ai_response})
        else:
             ai_response = "Analysis hasn't run yet. Run the analysis first, then ask about variables."
             
             # Log assistant response
             if interaction_logger and session.get('session_id'):
                 interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
             
             return jsonify({'status': 'success', 'response': ai_response})

    # --- RUN STANDARD ANALYSIS ---
    elif intent == 'run_standard_analysis':
        # Check data loaded
        if not all([session_state['csv_loaded'], session_state['shapefile_loaded']]):
             return jsonify({'status': 'error', 'response': "Please upload both data files first.", 'action': 'error'})
        
        try:
            result = data_handler.run_full_analysis(selected_variables=None)
            if result['status'] == 'success':
                # Update session
                session['analysis_complete'] = True
                session['analysis_result'] = {
                    'variables_used': result.get('variables_used', []),
                    'vulnerable_wards': result.get('vulnerable_wards', [])[:5],
                    'variable_selection_method': 'default',
                     'steps': {k: v.get('message', '') for k, v in result.get('steps', {}).items()}
                }
                
                # Log the analysis
                if interaction_logger:
                    details = {
                        'standard_analysis': True,
                        'variables_used': result.get('variables_used', []),
                        'num_variables': len(result.get('variables_used', []))
                    }
                    interaction_logger.log_analysis_event(
                        session.get('session_id'),
                        'standard_analysis',
                        details,
                        True
                    )
                
                ai_response = generate_analysis_success_message(result, is_custom=False)
                
                # Log assistant response
                if interaction_logger and session.get('session_id'):
                    interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
                
                return jsonify({'status': 'success', 'response': ai_response, 'action': 'analysis_complete'})
            else:
                # Log the error
                if interaction_logger:
                    interaction_logger.log_error(
                        session.get('session_id'),
                        'standard_analysis_error',
                        result.get('message', 'Unknown error')
                    )
                return jsonify({'status': 'error', 'response': f"Error: {result.get('message', 'Unknown')}", 'action': 'error'})
        except Exception as e:
             logger.error(f"Run Standard Analysis Error: {e}", exc_info=True)
             # Log the error
             if interaction_logger:
                 import traceback
                 interaction_logger.log_error(
                     session.get('session_id'),
                     'standard_analysis_exception',
                     str(e),
                     traceback.format_exc()
                 )
             return jsonify({'status': 'error', 'response': f"Error: {e}", 'action': 'error'})

    # --- RUN CUSTOM ANALYSIS ---
    elif intent == 'run_custom_analysis':
        if not all([session_state['csv_loaded'], session_state['shapefile_loaded']]):
             return jsonify({'status': 'error', 'response': "Please upload both data files first.", 'action': 'error'})

        variables = entities.get('variable_names', [])
        # Validate variables again even if LLM extracted them, just in case
        variables = match_variables_to_dataset(variables, available_vars, variable_metadata)

        if not variables or len(variables) < 2:
             available_vars_text = ", ".join(available_vars[:10]) + ('...' if len(available_vars) > 10 else '')
             raw_request_vars = entities.get('variable_names', []) # Get original request
             error_msg = f"Could not validate enough variables from your request "
             if raw_request_vars: error_msg += f"({', '.join(raw_request_vars)})"
             error_msg += f". Need >= 2 valid variables. Available: {available_vars_text}. Try again?"
             
             # Log the error
             if interaction_logger:
                 interaction_logger.log_error(
                     session.get('session_id'),
                     'custom_analysis_validation_error',
                     f"Invalid or insufficient variables: {raw_request_vars}"
                 )
             
             # Log assistant response
             if interaction_logger and session.get('session_id'):
                 interaction_logger.log_message(session.get('session_id'), 'assistant', error_msg)
             
             return jsonify({'status': 'error', 'response': error_msg, 'action': 'error'})

        # PHASE 3: Set pending state rather than immediately running
        session['pending_action'] = 'confirm_custom_analysis'
        session['pending_variables'] = variables

        # Log the custom analysis request
        if interaction_logger:
            interaction_logger.log_analysis_event(
                session.get('session_id'),
                'custom_analysis_requested',
                {'variables': variables},
                True
            )

        # Generate confirmation message
        variables_list_html = "<ul>" + "".join([f"<li>{var}</li>" for var in variables]) + "</ul>"
        confirmation_response = f"""
        <p>I can run a custom analysis with these variables:</p>
        {variables_list_html}
        <p>Would you like me to proceed with this custom analysis? Please reply with "yes" or "no".</p>
        """
        
        # Log assistant response
        if interaction_logger and session.get('session_id'):
            interaction_logger.log_message(session.get('session_id'), 'assistant', confirmation_response)
        
        return jsonify({
            'status': 'success',
            'response': confirmation_response
        })

    # --- VISUALIZATION REQUESTS ---
    elif intent == 'request_visualization':
        viz_type = entities.get('visualization_type') # General type like 'map', 'plot'
        map_type = entities.get('map_type')
        plot_type = entities.get('plot_type')
        variable = entities.get('variable_for_viz') # Specific variable for the viz
        threshold = entities.get('threshold_value', 30)

        # Determine specific viz request type
        viz_request = {}
        if map_type:
            if map_type == 'variable': viz_request = {'type': 'variable_map', 'variable': variable}
            elif map_type == 'normalized': viz_request = {'type': 'normalized_map', 'variable': variable}
            elif map_type == 'composite': viz_request = {'type': 'composite_map'}
            elif map_type == 'vulnerability': viz_request = {'type': 'vulnerability_map'}
            elif map_type == 'urban_extent': viz_request = {'type': 'urban_extent_map', 'threshold': threshold}
            else: viz_request = {'type': 'composite_map'} # Default map
        elif plot_type:
            if plot_type == 'vulnerability': viz_request = {'type': 'vulnerability_plot'}
            elif plot_type == 'decision_tree': viz_request = {'type': 'decision_tree'}
            else: viz_request = {'type': 'vulnerability_plot'} # Default plot
        elif viz_type == 'map': # General map request
             viz_request = {'type': 'variable_map', 'variable': variable} if variable else {'type': 'composite_map'}
        elif viz_type == 'plot': # General plot request
             viz_request = {'type': 'vulnerability_plot'}
        elif viz_type == 'tree':
              viz_request = {'type': 'decision_tree'}
        else:
             # Could not determine specific type
             ai_response = "I understand you want a visualization, but couldn't determine which one. Try 'show composite map' or 'plot vulnerability'?"
             
             # Log the error
             if interaction_logger:
                 interaction_logger.log_error(
                     session.get('session_id'),
                     'visualization_type_error',
                     "Could not determine visualization type"
                 )
             
             # Log assistant response
             if interaction_logger and session.get('session_id'):
                 interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
             
             return jsonify({'status':'error', 'response': ai_response, 'action':'error'})

        # Variable maps and normalized maps need a variable - if not provided, ask for clarification
        if (viz_request.get('type') in ['variable_map', 'normalized_map']) and not viz_request.get('variable'):
            # Set up dialogue state to expect variable specification
            dialogue_context['expecting_clarification'] = True
            dialogue_context['clarification_type'] = 'variable_specification'
            dialogue_context['viz_type'] = viz_request.get('type')
            session['dialogue_context'] = dialogue_context
            
            # Log the clarification request
            if interaction_logger:
                interaction_logger.log_analysis_event(
                    session.get('session_id'),
                    'request_variable_clarification',
                    {'viz_type': viz_request.get('type')},
                    True
                )
            
            # Get list of available variables to suggest
            available_vars_examples = ", ".join(available_vars[:5]) + ("..." if len(available_vars) > 5 else "")
            
            ai_response = f"What variable would you like to visualize? Available variables include: {available_vars_examples}"
            
            # Log assistant response
            if interaction_logger and session.get('session_id'):
                interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
            
            return jsonify({
                'status': 'success',
                'response': ai_response
            })

        # Generate visualization and return response
        return get_visualization_response(data_handler, viz_request.get('type'), viz_request.get('variable'), viz_request.get('threshold'))

    # --- GENERATE REPORT ---
    elif intent == 'generate_report':
         # Ensure format is extracted from entities if available
         format_type = entities.get('report_format', 'pdf')
         
         if not session_state['analysis_complete']:
             ai_response = "Please run analysis first."
             
             # Log assistant response
             if interaction_logger and session.get('session_id'):
                 interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
             
             return jsonify({'status': 'error', 'response': ai_response, 'action': 'error'})
             
         try:
            report_result = report_gen.generate_report(data_handler, format=format_type)
            if report_result.get('status') == 'success':
                 report_url = report_result.get('report_url')
                 ai_response = report_result.get('message', f'Report ({format_type.upper()}) ready.')
                 ai_response_html = f'<p>{ai_response}</p><a href="{report_url}" class="btn btn-success mt-2" download target="_blank"><i class="fas fa-download"></i> Download</a>'
                 
                 # Log the report generation
                 if interaction_logger:
                     interaction_logger.log_analysis_event(
                         session.get('session_id'),
                         'generate_report',
                         {'format': format_type, 'report_url': report_url},
                         True
                     )
                 
                 # Log assistant response
                 if interaction_logger and session.get('session_id'):
                     interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response_html)
                 
                 return jsonify({'status': 'success', 'response': ai_response_html, 'report_url': report_url, 'action': 'show_report'})

            else:
                 # Log the error
                 if interaction_logger:
                     interaction_logger.log_error(
                         session.get('session_id'),
                         'report_generation_error',
                         report_result.get('message', 'Unknown error generating report')
                     )
                 
                 ai_response = f"Error: {report_result.get('message', 'Unknown')}"
                 
                 # Log assistant response
                 if interaction_logger and session.get('session_id'):
                     interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
                 
                 return jsonify({'status': 'error', 'response': ai_response, 'action': 'error'})
         except Exception as e:
             logger.error(f"Report Generation Error: {e}", exc_info=True)
             # Log the error
             if interaction_logger:
                 import traceback
                 interaction_logger.log_error(
                     session.get('session_id'),
                     'report_generation_exception',
                     str(e),
                     traceback.format_exc()
                 )
             
             ai_response = f"Error: {e}"
             
             # Log assistant response
             if interaction_logger and session.get('session_id'):
                 interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
             
             return jsonify({'status': 'error', 'response': ai_response, 'action': 'error'})

    # --- CHANGE LANGUAGE ---
    elif intent == 'change_language':
        # Use language_code from entities if available
        language = entities.get('language_code', session_state['current_language']) 
        session['current_language'] = language
        language_names = {'en': 'English', 'ha': 'Hausa', 'yo': 'Yoruba', 'ig': 'Igbo', 'fr': 'French', 'ar': 'Arabic'}
        ai_response = f"Language set to **{language_names.get(language, language)}**."
        
        # Log the language change
        if interaction_logger:
            interaction_logger.update_session_language(session.get('session_id'), language)
            interaction_logger.log_analysis_event(
                session.get('session_id'),
                'change_language',
                {'language': language},
                True
            )
        
        # Log assistant response
        if interaction_logger and session.get('session_id'):
            interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
        
        return jsonify({'status': 'success', 'response': ai_response, 'action': 'language_changed'})

    # --- SIMPLE GREETING/GOODBYE ---
    elif intent == 'greet':
        ai_response = "Hello! How can I help with your malaria risk analysis today?"
        
        # Log assistant response
        if interaction_logger and session.get('session_id'):
            interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
        
        return jsonify({'status': 'success', 'response': ai_response})
    
    elif intent == 'goodbye':
        ai_response = "Goodbye! Feel free to return anytime you need assistance with malaria risk analysis."
        
        # Log assistant response
        if interaction_logger and session.get('session_id'):
            interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
        
        return jsonify({'status': 'success', 'response': ai_response})

    # --- CONTEXT-AWARE FOLLOW-UP FOR VISUALIZATIONS ---
    elif intent == 'viz_followup_question' and last_visualization:
        # User is asking a follow-up about the last visualization shown
        viz_type = last_visualization.get('type')
        variable = last_visualization.get('variable')
        
        # Prepare context about the visualization for the LLM
        context_for_llm = f"The user is asking about a {viz_type.replace('_', ' ')} "
        if variable:
            context_for_llm += f"showing the variable '{variable}'. "
        else:
            context_for_llm += "that doesn't focus on a specific variable. "
            
        if viz_type == 'variable_map':
            context_for_llm += "This map shows the raw values of a variable across different wards. "
            if variable:
                var_knowledge = get_knowledge(variable)
                if var_knowledge:
                    context_for_llm += f"Information about this variable: {var_knowledge}"
        elif viz_type == 'normalized_map':
            context_for_llm += "This map shows normalized values (0-1 scale) of a variable's contribution to malaria risk. "
        elif viz_type == 'composite_map':
            context_for_llm += "This shows multiple maps of different variable combinations and their calculated risk scores. "
        elif viz_type == 'vulnerability_plot':
            context_for_llm += "This shows a box and whisker plot of ward rankings by vulnerability, with most vulnerable wards at the top. "
        elif viz_type == 'vulnerability_map':
            context_for_llm += "This shows a geographical map of ward vulnerability rankings. "
        elif viz_type == 'urban_extent_map':
            threshold = last_visualization.get('threshold', 30)
            context_for_llm += f"This shows urban areas that exceed {threshold}% urbanicity threshold. "
        elif viz_type == 'decision_tree':
            context_for_llm += "This shows the analysis workflow from data loading through variable selection to final risk scores. "
        
        # Use the ai response function with the specific context
        ai_response = generate_ai_response(user_message, session_state, None, session.get('analysis_result'), context_for_llm)
        
        # Log the visualization follow-up
        if interaction_logger:
            interaction_logger.log_analysis_event(
                session.get('session_id'),
                'visualization_followup',
                {'viz_type': viz_type, 'variable': variable},
                True
            )
        
        # Log assistant response
        if interaction_logger and session.get('session_id'):
            interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
        
        return jsonify({'status': 'success', 'response': ai_response})

    # --- ELABORATION REQUESTS --- 
    # In routes.py, in the request_elaboration section:

    elif intent == 'request_elaboration':
        topic = entities.get('topic', '')
        logger.info(f"Handling request for elaboration about: {topic}")
        
        # Get the last topic from dialogue context if needed
        last_topic = dialogue_context.get('last_topic', '')
        
        if not topic and last_topic:
            topic = last_topic
        
        # Get analysis_result from session
        analysis_result = session.get('analysis_result', {})
        
        # Check for common elaboration topics
        if any(word in topic.lower() for word in ['variable', 'parameter', 'variables']):
            # Get data handler for context
            data_handler = get_data_handler()
            
            # Use the LLM for a dynamic response based on the actual query and available data
            ai_response = generate_ai_response_for_variables(
                user_message, 
                analysis_result, 
                data_handler
            )
            
            # Log the elaboration
            if interaction_logger:
                interaction_logger.log_analysis_event(
                    session.get('session_id'),
                    'elaboration_variables',
                    {'topic': topic, 'user_message': user_message},
                    True
                )
            
            # Log assistant response
            if interaction_logger and session.get('session_id'):
                interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
            
            return jsonify({
                'status': 'success',
                'response': ai_response
            })
        
        # Check if it's about methodology
        elif any(word in topic.lower() for word in ['methodology', 'process', 'analysis', 'calculation']):
            # Try to determine which specific methodology
            method_type = None
            if 'missing' in topic.lower() or 'clean' in topic.lower():
                method_type = 'missing_values'
            elif 'normal' in topic.lower():
                method_type = 'normalization'
            elif 'compos' in topic.lower() or 'score' in topic.lower():
                method_type = 'composite_scores'
            elif 'vulnerab' in topic.lower() or 'rank' in topic.lower():
                method_type = 'vulnerability_ranking'
            elif 'urban' in topic.lower():
                method_type = 'urban_extent'
            
            if method_type:
                kb_content = get_knowledge('methodology', method_type)
                if kb_content:
                    # Log the elaboration
                    if interaction_logger:
                        interaction_logger.log_analysis_event(
                            session.get('session_id'),
                            'elaboration_methodology',
                            {'method_type': method_type},
                            True
                        )
                    
                    # Log assistant response
                    if interaction_logger and session.get('session_id'):
                        interaction_logger.log_message(session.get('session_id'), 'assistant', kb_content)
                    
                    return jsonify({
                        'status': 'success',
                        'response': kb_content
                    })
        
        # Check for urban microstratification specifically
        elif 'microstrat' in topic.lower() or ('urban' in topic.lower() and any(word in topic.lower() for word in ['stratification', 'classification', 'categorization'])):
            ai_response = "Urban microstratification involves classifying urban areas at a fine scale based on their characteristics relevant to malaria transmission. In the context of malaria risk analysis, this typically refers to dividing urban areas into distinct ecological zones or strata that might have different risk profiles. This classification helps in targeting interventions more precisely. For example, areas near standing water bodies within an urban environment might have higher malaria risk compared to densely built areas without breeding sites for mosquitoes."
            
            # Log the elaboration
            if interaction_logger:
                interaction_logger.log_analysis_event(
                    session.get('session_id'),
                    'elaboration_urban_microstratification',
                    {},
                    True
                )
            
            # Log assistant response
            if interaction_logger and session.get('session_id'):
                interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
            
            return jsonify({
                'status': 'success',
                'response': ai_response
            })
        
        # If we don't have specific knowledge, get AI to generate an explanation
        context_for_llm = f"The user is asking for more details about {topic}."
        if last_topic:
            context_for_llm += f" They previously asked about {last_topic}."
        
        # Include knowledge base content as context if available
        if topic:
            kb_content = None
            for potential_var in available_vars:
                if potential_var.lower() in topic.lower():
                    kb_content = get_knowledge(potential_var)
                    if kb_content:
                        context_for_llm += f"\n\nInformation about {potential_var}: {kb_content}"
                        break
        
        ai_response = generate_ai_response(user_message, session_state, nlu_result, analysis_result, context_for_llm)
        
        # Log the generic elaboration
        if interaction_logger:
            interaction_logger.log_analysis_event(
                session.get('session_id'),
                'elaboration_generic',
                {'topic': topic},
                True
            )
        
        # Log assistant response
        if interaction_logger and session.get('session_id'):
            interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
        
        return jsonify({'status': 'success', 'response': ai_response})
    
    # --- CLARIFICATION NEEDED ---
    elif intent == 'clarification_needed':
        ai_response = "I'm not quite sure what you're asking. Could you please rephrase your request? For example, are you asking to run analysis, view a map, or something else?"
        
        # Log the clarification needed
        if interaction_logger:
            interaction_logger.log_analysis_event(
                session.get('session_id'),
                'clarification_needed',
                {'original_message': user_message},
                False
            )
        
        # Log assistant response
        if interaction_logger and session.get('session_id'):
            interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
        
        return jsonify({'status': 'success', 'response': ai_response})

    # --- FALLBACK TO GENERAL QUERY ---
    else:  # Default to general_query for any other intent
        logger.info(f"Handling general query or fallback for intent: {intent}")
        
        # Create an appropriate context-aware response
        analysis_result = session.get('analysis_result', None)
        
        # Access knowledge base if needed for specific topics
        context_for_llm = None
        
        # Check if the query might be about a specific topic we have knowledge on
        knowledge_topics = ["rainfall", "temperature", "elevation", "ndvi", "evi", 
                           "distance_to_water", "housing_quality", "population"]
        
        for topic in knowledge_topics:
            if topic.lower() in user_message.lower():
                knowledge = get_knowledge(topic)
                if knowledge:
                    context_for_llm = f"Information about {topic}: {knowledge}"
                    break
        
        # Get AI response, passing context if available
        ai_response = generate_ai_response(user_message, session_state, nlu_result, analysis_result, context_for_llm)
        
        if not ai_response:  # Fallback if LLM fails
            ai_response = get_fallback_response(user_message, session_state)
        
        # Log the general query
        if interaction_logger:
            interaction_logger.log_analysis_event(
                session.get('session_id'),
                'general_query',
                {'intent': intent},
                True
            )
        
        # Log assistant response
        if interaction_logger and session.get('session_id'):
            interaction_logger.log_message(session.get('session_id'), 'assistant', ai_response)
        
        # Save the current topic and intent in dialogue context for better continuity
        dialogue_context = session.get('dialogue_context', {})
        
        # Update last intent
        dialogue_context['last_intent'] = intent
        
        # Track the topic based on intent
        if intent == 'explain_methodology':
            dialogue_context['last_topic'] = entities.get('methodology_type', 'methodology')
        elif intent == 'explain_variable':
            dialogue_context['last_topic'] = entities.get('variable_name', 'variables')
        elif intent == 'explain_variable_category':
            dialogue_context['last_topic'] = entities.get('variable_category', 'variable categories')
        elif intent == 'query_analysis_details':
            dialogue_context['last_topic'] = 'analysis variables'
        elif intent == 'request_visualization':
            dialogue_context['last_topic'] = f"{entities.get('map_type', entities.get('plot_type', 'visualization'))}"
        elif intent == 'request_elaboration':
            # Keep the last topic if we're elaborating on it
            if 'topic' in entities:
                dialogue_context['last_topic'] = entities['topic']
        
        # Store updated context
        session['dialogue_context'] = dialogue_context
        
        return jsonify({'status': 'success', 'response': ai_response})

# --- Admin routes for interaction logging ---

@main_bp.route('/admin/logs', methods=['GET'])
def admin_logs():
    """Admin interface to view interaction logs"""
    # Simple password protection (replace with proper authentication)
    if request.args.get('key') != current_app.config.get('ADMIN_KEY', 'admin'):
        return jsonify({'status': 'error', 'message': 'Unauthorized'}), 401
    
    # Get interaction logger
    logger = get_interaction_logger()
    if not logger:
        return jsonify({'status': 'error', 'message': 'Interaction logger not initialized'}), 500
    
    # Connect to database
    try:
        conn = sqlite3.connect(logger.db_path)
        conn.row_factory = sqlite3.Row  # Return rows as dictionaries
        cursor = conn.cursor()
        
        # Get sessions
        cursor.execute('''
        SELECT * FROM sessions ORDER BY last_activity DESC LIMIT 100
        ''')
        sessions = []
        for row in cursor.fetchall():
            session_dict = dict(row)
            # Ensure last_activity and start_time are strings
            for time_field in ['last_activity', 'start_time']:
                if time_field in session_dict and session_dict[time_field] is not None:
                    session_dict[time_field] = str(session_dict[time_field])
                else:
                    session_dict[time_field] = ""
            sessions.append(session_dict)
        
        # Get message counts by session
        cursor.execute('''
        SELECT session_id, COUNT(*) as message_count FROM messages
        GROUP BY session_id
        ''')
        message_counts = {row['session_id']: row['message_count'] for row in cursor.fetchall()}
        
        # Get error counts by session
        cursor.execute('''
        SELECT session_id, COUNT(*) as error_count FROM errors
        GROUP BY session_id
        ''')
        error_counts = {row['session_id']: row['error_count'] for row in cursor.fetchall()}
        
        # Add counts to sessions
        for session in sessions:
            session_id = session['session_id']
            session['message_count'] = message_counts.get(session_id, 0)
            session['error_count'] = error_counts.get(session_id, 0)
        
        conn.close()
        
        # Add today's date for filtering
        today_date = datetime.now().strftime('%Y-%m-%d')
        
        return render_template('admin_logs.html', sessions=sessions, today_date=today_date)
        
    except Exception as e:
        current_app.logger.error(f"Error retrieving logs: {str(e)}", exc_info=True)
        return jsonify({'status': 'error', 'message': f'Error retrieving logs: {str(e)}'}), 500

@main_bp.route('/admin/session/<session_id>', methods=['GET'])
def admin_session_detail(session_id):
    """View detailed logs for a specific session"""
    # Simple password protection (replace with proper authentication)
    if request.args.get('key') != current_app.config.get('ADMIN_KEY', 'admin'):
        return jsonify({'status': 'error', 'message': 'Unauthorized'}), 401
    
    # Get interaction logger
    logger = get_interaction_logger()
    if not logger:
        return jsonify({'status': 'error', 'message': 'Interaction logger not initialized'}), 500
    
    # Connect to database
    try:
        conn = sqlite3.connect(logger.db_path)
        conn.row_factory = sqlite3.Row  # Return rows as dictionaries
        cursor = conn.cursor()
        
        # Get session info
        cursor.execute('SELECT * FROM sessions WHERE session_id = ?', (session_id,))
        session_info = dict(cursor.fetchone() or {})
        
        if not session_info:
            return jsonify({'status': 'error', 'message': 'Session not found'}), 404
        
        # Get messages
        cursor.execute('''
        SELECT * FROM messages WHERE session_id = ? ORDER BY timestamp
        ''', (session_id,))
        messages = [dict(row) for row in cursor.fetchall()]
        
        # Get file uploads
        cursor.execute('''
        SELECT * FROM file_uploads WHERE session_id = ? ORDER BY timestamp
        ''', (session_id,))
        uploads = [dict(row) for row in cursor.fetchall()]
        
        # Get analysis events
        cursor.execute('''
        SELECT * FROM analysis_events WHERE session_id = ? ORDER BY timestamp
        ''', (session_id,))
        events = [dict(row) for row in cursor.fetchall()]
        
        # Get errors
        cursor.execute('''
        SELECT * FROM errors WHERE session_id = ? ORDER BY timestamp
        ''', (session_id,))
        errors = [dict(row) for row in cursor.fetchall()]
        
        conn.close()
        
        return render_template(
            'admin_session_detail.html', 
            session_info=session_info,
            messages=messages,
            uploads=uploads,
            events=events,
            errors=errors
        )
        
    except Exception as e:
        current_app.logger.error(f"Error retrieving session details: {str(e)}", exc_info=True)
        return jsonify({'status': 'error', 'message': f'Error retrieving session details: {str(e)}'}), 500

@main_bp.route('/admin/export', methods=['GET'])
def admin_export_logs():
    """Export logs as JSON"""
    # Simple password protection (replace with proper authentication)
    if request.args.get('key') != current_app.config.get('ADMIN_KEY', 'admin'):
        return jsonify({'status': 'error', 'message': 'Unauthorized'}), 401
    
    # Get interaction logger
    logger = get_interaction_logger()
    if not logger:
        return jsonify({'status': 'error', 'message': 'Interaction logger not initialized'}), 500
    
    # Connect to database
    try:
        conn = sqlite3.connect(logger.db_path)
        conn.row_factory = sqlite3.Row  # Return rows as dictionaries
        cursor = conn.cursor()
        
        # Get all data
        data = {}
        
        # Get sessions
        cursor.execute('SELECT * FROM sessions')
        data['sessions'] = [dict(row) for row in cursor.fetchall()]
        
        # Get messages
        cursor.execute('SELECT * FROM messages')
        data['messages'] = [dict(row) for row in cursor.fetchall()]
        
        # Get file uploads
        cursor.execute('SELECT * FROM file_uploads')
        data['uploads'] = [dict(row) for row in cursor.fetchall()]
        
        # Get analysis events
        cursor.execute('SELECT * FROM analysis_events')
        data['events'] = [dict(row) for row in cursor.fetchall()]
        
        # Get errors
        cursor.execute('SELECT * FROM errors')
        data['errors'] = [dict(row) for row in cursor.fetchall()]
        
        conn.close()
        
        # Create a response with the JSON data and appropriate headers for download
        response = current_app.response_class(
            response=json.dumps(data, indent=2, default=str),
            status=200,
            mimetype='application/json'
        )
        response.headers["Content-Disposition"] = f"attachment; filename=mrpt_logs_{datetime.datetime.now().strftime('%Y%m%d_%H%M%S')}.json"
        
        return response
        
    except Exception as e:
            current_app.logger.error(f"Error exporting logs: {str(e)}", exc_info=True)
            return jsonify({'status': 'error', 'message': f'Error exporting logs: {str(e)}'}), 500

# --- Helper Functions ---

# Add this function to routes.py - it will forward questions about variables directly to the LLM

def generate_ai_response_for_variables(user_message, analysis_result=None, data_handler=None):
    """
    Generates a detailed AI response specifically for variable-related questions
    using the OpenAI API directly. This avoids hardcoded explanations.
    """
    try:
        api_key = current_app.config.get('OPENAI_API_KEY')
        if not api_key:
            return "I'm unable to provide detailed variable explanations at the moment."
            
        # Get the available variables from the data handler if possible
        available_variables = []
        selected_variables = []
        variable_relationships = {}
        
        if data_handler:
            if hasattr(data_handler, 'csv_data') and data_handler.csv_data is not None:
                available_variables = [col for col in data_handler.csv_data.columns 
                                    if col != 'WardName' and not is_id_column(col)]
            
            if hasattr(data_handler, 'composite_variables') and data_handler.composite_variables:
                selected_variables = data_handler.composite_variables
            
            if hasattr(data_handler, 'variable_relationships'):
                variable_relationships = data_handler.variable_relationships
        
        # If we have analysis_result, get variables from there
        if analysis_result and 'variables_used' in analysis_result:
            selected_variables = analysis_result.get('variables_used', [])
        
        # Create a detailed context for the LLM
        context = f"""
        You are an expert in malaria epidemiology and the Malaria Reprioritization Tool (MRPT) assistant.
        You've been asked to explain about variable selection for composite scores in malaria risk analysis.
        
        Available information about the current analysis:
        - Available variables: {available_variables}
        - Variables selected for analysis: {selected_variables}
        - Variable relationships with risk: {variable_relationships}
        
        Answer the user's question about variables in detail, explaining:
        1. How and why variables were selected
        2. The epidemiological significance of these variables 
        3. How they relate to malaria risk
        4. How they contribute to the composite score calculation
        
        Be comprehensive but easy to understand, and tailor your response specifically to the user's question.
        """
        
        # Call the LLM with the detailed context
        client = openai.OpenAI(api_key=api_key)
        
        messages = [
            {"role": "system", "content": context},
            {"role": "user", "content": user_message}
        ]
        
        response = client.chat.completions.create(
            model="gpt-4o",  # Use latest model for best responses
            messages=messages,
            temperature=0.5,  # Balance between creative and factual
            max_tokens=1000  # Allow for detailed responses
        )
        
        return response.choices[0].message.content
        
    except Exception as e:
        logger.error(f"Error generating AI response for variables: {e}", exc_info=True)
        return f"I apologize, but I encountered an error while generating a detailed explanation: {str(e)}"

def is_confirmation(message):
   """Check if a message is a confirmation or cancellation."""
   message_lower = message.lower().strip()
   
   # Confirmation patterns
   confirm_patterns = [
       r'\byes\b', r'\byeah\b', r'\byep\b', r'\bsure\b', r'\bdefinitely\b', 
       r'\bconfirm\b', r'\baffirmative\b', r'\bproceed\b', r'\bgo ahead\b', 
       r'\bok\b', r'\bokay\b', r'\bfine\b'
   ]
   
   # Cancellation patterns
   cancel_patterns = [
       r'\bno\b', r'\bnope\b', r'\bcancel\b', r'\bdont\b', r'\bdon\'t\b', 
       r'\bstop\b', r'\babort\b', r'\bnegative\b', r'\bwait\b'
   ]
   
   # Check for confirmation
   for pattern in confirm_patterns:
       if re.search(pattern, message_lower):
           return True
   
   # Check for cancellation
   for pattern in cancel_patterns:
       if re.search(pattern, message_lower):
           return False
   
   # No clear indication
   return None


def get_visualization_response(data_handler, viz_type, variable=None, threshold=30):
   """Generate a response for a visualization request."""
   try:
       result = get_visualization_result({
           'type': viz_type,
           'variable': variable,
           'threshold': threshold
       }, data_handler)
       
       # Get proper loggers
       python_logger = logging.getLogger(__name__)
       interaction_logger = get_interaction_logger()
       
       if result['status'] == 'success':
           # Double-check that image_path exists in a success response
           if 'image_path' not in result:
               python_logger.error(f"Missing image_path in successful result: {result}")
               return jsonify({
                   'status': 'error', 
                   'response': "I created the visualization but couldn't retrieve its path. Please try again.", 
                   'action': 'error'
               })
           
           # Update session to track visualization for context
           session['last_visualization'] = {
               'type': viz_type,
               'variable': variable,
               'threshold': threshold,
               'timestamp': datetime.now().isoformat()
           }
           
           # Log visualization in interaction logger
           if interaction_logger and session.get('session_id'):
               details = {
                   'viz_type': viz_type,
                   'variable': variable,
                   'threshold': threshold
               }
               interaction_logger.log_analysis_event(
                   session.get('session_id'),
                   'visualization_success',
                   details,
                   True
               )
               
               # Log assistant response
               interaction_logger.log_message(
                   session.get('session_id'), 
                   'assistant', 
                   result.get('ai_response', 'Here is the visualization.')
               )
           
           # Debug print the format being returned
           python_logger.info(f"Returning visualization response: {json.dumps({
               'status': 'success', 
               'response': result.get('ai_response', 'Here is the visualization:'), 
               'visualization': result.get('image_path', ''), 
               'viz_type': result.get('viz_type', ''), 
               'variable': result.get('variable'), 
               'current_page': result.get('current_page', 1), 
               'total_pages': result.get('total_pages', 1), 
               'action': 'show_visualization'
           })[:500]}...")
           
           return jsonify({
               'status': 'success', 
               'response': result.get('ai_response', 'Here is the visualization:'), 
               'visualization': result.get('image_path', ''), 
               'viz_type': result.get('viz_type', ''), 
               'variable': result.get('variable'), 
               'current_page': result.get('current_page', 1), 
               'total_pages': result.get('total_pages', 1), 
               'action': 'show_visualization'
           })
       else:
           # Log the error
           if interaction_logger and session.get('session_id'):
               interaction_logger.log_error(
                   session.get('session_id'),
                   'visualization_error',
                   result.get('message', 'Error generating visualization')
               )
               
               # Log assistant response
               interaction_logger.log_message(
                   session.get('session_id'), 
                   'assistant', 
                   result.get('ai_response', result.get('message', 'Error generating visualization'))
               )
           
           return jsonify({
               'status': 'error', 
               'response': result.get('ai_response', result.get('message', 'Error generating visualization')), 
               'action': 'error'
           })
   except Exception as e:
       python_logger = logging.getLogger(__name__)
       python_logger.error(f"Visualization Error: {e}", exc_info=True)
       
       # Log the error
       interaction_logger = get_interaction_logger()
       if interaction_logger and session.get('session_id'):
           import traceback
           interaction_logger.log_error(
               session.get('session_id'),
               'visualization_exception',
               str(e),
               traceback.format_exc()
           )
       
       return jsonify({
           'status': 'error', 
           'response': f"Error generating visualization: {e}", 
           'action': 'error'
       })


def get_data_handler():
   """Get the data handler for the current session"""
   session_id = session.get('session_id', 'default')
   session_data = current_app.config.get('SESSION_DATA', {})
   
   if session_id in session_data:
       return session_data[session_id]['data_handler']
   
   # Create new data handler if not found
   session_folder = os.path.join(current_app.config['UPLOAD_FOLDER'], session_id)
   data_handler = DataHandler(session_folder)
   
   # Load files if they exist in session
   csv_filename = session.get('csv_filename', '')
   shapefile_filename = session.get('shapefile_filename', '')
   
   if csv_filename:
       csv_path = os.path.join(session_folder, csv_filename)
       if os.path.exists(csv_path):
           data_handler.load_csv(csv_path)
   
   if shapefile_filename:
       shp_path = os.path.join(session_folder, shapefile_filename)
       if os.path.exists(shp_path):
           data_handler.load_shapefile(shp_path)
   
   # Store in session data
   current_app.config.setdefault('SESSION_DATA', {})
   current_app.config['SESSION_DATA'][session_id] = {
       'data_handler': data_handler,
       'timestamp': datetime.now()
   }
   
   return data_handler


def get_available_variables(data_handler):
   """Get a list of available variables from the dataset"""
   available_vars = []
   
   if data_handler.normalized_data is not None:
       # Get variables from normalized data
       available_vars = [col.replace('normalization_', '') for col in data_handler.normalized_data.columns 
                        if col.startswith('normalization_')]
   elif data_handler.cleaned_data is not None:
       # Get numeric columns from cleaned data
       available_vars = [col for col in data_handler.cleaned_data.columns 
                        if col != 'WardName' and pd.api.types.is_numeric_dtype(data_handler.cleaned_data[col])]
   elif data_handler.csv_data is not None:
       # Get numeric columns from original CSV
       available_vars = [col for col in data_handler.csv_data.columns 
                        if col != 'WardName' and pd.api.types.is_numeric_dtype(data_handler.csv_data[col])]
   
   return available_vars


def base_name_from_parts(parts):
   """Convert variable parts to a base name format."""
   if not parts:
       return ""
   
   # Join with underscore and lowercase
   base_name = "_".join(parts).lower()
   
   # Remove common prefixes/suffixes for consistent matching
   prefixes = ['mean_', 'avg_', 'normalized_']
   suffixes = ['_mean', '_avg', '_value', '_data']
   
   for prefix in prefixes:
       if base_name.startswith(prefix):
           base_name = base_name[len(prefix):]
           break
   
   for suffix in suffixes:
       if base_name.endswith(suffix):
           base_name = base_name[:-len(suffix)]
           break
   
   return base_name


def extract_variable_metadata(data_handler):
   """
   Extract metadata about available variables to aid in matching
   
   Args:
       data_handler: DataHandler instance
   
   Returns:
       dict: Variable metadata including alternative names and patterns
   """
   variable_metadata = {}
   
   # Get all available variables
   available_vars = get_available_variables(data_handler)
   
   # Common prefixes/suffixes to generate alternatives
   prefixes = ['mean_', 'avg_', 'normalized_', '']
   suffixes = ['_mean', '_avg', '_value', '_data', '']
   
   # Add metadata for each variable
   for var in available_vars:
       var_lower = var.lower()
       
       # Initialize metadata entry
       variable_metadata[var] = {
           'original_name': var,
           'alternative_names': set(),
           'keywords': set(),
           'data_type': None
       }
       
       # Try to determine data type if CSV data is available
       if data_handler.csv_data is not None and var in data_handler.csv_data.columns:
           dtype = data_handler.csv_data[var].dtype
           variable_metadata[var]['data_type'] = str(dtype)
       
       # Generate alternative names
       # Split by underscores for parts
       parts = var_lower.split('_')
       
       # Add the joined parts in different formats
       if len(parts) > 1:
           variable_metadata[var]['alternative_names'].add(' '.join(parts))
           variable_metadata[var]['alternative_names'].add('-'.join(parts))
       
       # Add alternatives with different prefixes/suffixes
       # First, get a normalized base name
       base_name = base_name_from_parts(parts)
       
       for prefix in prefixes:
           for suffix in suffixes:
               alt_name = prefix + base_name + suffix
               if alt_name and alt_name != var_lower:
                   variable_metadata[var]['alternative_names'].add(alt_name)
       
       # Add each part as a keyword
       for part in parts:
           if len(part) > 2:  # Only meaningful parts
               variable_metadata[var]['keywords'].add(part)
       
       # Special handling for common variables
       # Rainfall
       if 'rain' in var_lower or 'precip' in var_lower:
           keywords = ['rain', 'rainfall', 'precipitation', 'precip']
           variable_metadata[var]['keywords'].update(keywords)
           variable_metadata[var]['alternative_names'].update(keywords)
       
       # Temperature
       elif 'temp' in var_lower:
           keywords = ['temp', 'temperature', 'climate']
           variable_metadata[var]['keywords'].update(keywords)
           variable_metadata[var]['alternative_names'].update(keywords)
       
       # Elevation
       elif 'elev' in var_lower or 'alt' in var_lower:
           keywords = ['elevation', 'altitude', 'height']
           variable_metadata[var]['keywords'].update(keywords)
           variable_metadata[var]['alternative_names'].update(keywords)
       
       # NDVI/EVI
       elif 'ndvi' in var_lower:
           keywords = ['ndvi', 'vegetation', 'greenness']
           variable_metadata[var]['keywords'].update(keywords)
           variable_metadata[var]['alternative_names'].update(keywords)
       elif 'evi' in var_lower:
           keywords = ['evi', 'vegetation', 'enhanced']
           variable_metadata[var]['keywords'].update(keywords)
           variable_metadata[var]['alternative_names'].update(keywords)
       
       # Water-related
       elif 'ndwi' in var_lower or 'water' in var_lower:
           keywords = ['ndwi', 'water', 'moisture', 'wetness']
           variable_metadata[var]['keywords'].update(keywords)
           variable_metadata[var]['alternative_names'].update(keywords)
       
       # Convert sets to lists for JSON serialization
       variable_metadata[var]['alternative_names'] = list(variable_metadata[var]['alternative_names'])
       variable_metadata[var]['keywords'] = list(variable_metadata[var]['keywords'])
   
   return variable_metadata


def match_variables_to_dataset(requested_vars, available_vars, variable_metadata=None):
   """
   Match requested variables to available variables with fuzzy matching
   
   Args:
       requested_vars: List of variables requested by the user
       available_vars: List of available variables in the dataset
       variable_metadata: Optional metadata to improve matching
       
   Returns:
       list: Matched variable names
   """
   if not requested_vars or not available_vars:
       return []
       
   matched_variables = []
   
   # Convert available vars to lowercase for case-insensitive matching
   available_vars_lower = [var.lower() for var in available_vars]
   # Create a mapping from lowercase to original case
   case_mapping = {var.lower(): var for var in available_vars}
   
   for var in requested_vars:
       var_lower = var.lower().strip()
       if not var_lower:
           continue
       
       # Try exact matching first
       if var_lower in available_vars_lower:
           original_case = case_mapping[var_lower]
           if original_case not in matched_variables:
               matched_variables.append(original_case)
           continue
       
       # Standardize compound variable names
       standardized_var = var_lower.replace(' ', '_')
       if standardized_var in available_vars_lower:
           original_case = case_mapping[standardized_var]
           if original_case not in matched_variables:
               matched_variables.append(original_case)
           continue
       
       # Try to use variable metadata for better matching
       if variable_metadata:
           matched = False
           for available_var, metadata in variable_metadata.items():
               avail_var_lower = available_var.lower()
               
               # Check if requested var matches any alternative names
               if var_lower in [alt.lower() for alt in metadata['alternative_names']]:
                   if available_var not in matched_variables:
                       matched_variables.append(available_var)
                       matched = True
                       break
               
               # Check if keyword matches
               if any(keyword.lower() in var_lower for keyword in metadata['keywords']):
                   if available_var not in matched_variables:
                       matched_variables.append(available_var)
                       matched = True
                       break
           
           if matched:
               continue
       
       # Try partial matching as a last resort
       for available_var_lower in available_vars_lower:
           if var_lower in available_var_lower or available_var_lower in var_lower:
               original_case = case_mapping[available_var_lower]
               if original_case not in matched_variables:
                   matched_variables.append(original_case)
               break
   
   return matched_variables


def extract_variables(message, available_vars, variable_metadata=None):
   """
   Extract *validated* variable names from a message for custom analysis,
   filtering using the loaded STOP_WORDS set.

   Args:
       message: The user message
       available_vars: List of actual variable names available in the dataset.
       variable_metadata: Optional metadata to aid matching.

   Returns:
       list: List of validated variable names found in the message.
   """
   global STOP_WORDS # Access the globally defined STOP_WORDS set

   if not available_vars:
       logger.warning("Cannot extract variables: available_vars list is empty.")
       return []

   message_lower = message.lower()
   validated_variables = set() # Use a set to store unique validated vars

   # --- Patterns to find potential variable lists ---
   # Prioritize lists explicitly marked with keywords like "variables:", "using", "with" etc.
   # This pattern looks for the keyword followed by colon (optional) and then a sequence of words/commas/and
   list_patterns = [
       # E.g., "variables: rain, pop, elev", "using rainfall and population", "with variable elevation"
       r'\b(?:variables?|using|with|include|consider|only|select|custom)\s*:?\s+((?:[\w_\-\s]+(?:(?:,\s*|\s+and\s+)[\w_\-\s]+)*))'
   ]
   # More specific pattern: Match "run/analyze/process ... with/using ..."
   command_list_pattern = r'\b(?:run|analyze|process)\s+(?:analysis|data)?\s*(?:with|using|for)\s+((?:[\w_\-\s]+(?:(?:,\s*|\s+and\s+)[\w_\-\s]+)*))'

   potential_variable_strings = []
   combined_patterns = list_patterns + [command_list_pattern]

   for pattern in combined_patterns:
       matches = re.findall(pattern, message_lower)
       for group in matches:
           # Split the captured group carefully: handles commas, 'and', and spaces between words
           # Prioritize splitting by comma or ' and '
           candidates = []
           if ',' in group or ' and ' in group:
               candidates = re.split(r'\s+and\s+|\s*,\s*', group.strip())
           else:
               # If no comma or 'and', split by space (might capture multi-word vars)
               candidates = re.split(r'\s+', group.strip())

           potential_variable_strings.extend([c.strip() for c in candidates if c.strip()])


   # --- Filter and Validate ---
   logger.info(f"Potential variable strings found: {potential_variable_strings}")
   used_potential_strings = set() # Track which raw strings led to a validation

   for potential_var in potential_variable_strings:
       # Clean: lowercase, strip whitespace, remove trailing punctuation
       cleaned_var = potential_var.strip().lower().rstrip('.,;:?!')

       # **** Use the loaded STOP_WORDS set for filtering ****
       if not cleaned_var or cleaned_var in STOP_WORDS:
           continue

       # Check if this potential string (or a variation) has already resulted in a match
       # Helps prevent adding the same variable multiple times from phrases like "rainfall and mean rainfall"
       if cleaned_var in used_potential_strings:
           continue

       # Attempt to match against available variables using the robust function
       matched = match_variables_to_dataset([cleaned_var], available_vars, variable_metadata)

       if matched:
           # Add the *first* matched variable (most likely correct one)
           validated_variables.add(matched[0])
           used_potential_strings.add(cleaned_var) # Mark this raw string as used
           # Also mark variations if the matched variable is multi-word
           if '_' in matched[0] or ' ' in matched[0]:
               parts = matched[0].replace('_', ' ').split()
               for part in parts:
                   if part not in STOP_WORDS: used_potential_strings.add(part)

           logger.info(f"Validated '{cleaned_var}' -> '{matched[0]}'")

   final_list = sorted(list(validated_variables)) # Sort for consistency
   logger.info(f"Final validated variables extracted: {final_list}")
   return final_list


def clean_and_validate_variables(data_handler, raw_variables):
   """
   Clean up and validate variable names from user input
   
   Args:
       data_handler: DataHandler instance
       raw_variables: List of raw variable names from user input
       
   Returns:
       list: List of cleaned and validated variable names that exist in the dataset
   """
   if not raw_variables:
       return []
   
   # Get available variables using session data if possible
   available_vars = []
   variable_metadata = None
   
   if 'available_variables' in session:
       available_vars = session.get('available_variables', [])
       variable_metadata = session.get('variable_metadata', None)
   else:
       available_vars = get_available_variables(data_handler)
   
   # Use the improved matching function
   matched_variables = match_variables_to_dataset(raw_variables, available_vars, variable_metadata)
   
   logger.info(f"Raw variables: {raw_variables}")
   logger.info(f"Matched variables: {matched_variables}")
   
   return matched_variables


def parse_message_intent_fallback(message, session_state, data_handler=None):
   """Parse the intent from a user message (Phase 1 Refinement - using STOP_WORDS indirectly via extract_variables)"""
   message_lower = message.lower().strip()
   analysis_complete = session_state.get('analysis_complete', False)

   # --- 1. Check for Query Intent First ---
   if any(re.search(pattern, message_lower) for pattern in QUESTION_PATTERNS):
       # Specifically check if it's about the variables used in analysis
       if any(kw in message_lower for kw in ['variable', 'variables', 'parameter', 'parameters']) and \
          any(kw in message_lower for kw in ['used', 'use', 'in the analysis', 'composite score', 'calculation']):
            logger.info("Intent: Query Analysis Variables")
            return {'type': 'query_analysis_details'}
       # Add more specific query types here (e.g., methodology) if needed later
       logger.info("Intent: General Query (likely question)")
       return {'type': 'general_query'} # Let LLM handle general questions

   # --- 2. Check for Analysis Commands ---
   run_analysis_keywords = ['run', 'analyze', 'process', 'start', 'begin', 'compute', 'calculate']
   # More explicit triggers for custom analysis
   custom_analysis_keywords = ['with variable', 'using variable', 'variables:', 'custom analysis', 'only use', 'include variable', 'select variable']
   is_rerun = any(re.search(kw, message_lower) for kw in RERUN_KEYWORDS)

   # Check for Custom Analysis Request
   # Trigger if custom keywords are present OR (run keyword AND 'variable' mentioned)
   if any(re.search(kw, message_lower) for kw in custom_analysis_keywords) or \
      (any(kw in message_lower for kw in run_analysis_keywords) and 'variable' in message_lower):
       logger.info("Potential Custom Analysis Intent detected. Extracting variables...")
       available_vars = session.get('available_variables', []) # Get actual vars for validation
       if not available_vars and data_handler: # Fallback if not in session
           available_vars = get_available_variables(data_handler) # Assumes this helper exists
           session['available_variables'] = available_vars # Store for next time

       variable_metadata = session.get('variable_metadata', None)
       if not variable_metadata and data_handler: # Fallback
            variable_metadata = extract_variable_metadata(data_handler) # Assumes this helper exists
            session['variable_metadata'] = variable_metadata # Store for next time

       # Pass available vars and metadata for immediate validation during extraction
       extracted_variables = extract_variables(message, available_vars, variable_metadata) # extract_variables now uses STOP_WORDS

       # Only consider it a custom run if VALID variables were extracted
       if extracted_variables and len(extracted_variables) >= 1: # Allow even 1 for confirmation step
           logger.info(f"Intent: Run Custom Analysis with Variables: {extracted_variables}")
           return {'type': 'run_analysis_variables', 'variables': extracted_variables}
       else:
           logger.info("No valid variables extracted for custom analysis request.")
           # Fall through gracefully to check for standard run or general query

   # Check for Standard Analysis Request (requires explicit "analysis" keyword usually)
   # Ensures "run with variable X" isn't mistaken for standard run
   if any(re.search(kw + r'\s+(the\s+)?analysis', message_lower) for kw in run_analysis_keywords) or \
      message_lower in ['run analysis', 'start analysis', 'analyze data']:
       # Check if it was already identified as custom analysis (avoids misclassification)
       if not any(re.search(kw, message_lower) for kw in custom_analysis_keywords) and 'variable' not in message_lower:
           if analysis_complete and not is_rerun:
               logger.info("Intent: Query (User asked to run analysis, but it's complete. Needs confirmation/clarification)")
               return {'type': 'general_query', 'details': 'request_to_run_completed_analysis'}
           else:
               logger.info(f"Intent: Run Standard Analysis (Rerun: {is_rerun})")
               return {'type': 'run_analysis'}

   # --- 3. Check for Visualization Intent ---
   viz_keywords = ['show', 'display', 'view', 'see', 'map', 'plot', 'visualization', 'chart', 'generate', 'create', 'draw']
   if any(keyword in message_lower for keyword in viz_keywords):
       # Decision tree visualization
       if 'tree' in message_lower or 'decision' in message_lower or 'workflow' in message_lower:
           logger.info("Intent: View Plot - Decision Tree")
           return {'type': 'view_plot', 'plot_type': 'decision_tree'}
       # Box plot
       if ('box' in message_lower or 'whisker' in message_lower or 'ranking' in message_lower) and 'map' not in message_lower:
           logger.info("Intent: View Plot - Vulnerability")
           return {'type': 'view_plot', 'plot_type': 'vulnerability'}
       # Map visualizations
       if 'map' in message_lower:
           var_name = extract_variable_name(message_lower, data_handler) # Simple extraction for viz target
           if any(var_word in message_lower for var_word in ['variable', 'distribution']) and var_name:
               logger.info(f"Intent: View Map - Variable ({var_name})")
               return {'type': 'view_map', 'map_type': 'variable', 'variable_name': var_name}
           if ('normalize' in message_lower or 'normalized' in message_lower) and var_name:
               logger.info(f"Intent: View Map - Normalized ({var_name})")
               return {'type': 'view_map', 'map_type': 'normalized', 'variable_name': var_name}
           if any(word in message_lower for word in ['composite', 'risk', 'score']):
                logger.info("Intent: View Map - Composite")
                return {'type': 'view_map', 'map_type': 'composite'}
           if ('vulnerability' in message_lower or 'vulnerable' in message_lower):
                logger.info("Intent: View Map - Vulnerability")
                return {'type': 'view_map', 'map_type': 'vulnerability'}
           if any(word in message_lower for word in ['urban', 'extent', 'threshold']):
               threshold_match = re.search(r'(\d+)\s*%?', message_lower) # Allow % sign or not
               threshold = 30 if not threshold_match else int(threshold_match.group(1))
               logger.info(f"Intent: View Map - Urban Extent (Threshold: {threshold})")
               return {'type': 'view_map', 'map_type': 'urban_extent', 'threshold': threshold}
           # Default map type if variable mentioned
           if var_name:
                logger.info(f"Intent: View Map - Variable (Default map for {var_name})")
                return {'type': 'view_map', 'map_type': 'variable', 'variable_name': var_name}
           else: # Default map if no variable found
                logger.info("Intent: View Map - Composite (Default map)")
                return {'type': 'view_map', 'map_type': 'composite'}
       # Fallback for plot/chart/visualization if type unclear but variable mentioned
       var_name = extract_variable_name(message_lower, data_handler)
       if var_name:
           logger.info(f"Intent: View Map - Variable (Default viz for {var_name})")
           return {'type': 'view_map', 'map_type': 'variable', 'variable_name': var_name} # Default to map

   # --- 4. Check for Report Generation ---
   if any(word in message_lower for word in ['report', 'generate', 'download', 'pdf', 'docx', 'html', 'document']):
       format_type = 'pdf' # Default
       if 'html' in message_lower: format_type = 'html'
       elif 'word' in message_lower or 'docx' in message_lower: format_type = 'docx'
       logger.info(f"Intent: Generate Report (Format: {format_type})")
       return {'type': 'generate_report', 'format': format_type}

   # --- 5. Check for Language Change ---
   if any(word in message_lower for word in ['language', 'speak', 'talk', 'translate']):
       language = 'en' # Default
       if 'french' in message_lower or 'français' in message_lower: language = 'fr'
       elif 'hausa' in message_lower: language = 'ha'
       elif 'yoruba' in message_lower: language = 'yo'
       elif 'igbo' in message_lower: language = 'ig'
       elif 'arabic' in message_lower: language = 'ar'
       # Add more languages if needed
       logger.info(f"Intent: Change Language (To: {language})")
       return {'type': 'change_language', 'language': language}

   # --- Default: General Query ---
   logger.info("Intent: General Query (Fallback)")
   return {'type': 'general_query'}


def extract_variable_name(message_lower, data_handler=None):
   """
   Extract variable name from message
   
   Args:
       message_lower: Lowercase user message
       data_handler: Optional DataHandler instance for variable validation
       
   Returns:
       str: Extracted and validated variable name, or None if not found
   """
   # Common variable patterns
   patterns = [
       r'variable[:\s]+(\w+)',
       r'for\s+(?:the\s+)?(\w+)',
       r'of\s+(?:the\s+)?(\w+)',
       r'showing\s+(?:the\s+)?(\w+)',
       r'see\s+(?:the\s+)?(\w+)',
       r'display\s+(?:the\s+)?(\w+)',
       r'view\s+(?:the\s+)?(\w+)',
       r'about\s+(?:the\s+)?(\w+)'
   ]
   
   # Try each pattern
   for pattern in patterns:
       match = re.search(pattern, message_lower)
       if match:
           var_name = match.group(1).strip()
           # Filter out common words
           stop_words = ['the', 'map', 'plot', 'variable', 'visualization', 'chart', 'distribution']
           if var_name not in stop_words:
               # Validate against dataset if data_handler provided
               if data_handler:
                   validated_vars = clean_and_validate_variables(data_handler, [var_name])
                   if validated_vars:
                       return validated_vars[0]
                   else:
                       continue  # Try next pattern if this variable doesn't validate
               else:
                   return var_name
   
   # Check for common variable names
   common_vars = [
       'rainfall', 'temperature', 'elevation', 'population', 'distance', 
       'housing', 'temp_mean', 'mean_rainfall', 'pfpr', 'ndvi', 'evi',
       'flood', 'housing_quality', 'distance_to_water', 'mean_ndvi', 'mean_evi',
       'rh_mean', 'soil_wetness', 'mean_soil_wetness', 'water'
   ]
   
   for var in common_vars:
       if var in message_lower:
           # Validate if data_handler provided
           if data_handler:
               validated_vars = clean_and_validate_variables(data_handler, [var])
               if validated_vars:
                   return validated_vars[0]
           else:
               return var
   
   # If data_handler is provided but no variable found yet, try compound variables
   if data_handler:
       # Check for compound forms like "mean evi"
       compound_patterns = [
           (r'mean\s+evi', 'mean_evi'),
           (r'mean\s+ndvi', 'mean_ndvi'),
           (r'distance\s+to\s+water', 'distance_to_water'),
           (r'soil\s+wetness', 'mean_soil_wetness')
       ]
       for pattern, standard_name in compound_patterns:
           if re.search(pattern, message_lower):
               validated_vars = clean_and_validate_variables(data_handler, [standard_name])
               if validated_vars:
                   return validated_vars[0]
   
   return None


def get_llm_nlu_response(user_message, session_state, available_variables, last_visualization=None):
    """
    Uses the LLM to perform Natural Language Understanding (NLU)
    to identify intent and extract entities.

    Args:
        user_message (str): The raw message from the user.
        session_state (dict): Current state (analysis_complete, etc.).
        available_variables (list): List of valid variable names for entity extraction.
        last_visualization (dict): Information about the last visualization shown (for context).

    Returns:
        dict: A dictionary containing 'intent' and 'entities' (or None on error).
              Example: {'intent': 'run_custom_analysis', 'entities': {'variable_names': ['rainfall', 'pop_density']}}
    """
    api_key = current_app.config.get('OPENAI_API_KEY')
    if not api_key:
        logger.error("OpenAI API Key not found for NLU.")
        return None # Cannot perform LLM NLU

    client = openai.OpenAI(api_key=api_key)

    # Define possible intents and entities for the LLM
    # Enhanced for Phase 3 with explanation intents and confirmation handling
    intents_description = """
    Possible intents:
    - run_standard_analysis: User wants to run the default analysis.
    - run_custom_analysis: User wants to run analysis with specific variables.
    - query_analysis_details: User asks about parameters/variables used in the last analysis.
    - request_visualization: User asks to see a map, plot, or chart.
    - generate_report: User asks to generate a PDF, HTML, or DOCX report.
    - change_language: User wants to change the interaction language.
    - greet: User says hello or greets the assistant.
    - goodbye: User says goodbye or indicates ending the session.
    - explain_methodology: User wants an explanation of a particular methodology (cleaning, normalization, etc.)
    - explain_variable: User wants an explanation of a specific variable's relationship with malaria.
    - explain_variable_category: User wants an explanation of a category of variables (environmental, demographic, etc.)
    - viz_followup_question: User is asking a follow-up about the most recently shown visualization.
    - confirm_custom_analysis: User is confirming a previously proposed custom analysis.
    - cancel_custom_analysis: User is cancelling a previously proposed custom analysis.
    - clarification_needed: The user's request is too ambiguous to proceed.
    - request_elaboration: User asks for more details or elaboration on a previously discussed topic.
    - general_query: User asks a general question not covered above.
    """
    
    entities_description = f"""
    Extractable entities:
    - variable_names (list): List of specific variable names mentioned for custom analysis or visualization. Validate against this list: {', '.join(available_variables[:20])}{'...' if len(available_variables) > 20 else ''}.
    - variable_name (string): A specific variable the user is asking about for explanation purposes.
    - variable_category (string): Category of variables ('environmental', 'demographic', 'epidemiological').
    - visualization_type (string): General type like 'map', 'plot', 'chart', 'tree'.
    - map_type (string): Specific map type ('variable', 'normalized', 'composite', 'vulnerability', 'urban_extent').
    - plot_type (string): Specific plot type ('vulnerability', 'decision_tree').
    - variable_for_viz (string): The specific variable requested for a map/plot. Use a name from the available list if possible.
    - threshold_value (float): Numerical threshold for urban extent (default 30). E.g., for "0.5%", extract 0.5. For "50%", extract 50.
    - report_format (string): 'pdf', 'html', or 'docx'.
    - language_code (string): 'en', 'ha', 'yo', 'ig', 'fr', 'ar', etc.
    - methodology_type (string): The methodology the user is asking about ('missing_values', 'normalization', 'composite_scores', 'vulnerability_ranking', 'urban_extent').
    - topic (string): The specific topic the user is asking for more information about (e.g., 'variables', 'methodology', 'urban_extent').
    """
    
    # Add context about current state
    state_summary = f"Current state: Analysis previously run = {session_state.get('analysis_complete', False)}. Files loaded = {session_state.get('csv_loaded', False) and session_state.get('shapefile_loaded', False)}."
    
    # Add recent visualization context if available
    viz_context = ""
    if last_visualization:
        viz_type = last_visualization.get('type', '')
        variable = last_visualization.get('variable', '')
        threshold = last_visualization.get('threshold', '')
        
        viz_context = f"Most recent visualization shown was a {viz_type}"
        if variable:
            viz_context += f" of the variable '{variable}'"
        if threshold and viz_type == 'urban_extent_map':
            viz_context += f" with threshold {threshold}%"
        viz_context += "."
    
    # Get dialogue context for better continuity
    dialogue_context = session.get('dialogue_context', {})
    last_topic = dialogue_context.get('last_topic', '')
    last_intent = dialogue_context.get('last_intent', '')
    
    dialogue_context_str = ""
    if last_topic or last_intent:
        dialogue_context_str = f"Previous conversation was about: {last_topic or last_intent}."

    # Pending action context (for confirmation handling)
    pending_context = ""
    if session.get('pending_action') == 'confirm_custom_analysis' and session.get('pending_variables'):
        pending_vars = session.get('pending_variables', [])
        pending_context = f"IMPORTANT: The user was asked to confirm a custom analysis with these variables: {', '.join(pending_vars)}. Check if they are confirming or cancelling."

    system_prompt = f"""
    You are an expert NLU system for a Malaria Risk Analysis tool.
    Analyze the user message considering the current application state and identify the primary intent and any relevant entities.
    {state_summary}
    {viz_context}
    {dialogue_context_str}
    {pending_context}
    
    IMPORTANT: Pay special attention to follow-up questions and requests for elaboration.
    If the user asks for more details, explanations, or elaboration about a previously mentioned topic, 
    classify this as 'request_elaboration' intent with the 'topic' entity set to what they're asking for more details about.
    Examples of elaboration requests:
    - "Tell me more about X"
    - "Elaborate on X"
    - "Why were these variables chosen?"
    - "What makes these important?"
    - "Can you explain more about..."
    - "I want to understand better..."
    
    {intents_description}
    {entities_description}
    Respond ONLY with a JSON object containing the 'intent' (string) and 'entities' (object).
    If multiple variables are mentioned for custom analysis, include them all in the 'variable_names' list within entities.
    If a specific variable is mentioned for a visualization, put it in 'variable_for_viz'.
    If no relevant entities are found, provide an empty entities object {{}}.
    Prioritize specific intents over general_query. If the request is very ambiguous, use intent 'clarification_needed'.
    If the user asks *what* variables were used, the intent is 'query_analysis_details'.
    If the user asks *to use* specific variables, the intent is 'run_custom_analysis'.
    If the user's message is a simple 'yes', 'confirm', 'ok', etc. following a custom analysis proposal, the intent is 'confirm_custom_analysis'.
    If the user's message is a simple 'no', 'cancel', etc. following a custom analysis proposal, the intent is 'cancel_custom_analysis'.
    If the user is asking about 'how' something works (methodology), use 'explain_methodology'.
    If the user is asking about a specific variable and its relationship with malaria, use 'explain_variable'.
    If the user is asking for more information after you've already provided an answer, use 'request_elaboration'.
    """

    messages = [
        {"role": "system", "content": system_prompt},
        {"role": "user", "content": user_message}
    ]

    try:
        response = client.chat.completions.create(
            model="gpt-4o", # Or gpt-3.5-turbo for faster/cheaper NLU
            messages=messages,
            temperature=0.1, # Low temperature for factual NLU
            max_tokens=500, # Increased for potentially longer explanations
            response_format={ "type": "json_object" } # Request JSON output directly (supported by newer models)
        )
        content = response.choices[0].message.content
        logger.debug(f"LLM NLU raw response: {content}")
        nlu_result = json.loads(content)

        # Basic validation of the structure
        if 'intent' in nlu_result and 'entities' in nlu_result:
             # Validate extracted variable names against available_vars
             if 'variable_names' in nlu_result['entities']:
                  validated_vars = match_variables_to_dataset(
                      nlu_result['entities']['variable_names'],
                      available_variables,
                      session.get('variable_metadata')
                  )
                  nlu_result['entities']['variable_names'] = validated_vars
                  if not validated_vars: # Remove if validation yields nothing
                       del nlu_result['entities']['variable_names']

             if 'variable_for_viz' in nlu_result['entities'] and nlu_result['entities']['variable_for_viz']:
                  matched_viz_var = match_variables_to_dataset(
                      [nlu_result['entities']['variable_for_viz']],
                       available_variables,
                       session.get('variable_metadata')
                       )
                  # Update with validated name or remove if invalid
                  nlu_result['entities']['variable_for_viz'] = matched_viz_var[0] if matched_viz_var else None
                  if not nlu_result['entities']['variable_for_viz']:
                      del nlu_result['entities']['variable_for_viz']

             logger.info(f"LLM NLU Result: Intent='{nlu_result['intent']}', Entities={nlu_result['entities']}")
             return nlu_result
        else:
             logger.error(f"LLM NLU response missing required keys ('intent', 'entities'): {content}")
             return None

    except json.JSONDecodeError as json_err:
        logger.error(f"Failed to decode LLM NLU JSON response: {json_err}. Response: {content if 'content' in locals() else 'No response'}")
        return None
    except Exception as e:
        logger.error(f"Error calling OpenAI for NLU: {e}", exc_info=True)
        return None


def generate_analysis_success_message(result, is_custom=False):
   """Generates a standard success message after analysis."""
   custom_text = "with the variables you specified" if is_custom else "using default parameters"
   vars_used = result.get('variables_used', [])
   top_wards = result.get('vulnerable_wards', [])[:5] # Get top 5

   vars_text = ', '.join(f"'{v}'" for v in vars_used) if vars_used else 'a default set'
   wards_text = ', '.join(top_wards) if top_wards else 'N/A'

   message = f"""
       <p><strong>{'Custom analysis' if is_custom else 'Analysis'} completed successfully!</strong></p>
       <p>I've analyzed your data {custom_text}. Key results:</p>
       <ul>
           <li><strong>Variables Used:</strong> {vars_text}</li>
           <li><strong>Top 5 Vulnerable Wards:</strong> {wards_text}</li>
       </ul>
       <p>You can now ask me to show visualizations like:</p>
       <ul>
           <li>"Show map for <i>[variable name]</i>" (e.g., population)</li>
           <li>"Show normalized map for <i>[variable name]</i>"</li>
           <li>"Show composite map"</li>
           <li>"Show vulnerability plot" (Ranking)</li>
           <li>"Show vulnerability map"</li>
           <li>"Show urban extent map at 50%"</li>
           <li>"Show decision tree" (Workflow)</li>
       </ul>
       <p>You can also <a href="#" onclick="document.getElementById('download-report-btn').click(); return false;">generate a report</a> summarising these findings.</p>
       <p>What would you like to see first?</p>
   """
   return message


def get_visualization_result(viz_request, data_handler):
   """
   Get visualization result without going through the API endpoint
   Duplicates functionality from get_visualization but can be called directly
   """
   viz_type = viz_request.get('type', '')
   variable = viz_request.get('variable', None)
   threshold = viz_request.get('threshold', 30)
   
   # Check if analysis is complete, except for variable maps which can be viewed anytime
   if not session.get('analysis_complete', False) and viz_type not in ['variable_map']:
       return {
           'status': 'error',
           'message': 'Analysis has not been run yet. Please run the analysis first.',
           'ai_response': "I need to run the analysis before I can show you visualizations. Would you like me to run the analysis now?"
       }
   
   # Handle different visualization types
   try:
       if viz_type == 'variable_map':
           result = viz.create_variable_map(data_handler, variable)
       elif viz_type == 'normalized_map':
           result = viz.create_normalized_map(data_handler, variable)
       elif viz_type == 'composite_map':
           result = viz.create_composite_map(data_handler)
       elif viz_type == 'vulnerability_plot':
           if hasattr(data_handler, 'composite_scores') and data_handler.composite_scores is not None:
               box_plot_result = viz.box_plot_function(data_handler.composite_scores['scores'])
               if box_plot_result['status'] == 'success':
                   data_handler.boxwhisker_plot = box_plot_result
                   plot_fig = box_plot_result['plots'][0]
                   html_path = viz.create_plotly_html(plot_fig, "vulnerability_plot.html")
                   result = {
                       'status': 'success',
                       'message': 'Successfully generated vulnerability plot',
                       'image_path': html_path,
                       'current_page': int(1),
                       'total_pages': int(box_plot_result['total_pages']),
                       'viz_type': 'vulnerability_plot',
                       'ai_response': "Here's the vulnerability ranking box and whisker plot showing wards from most vulnerable (top) to least vulnerable (bottom). Each horizontal bar represents a ward, with the box showing the range of vulnerability scores across different models. This visualization helps identify priority areas for intervention."
                   }
               else:
                   result = box_plot_result
           else:
               result = {
                   'status': 'error',
                   'message': 'Composite scores not available for vulnerability plot',
                   'ai_response': "I couldn't generate the vulnerability box plot because the composite scores haven't been calculated yet. Let's run the analysis first."
               }
       elif viz_type == 'vulnerability_map':
           result = viz.create_vulnerability_map(data_handler)
       elif viz_type == 'urban_extent_map':
           result = viz.create_urban_extent_map(data_handler, threshold)
       elif viz_type == 'decision_tree':
           result = viz.create_decision_tree_plot(data_handler)
       else:
           result = {
               'status': 'error',
               'message': f'Unknown visualization type: {viz_type}',
               'ai_response': f"I'm not sure what visualization you're looking for. You can ask for variable maps, normalized maps, composite maps, vulnerability plots, vulnerability maps, or urban extent maps."
           }
       
       # Ensure all values in the result dictionary are JSON serializable
       result = convert_to_json_serializable(result)
       
       return result
   except Exception as e:
       logger.error(f"Error generating visualization: {str(e)}")
       import traceback
       logger.error(traceback.format_exc())
       return {
           'status': 'error',
           'message': f'Error generating visualization: {str(e)}',
           'ai_response': f"I encountered an error while creating the visualization: {str(e)}. Please try again with a different request."
       }


def generate_ai_response(user_message, session_state, nlu_result=None, analysis_result=None, context_for_llm=None):
   """
   Generate an AI response using OpenAI with enhanced context for Phase 3.
   
   Args:
       user_message: The message from the user
       session_state: Current state dict (csv_loaded, etc.)
       nlu_result: Optional NLU classification result
       analysis_result: Optional analysis results
       context_for_llm: Optional additional context (e.g., knowledge base content)
       
   Returns:
       str: The AI response or None on error
   """
   try:
       api_key = current_app.config.get('OPENAI_API_KEY')
       if not api_key:
           return None # Fallback will be used
           
       system_message = get_system_message(session_state, analysis_result, context_for_llm)
       conversation_history = session.get('conversation_history', [])
       
       conversation_history.append({"role": "user", "content": user_message})
       
       # Keep only the last N messages for context to OpenAI
       # And limit what's stored back in the session
       MAX_HISTORY_FOR_OPENAI = 10 # Last 5 pairs
       MAX_HISTORY_IN_SESSION = 10 # Store even less in session to keep cookie small
       
       messages_for_openai = [
           {"role": "system", "content": system_message},
       ] + conversation_history[-MAX_HISTORY_FOR_OPENAI:]
       
       client = openai.OpenAI(api_key=api_key)
       
       response = client.chat.completions.create(
           model="gpt-4o", # Consider "gpt-3.5-turbo" if cost/speed is an issue and quality is acceptable
           messages=messages_for_openai,
           temperature=0.7,
           max_tokens=800
       )
       ai_message = response.choices[0].message.content
       
       conversation_history.append({"role": "assistant", "content": ai_message})
       # Store a limited version of the history back into the session
       session['conversation_history'] = conversation_history[-MAX_HISTORY_IN_SESSION:] 
       
       return ai_message
   
   except Exception as e:
       logger.error(f"Error generating AI response: {str(e)}", exc_info=True)
       return None # Fallback will be used


def get_system_message(session_state, analysis_result=None, additional_context=None):
   """
   Generate a system message with CURRENT state for the LLM (Phase 3).
   Includes dynamic state information and optional additional context.
   
   Args:
       session_state: Current state dict
       analysis_result: Optional analysis results
       additional_context: Optional additional context (e.g., knowledge base entries)
       
   Returns:
       str: System message for the LLM
   """
   # Base instructions
   base_message = """
   You are an AI assistant for the Malaria Reprioritization Tool (MRPT).
   Your role is to help users analyze malaria risk factors and prioritize areas for intervention.
   Be warm, friendly, and conversational while maintaining scientific accuracy.
   Guide users through the analysis process, explain results clearly, and help them interpret visualizations.
   Focus on being helpful, responsive, and informative about malaria epidemiology and risk assessment.
   """

   # --- Add Dynamic State Information ---
   state_summary = "\n\nCurrent Session Status:\n"
   csv_loaded = session_state.get('csv_loaded', False)
   shp_loaded = session_state.get('shapefile_loaded', False)
   analysis_complete = session_state.get('analysis_complete', False)

   if csv_loaded and shp_loaded:
       state_summary += "- Data Files: Both CSV/Excel and Shapefile are LOADED.\n"
   elif csv_loaded:
       state_summary += "- Data Files: CSV/Excel LOADED, Shapefile MISSING.\n"
   elif shp_loaded:
       state_summary += "- Data Files: Shapefile LOADED, CSV/Excel MISSING.\n"
   else:
       state_summary += "- Data Files: No data files loaded yet.\n"

   if analysis_complete:
       state_summary += "- Analysis: COMPLETE.\n"
       if analysis_result:
            vars_used = analysis_result.get('variables_used', [])
            top_wards = analysis_result.get('vulnerable_wards', [])[:5] # Get top 5
            if vars_used:
                state_summary += f"  - Variables used in last analysis: {', '.join(vars_used)}\n"
            if top_wards:
                state_summary += f"  - Top vulnerable wards: {', '.join(top_wards)}\n"
   else:
        state_summary += "- Analysis: NOT YET RUN.\n"

   state_summary += f"- Current Language: {session_state.get('current_language', 'en')}\n"
   
   # Include additional context if provided (e.g., knowledge base content)
   context_section = ""
   if additional_context:
       context_section = f"\n\nAdditional Context:\n{additional_context}\n"

   return base_message + state_summary + context_section


def get_fallback_response(message, session_state):
   """Generate a fallback response when OpenAI is not available"""
   message_lower = message.lower()
   
   if 'hello' in message_lower or 'hi' in message_lower:
       return "Hello! I'm your Malaria Risk Analysis assistant. How can I help you today?"
   elif 'thank' in message_lower:
       return "You're welcome! Is there anything else you'd like to know about your data?"
   elif not session_state.get('csv_loaded', False) or not session_state.get('shapefile_loaded', False):
       return "To get started, please upload both your CSV data and shapefile. I'll guide you through the analysis process once they're loaded."
   elif not session_state.get('analysis_complete', False):
       return "Would you like me to run the analysis on your data? Just type 'Run the analysis' to begin!"
   else:
       return "Your analysis is complete. You can ask me to show you various visualizations like maps, plots, or generate a report. What would you like to see?"


def convert_to_json_serializable(obj):
    """
    Recursively convert objects to JSON serializable types.
    Specifically handles NumPy types which are not JSON serializable by default.
    Updated for NumPy 2.0 compatibility.
    """
    if isinstance(obj, dict):
        return {k: convert_to_json_serializable(v) for k, v in obj.items()}
    elif isinstance(obj, list):
        return [convert_to_json_serializable(item) for item in obj]
    
    # Integer types
    elif isinstance(obj, np.integer):
        return int(obj)
    # Floating point types
    elif isinstance(obj, np.floating):
        return float(obj)
    # Boolean types
    elif isinstance(obj, np.bool_):
        return bool(obj)
    # NumPy arrays
    elif isinstance(obj, np.ndarray):
        return convert_to_json_serializable(obj.tolist())
        
    # Other Python types
    elif obj is None or isinstance(obj, (str, int, float, bool)):
        return obj
    # For other types, try string conversion
    else:
        try:
            return str(obj)
        except:
            return f"Unserializable object of type: {type(obj).__name__}"
       
def extractThreshold(message):
    """
    Extract urban extent threshold from a message with improved pattern matching
    
    Args:
        message: The message to extract threshold from
        
    Returns:
        float: Extracted threshold percentage (default: 30.0)
    """
    # Default threshold
    default_threshold = 30.0 # Use float for consistency
    
    # Convert message to lowercase for case-insensitive matching
    message_lower = message.lower()
    
    # Pattern 1: Find "X%" or "X.Y%" pattern
    # Regex updated to capture floating point numbers
    threshold_match = re.search(r'(\d+(?:\.\d+)?)\s*%', message_lower)
    if threshold_match and threshold_match[1]:
        try:
            threshold_value = float(threshold_match[1])
            # Validate range (0-100%)
            return max(0.0, min(100.0, threshold_value))
        except ValueError:
            pass # Will try next pattern or return default
    
    # Pattern 2: Look for "threshold of X" or "at X" or "X threshold" or "X percent"
    # Regex updated to capture floating point numbers
    pattern2 = re.search(r'(?:threshold\s+(?:of\s+)?|at\s+|set\s+to\s+|level\s+of\s+)?(\d+(?:\.\d+)?)(?:\s*(?:threshold|percent|pct|%|urban))?', message_lower)
    if pattern2 and pattern2[1]:
        try:
            threshold_value = float(pattern2[1])
            # Validate range (0-100%)
            return max(0.0, min(100.0, threshold_value))
        except ValueError:
            pass # Will try next pattern or return default

    # Pattern 3: Look for written numbers (more complex, can be added if needed)
    
    # Return default if no patterns match
    return default_threshold
