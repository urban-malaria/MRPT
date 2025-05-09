import numpy as np
import json
import shutil
import os
import uuid
import logging
import pandas as pd
import re
from datetime import datetime
from flask import Blueprint, render_template, request, jsonify, current_app, session, send_from_directory
from werkzeug.utils import secure_filename
import openai

from .models.data_handler import DataHandler
import app.models.visualization as viz
import app.models.report_generator as report_gen

from flask import Blueprint, render_template, request, jsonify, current_app, session, send_from_directory # ensure send_from_directory is imported

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Create blueprint
main_bp = Blueprint('main', __name__)

# Allowed file extensions
ALLOWED_EXTENSIONS_CSV = {'csv', 'xlsx', 'xls'}
ALLOWED_EXTENSIONS_SHP = {'zip'}

def allowed_file(filename, allowed_extensions):
    return '.' in filename and filename.rsplit('.', 1)[1].lower() in allowed_extensions

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
    
    return render_template('index.html')

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
            
            return jsonify({
                'status': 'success', 
                'message': f'CSV file {filename} uploaded successfully',
                'rows': result.get('rows', 0),
                'columns': result.get('columns', 0),
                'missing_values': result.get('missing_values', 0),
                'available_variables': available_variables
            })
        else:
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
            return jsonify({'status': 'error', 'message': result.get('message', 'Failed to process shapefile')}), 400
    
    return jsonify({'status': 'error', 'message': 'Invalid file type'}), 400

@main_bp.route('/run_analysis', methods=['POST'])
def run_analysis():
    """Run the complete analysis workflow"""
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
        logger.info("Starting full analysis pipeline...")
        if selected_variables:
            logger.info(f"Using custom variables: {selected_variables}")
            
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
            
            # Return success response
            return jsonify({
                'status': 'success',
                'message': result.get('message', 'Analysis completed successfully'),
                'steps': result.get('steps', {}),
                'variables_used': result.get('variables_used', []),
                'vulnerable_wards': result.get('vulnerable_wards', [])[:5]
            })
        else:
            return jsonify({
                'status': 'error',
                'message': result.get('message', 'Error running analysis')
            }), 400
    
    except Exception as e:
        logger.error(f"Error running analysis: {str(e)}")
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
        # Extract and store available variables & metadata (using helper functions if refactored)
        available_variables = get_available_variables(temp_data_handler) # Assuming this helper exists
        session['available_variables'] = available_variables
        session['variable_metadata'] = extract_variable_metadata(temp_data_handler) # Assuming this helper exists
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
        # Clean up potentially partially copied files? Maybe not necessary.
        return jsonify({'status': 'error', 'message': f'An internal error occurred while loading sample data: {str(e)}'}), 500

# Make sure helper functions get_available_variables and extract_variable_metadata are accessible
# If they are not defined globally, you might need to instantiate DataHandler and call its methods
# or refactor them out. The code above assumes they are available.


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

@main_bp.route('/get_visualization', methods=['POST'])
def get_visualization():
    """Handle visualization requests directly"""
    data = request.json
    viz_type = data.get('type', '')
    variable = data.get('variable', None)
    threshold = data.get('threshold', 30)
    
    if not viz_type:
        return jsonify({'status': 'error', 'message': 'No visualization type provided'}), 400
    
    # Get data handler from session
    data_handler = get_data_handler()
    
    # Check if analysis is complete, except for variable maps which can be viewed anytime
    if not session.get('analysis_complete', False) and viz_type not in ['variable_map']:
        return jsonify({
            'status': 'error',
            'message': 'Analysis has not been run yet. Please run the analysis first.',
            'ai_response': "I need to run the analysis before I can show you visualizations. Would you like me to run the analysis now?"
        })
    
    # Handle different visualization types properly
    try:
        if viz_type == 'variable_map':
            # Call the variable map function with the specified variable
            # Note: Allow viewing any variable in the CSV data, not just those in the analysis
            result = viz.create_variable_map(data_handler, variable)
        elif viz_type == 'normalized_map':
            # Call the normalized map function with the specified variable
            result = viz.create_normalized_map(data_handler, variable)
        elif viz_type == 'composite_map':
            # Call the composite map function
            result = viz.create_composite_map(data_handler)
        elif viz_type == 'vulnerability_plot':
            # Call the box plot function to generate the vulnerability plot
            if hasattr(data_handler, 'composite_scores') and data_handler.composite_scores is not None:
                # Convert any NumPy data types to Python native types before creating the box plot
                box_plot_result = viz.box_plot_function(data_handler.composite_scores['scores'])
                if box_plot_result['status'] == 'success':
                    # Store the box plot data for pagination
                    data_handler.boxwhisker_plot = box_plot_result
                    # Get the first plot
                    plot_fig = box_plot_result['plots'][0]
                    # Save as HTML
                    html_path = viz.create_plotly_html(plot_fig, "vulnerability_plot.html")
                    # Create result dict - convert any NumPy types to native Python types for JSON serialization
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
            # Call the vulnerability map function
            result = viz.create_vulnerability_map(data_handler)
        elif viz_type == 'urban_extent_map':
            # Call the urban extent map function with the specified threshold
            result = viz.create_urban_extent_map(data_handler, threshold)
        elif viz_type == 'decision_tree':
            # Call the decision tree function
            result = viz.create_decision_tree_plot(data_handler)
        else:
            result = {
                'status': 'error',
                'message': f'Unknown visualization type: {viz_type}',
                'ai_response': f"I'm not sure what visualization you're looking for. You can ask for variable maps, normalized maps, composite maps, vulnerability plots, vulnerability maps, or urban extent maps."
            }
        
        # Ensure the result is serializable BEFORE jsonify
        serializable_result = convert_to_json_serializable(result) # CALL THE HELPER HERE
        return jsonify(serializable_result) # Pass the cleaned dict to jsonify

    except Exception as e:
        logger.error(f"Error generating visualization: {str(e)}", exc_info=True) # Use exc_info=True for full traceback in log
        # Return an error response, ensuring it's also serializable
        error_result = {
            'status': 'error',
            'message': f'Error generating visualization: {str(e)}',
            'ai_response': f"I encountered an error while creating the visualization. Please check the logs or try again."
        }
        return jsonify(convert_to_json_serializable(error_result)), 500 # Return 500 for server error
    

def convert_to_json_serializable(obj):
    """
    Recursively convert objects to JSON serializable types.
    Specifically handles NumPy types which are not JSON serializable by default.
    """
    if isinstance(obj, dict):
        return {k: convert_to_json_serializable(v) for k, v in obj.items()}
    elif isinstance(obj, list):
        return [convert_to_json_serializable(item) for item in obj]
    # ------ ADDED/MODIFIED HANDLING for NumPy types ------
    elif isinstance(obj, (np.int_, np.intc, np.intp, np.int8, np.int16, np.int32,
                         np.int64, np.uint8, np.uint16, np.uint32, np.uint64)):
        return int(obj) # Convert numpy int to python int
    elif isinstance(obj, (np.float_, np.float16, np.float32, np.float64)):
         # Note: np.float_ was removed in NumPy 2.0, kept for broader compatibility if needed
         # but the error indicates you are using NumPy >= 2.0. So let's remove np.float_
         # Use np.floating to catch all float types robustly
         return float(obj) # Convert numpy float to python float
    elif isinstance(obj, (np.complex_, np.complex64, np.complex128)):
        return {'real': obj.real, 'imag': obj.imag} # Example: represent complex as dict
    elif isinstance(obj, (np.bool_)):
         return bool(obj) # Convert numpy bool to python bool
    elif isinstance(obj, (np.void)):
         return None # Handle numpy void type, e.g., by returning None or converting appropriately
    # ------ END OF ADDED/MODIFIED HANDLING ------
    elif isinstance(obj, np.ndarray):
        return convert_to_json_serializable(obj.tolist()) # Existing good handling for arrays
    elif pd.isna(obj): # Handle Pandas NA types
        return None
    elif isinstance(obj, (pd.Timestamp)): # Handle Pandas Timestamp
        return obj.isoformat()
    elif obj is None or isinstance(obj, (str, int, float, bool)):
         # Check basic types last
        return obj
    else:
        # For other types, try regular conversion or convert to string if that fails
        try:
            # Check if it has a to_dict method (like some complex objects might)
            if hasattr(obj, 'to_dict') and callable(getattr(obj, 'to_dict')):
                 return convert_to_json_serializable(obj.to_dict())
            # Last resort: convert to string
            return str(obj)
        except (TypeError, OverflowError):
            logger.warning(f"Could not serialize object of type {type(obj)}, converting to string.")
            return str(obj)

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
        
        # Ensure all values in the result dictionary are JSON serializable
        result = convert_to_json_serializable(result)
        
        return jsonify(result)
    else:
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
        return jsonify({
            'status': 'error',
            'message': box_plot_result.get('message', 'Error updating box plot pagination')
        }), 400

@main_bp.route('/send_message', methods=['POST'])
def send_message():
    """Handle chat messages and AI responses"""
    data = request.json
    user_message = data.get('message', '')
    
    if not user_message:
        return jsonify({'status': 'error', 'message': 'No message provided'}), 400
    
    # Get data handler
    data_handler = get_data_handler()
    
    # Get current session state
    session_state = {
        'csv_loaded': session.get('csv_loaded', False),
        'shapefile_loaded': session.get('shapefile_loaded', False),
        'data_loaded': session.get('data_loaded', False),
        'analysis_complete': session.get('analysis_complete', False),
        'current_language': session.get('current_language', 'en')
    }
    
    # Parse intent from message - SIMPLIFIED to handle only variables for custom analysis
    intent = parse_message_intent(user_message, session_state, data_handler)
    
    # Process message based on intent
    if intent['type'] == 'run_analysis' and all([session.get('csv_loaded', False), session.get('shapefile_loaded', False)]):
        # Run the analysis logic
        try:
            # Run the full analysis pipeline
            logger.info("Starting full analysis pipeline...")
            result = data_handler.run_full_analysis()
            
            if result['status'] == 'success':
                # Store data handler in session config
                session_id = session.get('session_id', 'default')
                current_app.config.setdefault('SESSION_DATA', {})
                current_app.config['SESSION_DATA'][session_id] = {
                    'data_handler': data_handler,
                    'timestamp': datetime.now()
                }
                
                # Update session
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
                
                # Generate AI response with results
                ai_response = f"""
                <p><strong>Analysis completed successfully!</strong></p>
                <p>I've analyzed your data and here are the results:</p>
                <ul>
                    <li><strong>Variables used:</strong> {', '.join(result.get('variables_used', []))}</li>
                    <li><strong>Top 5 vulnerable wards:</strong> {', '.join(result.get('vulnerable_wards', [])[:5])}</li>
                </ul>
                <p>You can now ask me to show you visualizations like:</p>
                <ul>
                    <li>Variable distribution maps</li>
                    <li>Normalized maps for specific variables</li>
                    <li>Composite risk maps</li>
                    <li>Vulnerability ranking plot (box and whisker plot)</li>
                    <li>Vulnerability map</li>
                    <li>Urban extent maps at different thresholds</li>
                    <li>Decision tree visualization</li>
                </ul>
                <p>What would you like to see first?</p>
                """
                
                return jsonify({
                    'status': 'success',
                    'response': ai_response,
                    'action': 'analysis_complete'
                })
            else:
                return jsonify({
                    'status': 'error',
                    'response': f"Error running analysis: {result.get('message', 'Unknown error')}",
                    'action': 'error'
                })
                
        except Exception as e:
            logger.error(f"Error running analysis: {str(e)}")
            return jsonify({
                'status': 'error',
                'response': f"Error running analysis: {str(e)}",
                'action': 'error'
            })
    
    # Handle custom analysis with variables
    elif intent['type'] == 'run_analysis_variables' and all([session.get('csv_loaded', False), session.get('shapefile_loaded', False)]):
        # Get the extracted variables
        variables = intent['variables']
        
        # Validate variables against dataset
        cleaned_variables = clean_and_validate_variables(data_handler, variables)
        
        if not cleaned_variables or len(cleaned_variables) < 2:
            # Return a prompt for the user with available variables
            available_vars = get_available_variables(data_handler)
            available_vars_text = ", ".join(available_vars[:10])
            if len(available_vars) > 10:
                available_vars_text += f", and {len(available_vars) - 10} more"
            
            return jsonify({
                'status': 'error',
                'response': f"I couldn't find enough valid variables matching your request. You need to specify at least 2 valid variables. Some available variables are: {available_vars_text}. Would you like to try again with different variables?",
                'action': 'error'
            })
        
        # Return a confirmation message - the frontend will handle showing the confirmation UI
        variables_list = "<ul>" + "".join([f"<li>{var}</li>" for var in cleaned_variables]) + "</ul>"
        confirmation_response = f"""
        <p>I'll run a custom analysis with these variables for the composite score calculation:</p>
        {variables_list}
        <p>Please confirm if you want to proceed with these variables.</p>
        """
        
        return jsonify({
            'status': 'success',
            'response': confirmation_response,
            'variables': cleaned_variables,
            'action': 'confirm_custom_variables'
        })
    
    # Handle visualization requests
    elif intent['type'] in ['view_map', 'view_plot']:
        try:
            # Determine the visualization type and parameters
            viz_request = {}
            
            if intent['type'] == 'view_map':
                map_type = intent.get('map_type', 'unknown')
                if map_type == 'variable':
                    viz_request = {
                        'type': 'variable_map',
                        'variable': intent.get('variable_name')
                    }
                elif map_type == 'normalized':
                    viz_request = {
                        'type': 'normalized_map',
                        'variable': intent.get('variable_name')
                    }
                elif map_type == 'composite':
                    viz_request = {
                        'type': 'composite_map'
                    }
                elif map_type == 'vulnerability':
                    viz_request = {
                        'type': 'vulnerability_map'
                    }
                elif map_type == 'urban_extent':
                    viz_request = {
                        'type': 'urban_extent_map',
                        'threshold': intent.get('threshold', 50)
                    }
            elif intent['type'] == 'view_plot':
                plot_type = intent.get('plot_type', 'unknown')
                if plot_type == 'vulnerability':
                    viz_request = {
                        'type': 'vulnerability_plot'
                    }
                elif plot_type == 'decision_tree':
                    viz_request = {
                        'type': 'decision_tree'
                    }
            
            # Generate visualization if a valid type was determined
            if viz_request and 'type' in viz_request:
                # Call the function directly to prevent unnecessary route roundtrip
                result = get_visualization_result(viz_request, data_handler)
                
                if result['status'] == 'success':
                    return jsonify({
                        'status': 'success',
                        'response': result.get('ai_response', 'Here is the visualization you requested.'),
                        'visualization': result.get('image_path', ''),
                        'viz_type': result.get('viz_type', ''),
                        'current_page': result.get('current_page', 1),
                        'total_pages': result.get('total_pages', 1),
                        'action': 'show_visualization'
                    })
                else:
                    return jsonify({
                        'status': 'error',
                        'response': result.get('ai_response', result.get('message', 'Error generating visualization')),
                        'action': 'error'
                    })
            else:
                # If no valid visualization type was determined
                return jsonify({
                    'status': 'error',
                    'response': "I'm not sure what visualization you're looking for. You can ask for variable maps, normalized maps, composite maps, vulnerability plots, vulnerability maps, or urban extent maps.",
                    'action': 'error'
                })
        except Exception as e:
            logger.error(f"Error handling visualization request: {str(e)}")
            return jsonify({
                'status': 'error',
                'response': f"Error generating visualization: {str(e)}",
                'action': 'error'
            })
    
    # Handle report generation
    elif intent['type'] == 'generate_report':
        # Ensure analysis is complete before generating report
        if not session.get('analysis_complete', False):
             return jsonify({
                 'status': 'error',
                 'response': "Please run the analysis before generating a report.",
                 'action': 'error'
             })
        try:
            format_type = intent.get('format', 'pdf')

            # Call the report generator function (assuming it's imported)
            # Make sure generate_report_file exists or call report_generator.generate_report directly
            # For this example, let's assume you have a wrapper or call directly:
            import app.models.report_generator as report_gen # Make sure imported
            report_result = report_gen.generate_report(data_handler, format=format_type)

            # Check the status first
            if report_result.get('status') == 'success':
                report_url = report_result.get('report_url') # Use get for safety
                if report_url:
                    # Provide a user-friendly message and the download button info
                    ai_response = report_result.get('message', f'Your {format_type.upper()} report is ready.') # Get message
                    ai_response += f'<br><br><a href="{report_url}" class="btn btn-success" download target="_blank"><i class="fas fa-download"></i> Download {format_type.upper()} Report</a>'

                    return jsonify({
                        'status': 'success',
                        'response': ai_response,
                        'report_url': report_url, # Still useful to send if needed elsewhere by JS
                        'action': 'show_report' # Keep this action
                    })
                else:
                    # Handle case where success reported but URL missing (shouldn't happen ideally)
                    logger.error(f"Report generation successful but 'report_url'/'web_url' missing. Result: {report_result}")
                    return jsonify({
                        'status': 'error',
                        'response': f"Report generated ({report_result.get('format', 'unknown').upper()}), but the download link is missing. Please check server logs.",
                        'action': 'error'
                    })
            else:
                # Handle error status from report_generator
                return jsonify({
                    'status': 'error',
                    'response': f"Error generating report: {report_result.get('message', 'Unknown error during report generation.')}",
                    'action': 'error'
                })
        except Exception as e:
            logger.error(f"Exception in report generation route: {str(e)}", exc_info=True)
            return jsonify({
                'status': 'error',
                'response': f"An unexpected error occurred while processing the report request: {str(e)}",
                'action': 'error'
            })
    
    # Handle language change
    elif intent['type'] == 'change_language':
        try:
            language = intent.get('language', 'en')
            session['current_language'] = language
            
            # Generate confirmation response in the new language
            language_names = {
                'en': 'English',
                'ha': 'Hausa',
                'yo': 'Yoruba',
                'ig': 'Igbo',
                'fr': 'French',
                'ar': 'Arabic'
            }
            
            ai_response = f"<strong>Language changed to:</strong> {language_names.get(language, 'English')}"
            
            return jsonify({
                'status': 'success',
                'response': ai_response,
                'action': 'language_changed'
            })
        except Exception as e:
            logger.error(f"Error changing language: {str(e)}")
            return jsonify({
                'status': 'error',
                'response': f"Error changing language: {str(e)}",
                'action': 'error'
            })
    
    # General queries with AI response
    else:
        # Generate a contextual response using OpenAI
        analysis_result = session.get('analysis_result', None)
        ai_response = generate_ai_response(user_message, session_state, intent, analysis_result)
        
        # Fallback if OpenAI is not available
        if not ai_response:
            ai_response = get_fallback_response(user_message, session_state)
        
        return jsonify({
            'status': 'success',
            'response': ai_response
        })

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
        return send_from_directory(session_folder, filename, as_attachment=True)
    except Exception as e:
        logger.error(f"Error serving report file {filename} for session {session.get('session_id', 'default')}: {e}")
        return jsonify({'status': 'error', 'message': 'Could not serve report file.'}), 500

# Function to clean and validate variable names
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

def parse_message_intent(message, session_state, data_handler=None):
    """Parse the intent from a user message"""
    message_lower = message.lower()
    
    # Check for analysis intent with custom parameters
    if any(keyword in message_lower for keyword in ['run', 'analyze', 'process', 'rerun']):
        # Check if this is a custom analysis with variables
        custom_analysis_patterns = [
            r'rerun.*analysis.*variables',
            r'rerun.*with.*variables',
            r'run.*with.*variables',
            r'use.*variables',
            r'using.*variables',
            r'following\s+variables',
            r'composite.*score.*calculation',
            r'calculation.*use'
        ]
        
        is_custom = any(re.search(pattern, message_lower) for pattern in custom_analysis_patterns)
        
        if is_custom:
            # Extract variables for the analysis
            extracted_variables = extract_variables(message, data_handler)
            
            # If variables were found, treat as custom analysis request
            if extracted_variables and len(extracted_variables) > 0:
                return {
                    'type': 'run_analysis_variables',
                    'variables': extracted_variables
                }
                
        # Default to standard analysis
        return {'type': 'run_analysis'}
    
    # Check for visualization intent
    if any(keyword in message_lower for keyword in ['show', 'display', 'view', 'see', 'map', 'plot', 'visualization', 'chart']):
        # Decision tree visualization (highest priority to check)
        if 'tree' in message_lower or 'decision' in message_lower or 'workflow' in message_lower:
            return {'type': 'view_plot', 'plot_type': 'decision_tree'}
            
        # Box and whisker plot (check this before other vulnerability visualizations)
        if ('box' in message_lower or 'whisker' in message_lower or 'ranking' in message_lower) and 'map' not in message_lower:
            if 'vulnerability' in message_lower or 'vulnerable' in message_lower or 'ranking' in message_lower:
                return {'type': 'view_plot', 'plot_type': 'vulnerability'}
            # Default to vulnerability plot if just asking for box plot without specifying
            if 'box' in message_lower or 'whisker' in message_lower:
                return {'type': 'view_plot', 'plot_type': 'vulnerability'}
        
        # Map visualizations
        if 'map' in message_lower:
            # Variable distribution map
            if any(var_word in message_lower for var_word in ['variable', 'distribution']):
                var_name = extract_variable_name(message_lower, data_handler)
                return {'type': 'view_map', 'map_type': 'variable', 'variable_name': var_name}
            
            # Normalized map
            if 'normalize' in message_lower or 'normalized' in message_lower:
                var_name = extract_variable_name(message_lower, data_handler)
                return {'type': 'view_map', 'map_type': 'normalized', 'variable_name': var_name}
            
            # Composite map
            if any(word in message_lower for word in ['composite', 'risk', 'score']):
                return {'type': 'view_map', 'map_type': 'composite'}
            
            # Vulnerability map
            if ('vulnerability' in message_lower or 'vulnerable' in message_lower) and 'map' in message_lower:
                return {'type': 'view_map', 'map_type': 'vulnerability'}
            
            # Urban extent map
            if any(word in message_lower for word in ['urban', 'extent', 'threshold']):
                import re
                threshold_match = re.search(r'(\d+)\s*%', message_lower)
                threshold = 30  # Default
                if threshold_match:
                    threshold = int(threshold_match.group(1))
                return {'type': 'view_map', 'map_type': 'urban_extent', 'threshold': threshold}
            
            # Default map type (if just asking for a map)
            return {'type': 'view_map', 'map_type': 'composite'}
        
        # Check for decision tree again (in case it wasn't caught above)
        if 'tree' in message_lower or 'decision' in message_lower or 'flow' in message_lower:
            return {'type': 'view_plot', 'plot_type': 'decision_tree'}
        
        # Check for variable visualization when not specifying map
        var_name = extract_variable_name(message_lower, data_handler)
        if var_name:
            return {'type': 'view_map', 'map_type': 'variable', 'variable_name': var_name}
    
    # Check for report generation
    if any(word in message_lower for word in ['report', 'generate', 'download', 'pdf', 'docx', 'html']):
        format_type = 'pdf'  # Default
        if 'html' in message_lower:
            format_type = 'html'
        elif 'word' in message_lower or 'docx' in message_lower:
            format_type = 'docx'
        
        return {'type': 'generate_report', 'format': format_type}
    
    # Check for language change
    if any(word in message_lower for word in ['language', 'speak', 'talk']):
        language = 'en'  # Default
        if 'french' in message_lower or 'français' in message_lower:
            language = 'fr'
        elif 'hausa' in message_lower:
            language = 'ha'
        elif 'yoruba' in message_lower:
            language = 'yo'
        elif 'igbo' in message_lower:
            language = 'ig'
        
        return {'type': 'change_language', 'language': language}
    
    # Default: general query
    return {'type': 'general_query'}

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
        base_name = var_lower
        for prefix in prefixes:
            for suffix in suffixes:
                if prefix + base_name + suffix != var_lower:
                    variable_metadata[var]['alternative_names'].add(prefix + base_name + suffix)
        
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


def extract_variables(message, data_handler=None):
    """
    Extract variable names from a message for custom analysis with improved processing
    
    Args:
        message: The user message
        data_handler: Optional DataHandler instance for variable validation
    
    Returns:
        list: List of extracted variable names
    """
    message_lower = message.lower()
    
    # First check for variable specifications with explicit lists
    # Pattern 1: Look for "variables: var1, var2, var3" or "variables: var1 var2 var3" 
    explicit_var_pattern = r'variables?:?\s+([^.;!?]+)'
    
    # Pattern 2: Look for "using X, Y, Z" or "use X, Y, Z"
    using_var_pattern = r'(?:use|using|with)\s+([^.;!?]+?)(?:for|in|to|variable|$)'
    
    # Pattern 3: Look for "following variables: X, Y, Z"
    following_var_pattern = r'following\s+variables:?\s+([^.;!?]+)'
    
    # Try each pattern
    patterns = [explicit_var_pattern, using_var_pattern, following_var_pattern]
    raw_variables = []
    
    for pattern in patterns:
        var_match = re.search(pattern, message_lower)
        if var_match:
            var_list_text = var_match.group(1).strip()
            
            # First, check for comma-separated list
            if ',' in var_list_text:
                # Split by commas
                var_list = var_list_text.split(',')
                raw_variables.extend([v.strip() for v in var_list if v.strip()])
            # Then check for "and" joining
            elif ' and ' in var_list_text:
                # Split by "and"
                var_list = var_list_text.split(' and ')
                raw_variables.extend([v.strip() for v in var_list if v.strip()])
            else:
                # Try to identify variables in space-separated text
                # This is trickier, let's see if any known variables match first
                if data_handler:
                    # Get variable metadata
                    variable_metadata = None
                    if 'variable_metadata' in session:
                        variable_metadata = session.get('variable_metadata')
                    elif hasattr(data_handler, 'variable_metadata'):
                        variable_metadata = data_handler.variable_metadata
                    
                    # If metadata available, check for matches in the text
                    if variable_metadata:
                        identified_vars = set()
                        for var, metadata in variable_metadata.items():
                            original_name = var.lower()
                            # Check if original name is in the text
                            if original_name in var_list_text:
                                identified_vars.add(var)
                                continue
                            
                            # Check alternative names
                            for alt_name in metadata['alternative_names']:
                                if alt_name in var_list_text:
                                    identified_vars.add(var)
                                    # Remove matching text to avoid double matches
                                    var_list_text = var_list_text.replace(alt_name, '')
                                    break
                        
                        # Add identified variables
                        raw_variables.extend(list(identified_vars))
                
                # If no variables identified or no metadata available, try splitting by spaces and filtering
                if not raw_variables:
                    words = var_list_text.split()
                    # Filter out stop words
                    stop_words = ['the', 'following', 'variables', 'variable', 'these', 'in', 'for', 'and', 'to', 'with']
                    for word in words:
                        word = word.strip()
                        if word and word not in stop_words and len(word) > 2:
                            raw_variables.append(word)
            
            # Found variables with this pattern, no need to try others if we have results
            if raw_variables:
                break
    
    # If no variables were found using the regular patterns, try compound variable name detection
    if not raw_variables:
        # Look for compound variable names like "mean rainfall" or "distance to water"
        compound_patterns = [
            r'(mean\s+\w+)', 
            r'(distance\s+to\s+\w+)',
            r'(soil\s+wetness)',
            r'(housing\s+quality)',
            r'(\w+\s+tpr)',
            r'(test\s+positivity\s+rate)',
            r'(settlement\s+type)'
        ]
        
        for pattern in compound_patterns:
            matches = re.findall(pattern, message_lower)
            if matches:
                raw_variables.extend(matches)
    
    # If still no variables found, check for specific variable names
    if not raw_variables:
        # Check for common variable words
        common_vars = [
            'rainfall', 'temperature', 'elevation', 'population', 'distance', 
            'housing', 'ndvi', 'evi', 'flood', 'ndwi', 'soil_wetness', 
            'settlement_type', 'u5_tpr', 'tpr', 'mean_rainfall', 'mean_evi', 
            'mean_ndvi', 'distance_to_water', 'housing_quality'
        ]
        
        for var in common_vars:
            var_lower = var.lower()
            if var_lower in message_lower:
                # Check if it's a standalone mention (not part of another word)
                word_boundaries = r'\b' + re.escape(var_lower) + r'\b'
                if re.search(word_boundaries, message_lower):
                    raw_variables.append(var)
    
    # If data_handler is provided, validate the variables
    if data_handler and raw_variables:
        # Use stored variables from session if available
        if 'available_variables' in session:
            available_vars = session.get('available_variables')
            # Perform matching with available variables
            matched_variables = match_variables_to_dataset(raw_variables, available_vars, session.get('variable_metadata'))
            return matched_variables
        else:
            # Fall back to the original validation method
            return clean_and_validate_variables(data_handler, raw_variables)
    
    return raw_variables


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

def generate_report_file(data_handler, format_type='pdf'):
    """
    Generate a report file
    
    Args:
        data_handler: DataHandler instance
        format_type: Report format type ('pdf', 'html', 'docx')
        
    Returns:
        dict: Result with status and report URL
    """
    try:
        result = report_gen.generate_report(data_handler, format=format_type)
        
        if result['status'] == 'success':
            return {
                'status': 'success',
                'message': result['message'],
                'report_url': result['web_url'],
                'ai_response': f"I've generated a {format_type.upper()} report for you. You can download it using the link below. The report includes data overview, missing value handling, variable relationships, composite scores, vulnerability rankings, and urban extent analysis."
            }
        else:
            return {
                'status': 'error',
                'message': result['message'],
                'ai_response': f"I encountered an error while generating the {format_type.upper()} report. Please try again or check if all required data is available."
            }
    except Exception as e:
        logger.error(f"Error generating report: {str(e)}")
        return {
            'status': 'error',
            'message': f'Error generating report: {str(e)}',
            'ai_response': f"I encountered an error while generating the report. {str(e)}"
        }
        
def generate_ai_response(message, session_state, intent, analysis_result=None):
    """Generate an AI response using OpenAI"""
    try:
        api_key = current_app.config.get('OPENAI_API_KEY')
        if not api_key:
            return None # Fallback will be used
            
        system_message = get_system_message(session_state, analysis_result)
        conversation_history = session.get('conversation_history', [])
        
        conversation_history.append({"role": "user", "content": message})
        
        # Keep only the last N messages for context to OpenAI
        # And limit what's stored back in the session
        MAX_HISTORY_FOR_OPENAI = 20 # e.g., last 10 pairs
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
        logger.error(f"Error generating AI response: {str(e)}")
        return None # Fallback will be used

def get_system_message(session_state, analysis_result=None):
    """Generate a system message with the current state for the AI"""
    base_message = """
    You are an AI assistant for the Malaria Reprioritization Tool (MRPT), a sophisticated application for malaria risk analysis
    and bed net distribution planning in Nigeria. You combine expertise in epidemiology, spatial analysis, and data science with
    a conversational, helpful approach. Your role is to guide users through their analysis while being responsive to questions.
    
    Be warm, friendly, and conversational - like a helpful companion rather than a formal tool.
    Use a natural, engaging tone while maintaining professionalism.
    Keep explanations concise and clear, avoiding unnecessary jargon.
    
    When the user asks to see visualizations, suggest specific types they can request:
    - Variable distribution maps (showing original data distribution)
    - Normalized variable maps (showing values adjusted for relationship with malaria risk)
    - Composite risk maps (showing combined risk scores from multiple variables)
    - Vulnerability ranking plot (box and whisker plot showing ward vulnerability)
    - Vulnerability map (showing geographical distribution of vulnerability)
    - Urban extent maps (showing areas above/below urban thresholds)
    - Decision tree visualization (showing the analysis workflow)
    
    When explaining visualizations:
    - For variable maps: Explain how darker colors represent higher values
    - For normalized maps: Explain the variable relationship (direct/inverse) with malaria risk
    - For composite maps: Explain how multiple variables combine to create risk score
    - For vulnerability plots: Explain how wards are ranked from most to least vulnerable
    - For urban extent maps: Explain how thresholds determine resource allocation decisions
    
    Users can customize analysis by selecting specific variables for the composite score calculation.
    Help users understand how to specify the variables they want to include in their analysis.
    """
    
    # Add current session state
    state_message = "\n\nCurrent session state:\n"
    
    if session_state.get('csv_loaded', False):
        csv_rows = session.get('csv_rows', 0)
        csv_columns = session.get('csv_columns', 0)
        state_message += f"- CSV data loaded: {csv_rows} rows, {csv_columns} columns\n"
    else:
        state_message += "- CSV data has not been loaded yet\n"
    
    if session_state.get('shapefile_loaded', False):
        state_message += "- Shapefile data has been loaded\n"
    else:
        state_message += "- Shapefile data has not been loaded yet\n"
    
    if session_state.get('analysis_complete', False):
        state_message += "- Analysis is complete. All visualizations are available.\n"
    else:
        state_message += "- Analysis has not been run yet\n"
    
    # Add analysis results if available
    if analysis_result:
        result_message = "\n\nAnalysis results overview:\n"
        
        if 'variables_used' in analysis_result:
            result_message += f"- Variables used in analysis: {', '.join(analysis_result['variables_used'])}\n"
        
        if 'vulnerable_wards' in analysis_result:
            top_wards = analysis_result['vulnerable_wards'][:5]
            result_message += f"- Top 5 most vulnerable wards: {', '.join(top_wards)}\n"
    else:
        result_message = ""
    
    return base_message + state_message + result_message

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
        return "Great! Your analysis is complete. You can ask me to show you various visualizations like maps, plots, or generate a report. What would you like to see?"