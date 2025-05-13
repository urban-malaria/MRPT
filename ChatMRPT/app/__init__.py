# app/__init__.py

import os
import logging
from flask import Flask, jsonify, request # Added 'request' here
from dotenv import load_dotenv
from pathlib import Path

# Load environment variables from .env file
load_dotenv()

def create_app(test_config=None):
    # Create and configure the app
    app = Flask(__name__, instance_relative_config=True) # instance_relative_config=True is important

    # --- Ensure the instance folder exists FIRST ---
    try:
        instance_dir = Path(app.instance_path)
        instance_dir.mkdir(parents=True, exist_ok=True)
        # Use app.logger after basic app init, print if before
        # For now, assume app.logger is available after Flask(__name__)
        app.logger.info(f"Ensured instance directory exists: {instance_dir}")
    except OSError as e:
        log_func = app.logger.error if hasattr(app, 'logger') and app.logger else print
        log_func(f"CRITICAL ERROR: Could not create instance directory '{app.instance_path}': {e}")
        pass

    # --- Define RELOADER_EXCLUSIONS ---
    reloader_exclusions = [
        os.path.join('instance', 'uploads', '**', '*'),
        os.path.join('instance', 'reports', '**', '*'),
        os.path.join('instance', 'app.log'),
        '*.zip', '*.csv', '*.json', '*.png', '*.html', '*.xlsx', '*.xls',
        '*.shp', '*.dbf', '*.shx', '*.prj', '*.cpg',
        '*.geojson',
    ]
    # app.logger might not be fully configured here yet if using basicConfig later,
    # but Flask's default logger should pick this up if run with flask run.
    # For safety, print if app.logger is not available.
    log_init_func = app.logger.info if hasattr(app, 'logger') and app.logger else print
    log_init_func(f"Setting RELOADER_EXCLUSIONS: {reloader_exclusions}")


    # --- Configure the app ---
    app.config.from_mapping(
        SECRET_KEY=os.environ.get('SECRET_KEY', 'dev'),
        UPLOAD_FOLDER=os.path.join(app.instance_path, 'uploads'),
        REPORTS_FOLDER=os.path.join(app.instance_path, 'reports'),
        MAX_CONTENT_LENGTH=50 * 1024 * 1024,
        OPENAI_API_KEY=os.environ.get('OPENAI_API_KEY'),
        RELOADER_EXCLUSIONS=reloader_exclusions,
        RESPONSE_TIMEOUT=300
    )

    # --- Configure logging ---
    log_file_path = os.path.join(app.instance_path, 'app.log')
    os.makedirs(os.path.dirname(log_file_path), exist_ok=True)

    log_formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')

    stream_handler = logging.StreamHandler()
    stream_handler.setFormatter(log_formatter)

    file_handler = logging.FileHandler(log_file_path, mode='a')
    file_handler.setFormatter(log_formatter)

    # Configure app.logger
    if app.logger: # Check if Flask has already initialized its logger
        # Remove default handlers if any to avoid duplicate logs if we add our own
        for handler in app.logger.handlers[:]:
            app.logger.removeHandler(handler)
        app.logger.addHandler(stream_handler)
        app.logger.addHandler(file_handler)
        app.logger.setLevel(logging.INFO)
    else: # Fallback if app.logger is not yet available (should be rare with Flask())
        logging.basicConfig(
            level=logging.INFO,
            format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
            handlers=[stream_handler, file_handler]
        )


    # Configure the root logger for consistency across modules
    root_logger = logging.getLogger()
    # Set level for root logger, can be different from app.logger if needed
    root_logger.setLevel(logging.INFO)
    # Clear any existing handlers on root to avoid duplication or conflicts
    for handler in root_logger.handlers[:]:
        root_logger.removeHandler(handler)
    root_logger.addHandler(stream_handler)
    root_logger.addHandler(file_handler)

    app.logger.info("Flask app configured and logging set up.")
    app.logger.info(f"Instance path: {app.instance_path}")
    app.logger.info(f"Upload folder: {app.config['UPLOAD_FOLDER']}")
    app.logger.info(f"Reports folder: {app.config['REPORTS_FOLDER']}")
    app.logger.info(f"Log file: {log_file_path}")


    # --- Load instance config / test config ---
    if test_config is None:
        app.config.from_pyfile('config.py', silent=True)
        app.logger.info("Attempted to load instance/config.py (if exists).")
    else:
        app.config.from_mapping(test_config)
        app.logger.info("Loaded test configuration.")

    # --- Ensure upload/report folders exist (using configured paths) ---
    try:
        os.makedirs(app.config['UPLOAD_FOLDER'], exist_ok=True)
        os.makedirs(app.config['REPORTS_FOLDER'], exist_ok=True)
        app.logger.info("Ensured upload and report directories exist.")
    except OSError as e:
        app.logger.error(f"Error creating upload/report directories: {e}")

    # --- Register blueprints ---
    try:
        from .routes import main_bp
        app.register_blueprint(main_bp)
        app.logger.info("Registered main blueprint.")
    except ImportError as e:
         app.logger.error(f"Failed to import or register blueprint: {e}")

    # --- Add error handlers ---
    @app.errorhandler(500)
    def server_error(e):
        app.logger.error(f"Server error occurred: {str(e)}", exc_info=True)
        return jsonify({
            'status': 'error',
            'message': 'Internal server error occurred. Please check logs for details.'
        }), 500

    @app.errorhandler(413)
    def request_entity_too_large(e):
        app.logger.warning(f"File upload rejected (too large): {e}")
        max_mb = app.config["MAX_CONTENT_LENGTH"] / (1024 * 1024)
        return jsonify({
            'status': 'error',
            'message': f'File too large. Maximum allowed size is {max_mb:.1f}MB.'
        }), 413

    @app.errorhandler(404)
    def not_found(e):
        # Use the imported 'request' object here
        app.logger.info(f"Resource not found (404): {request.url}")
        return jsonify({
            'status': 'error',
            'message': 'The requested resource was not found.'
        }), 404

    app.logger.info("Error handlers registered.")
    app.logger.info("App creation complete.")
    return app