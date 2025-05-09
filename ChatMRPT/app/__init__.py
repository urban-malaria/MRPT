# app/__init__.py

import os
import logging
from flask import Flask, jsonify
from dotenv import load_dotenv

# Load environment variables from .env file
load_dotenv()

def create_app(test_config=None):
    # Create and configure the app
    app = Flask(__name__, instance_relative_config=True) # instance_relative_config=True is important

    # Ensure the instance folder exists FIRST, as UPLOAD_FOLDER and REPORTS_FOLDER depend on it
    try:
        os.makedirs(app.instance_path, exist_ok=True)
    except OSError:
        pass # Should not happen if instance_path is valid

    app.config.from_mapping(
        SECRET_KEY=os.environ.get('SECRET_KEY', 'dev'),
        # OLD paths:
        # UPLOAD_FOLDER=os.path.join(app.static_folder, 'uploads'),
        # REPORTS_FOLDER=os.path.join(app.static_folder, 'reports'),
        # NEW paths using instance_path:
        UPLOAD_FOLDER=os.path.join(app.instance_path, 'uploads'),
        REPORTS_FOLDER=os.path.join(app.instance_path, 'reports'),
        MAX_CONTENT_LENGTH=50 * 1024 * 1024,  # 50MB max upload size
        OPENAI_API_KEY=os.environ.get('OPENAI_API_KEY'),
        RELOADER_EXCLUSIONS=['*uploads*', '*.zip', '*.csv', '*.json', '*.png', '*.html'], # Keep
        RESPONSE_TIMEOUT=300  # 5 minutes
    )

    # Configure logging (remains the same)
    logging.basicConfig(
        level=logging.INFO,
        format='%(asctime)s - %(name)s - %(levelname)s - %(message)s',
        handlers=[
            logging.StreamHandler(),
            logging.FileHandler(os.path.join(app.instance_path, 'app.log'), mode='a')
        ]
    )

    if test_config is None:
        # Load the instance config, if it exists, when not testing
        app.config.from_pyfile('config.py', silent=True)
    else:
        # Load the test config if passed in
        app.config.from_mapping(test_config)

    # Ensure upload folders exist (these paths now use app.instance_path)
    os.makedirs(app.config['UPLOAD_FOLDER'], exist_ok=True)
    os.makedirs(app.config['REPORTS_FOLDER'], exist_ok=True)

    # Register blueprints
    from .routes import main_bp
    app.register_blueprint(main_bp)

    # Add error handlers (remains the same)
    @app.errorhandler(500)
    def server_error(e):
        logging.error(f"Server error: {str(e)}")
        return jsonify({
            'status': 'error',
            'message': 'Internal server error occurred. Please check logs for details.'
        }), 500

    @app.errorhandler(413)
    def request_entity_too_large(e):
        return jsonify({
            'status': 'error',
            'message': f'File too large. Maximum allowed size is {app.config["MAX_CONTENT_LENGTH"] / (1024 * 1024)}MB.'
        }), 413

    @app.errorhandler(404)
    def not_found(e):
        return jsonify({
            'status': 'error',
            'message': 'The requested resource was not found.'
        }), 404

    return app