# app/models/interaction_logger.py
import os
import json
import logging
import datetime
from flask import current_app, session
import sqlite3
import uuid

# Set up logging
logger = logging.getLogger(__name__)

class InteractionLogger:
    """Class to log and store user interactions with the MRPT AI Assistant"""
    
    def __init__(self, db_path=None):
        """Initialize with database path"""
        if db_path is None:
            # Use instance folder by default (instance/interactions.db)
            self.db_path = os.path.join(current_app.instance_path, 'interactions.db')
        else:
            self.db_path = db_path
            
        # Ensure the database exists and has the correct schema
        self._init_database()
    
    def _init_database(self):
        """Initialize the database with required tables if they don't exist"""
        try:
            # Ensure parent directory exists
            os.makedirs(os.path.dirname(self.db_path), exist_ok=True)
            
            # Connect to database and create tables if they don't exist
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            
            # Create sessions table
            cursor.execute('''
            CREATE TABLE IF NOT EXISTS sessions (
                session_id TEXT PRIMARY KEY,
                start_time TIMESTAMP,
                last_activity TIMESTAMP,
                user_language TEXT,
                browser_info TEXT,
                ip_address TEXT
            )
            ''')
            
            # Create messages table
            cursor.execute('''
            CREATE TABLE IF NOT EXISTS messages (
                message_id TEXT PRIMARY KEY,
                session_id TEXT,
                timestamp TIMESTAMP,
                sender TEXT,
                content TEXT,
                FOREIGN KEY (session_id) REFERENCES sessions (session_id)
            )
            ''')
            
            # Create file_uploads table
            cursor.execute('''
            CREATE TABLE IF NOT EXISTS file_uploads (
                upload_id TEXT PRIMARY KEY,
                session_id TEXT,
                timestamp TIMESTAMP,
                file_type TEXT,
                file_name TEXT,
                file_size INTEGER,
                metadata TEXT,
                FOREIGN KEY (session_id) REFERENCES sessions (session_id)
            )
            ''')
            
            # Create analysis_events table
            cursor.execute('''
            CREATE TABLE IF NOT EXISTS analysis_events (
                event_id TEXT PRIMARY KEY,
                session_id TEXT,
                timestamp TIMESTAMP,
                event_type TEXT,
                details TEXT,
                success BOOLEAN,
                FOREIGN KEY (session_id) REFERENCES sessions (session_id)
            )
            ''')
            
            # Create errors table
            cursor.execute('''
            CREATE TABLE IF NOT EXISTS errors (
                error_id TEXT PRIMARY KEY,
                session_id TEXT,
                timestamp TIMESTAMP,
                error_type TEXT,
                error_message TEXT,
                stack_trace TEXT,
                FOREIGN KEY (session_id) REFERENCES sessions (session_id)
            )
            ''')
            
            conn.commit()
            conn.close()
            logger.info(f"Interaction database initialized at {self.db_path}")
            
        except Exception as e:
            logger.error(f"Error initializing interaction database: {str(e)}")
    
    def log_session_start(self, session_id, browser_info=None, ip_address=None):
        """Log the start of a new session"""
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            
            now = datetime.datetime.now()
            language = session.get('current_language', 'en')
            
            cursor.execute('''
            INSERT OR REPLACE INTO sessions 
            (session_id, start_time, last_activity, user_language, browser_info, ip_address)
            VALUES (?, ?, ?, ?, ?, ?)
            ''', (session_id, now, now, language, browser_info, ip_address))
            
            conn.commit()
            conn.close()
            logger.info(f"Logged session start: {session_id}")
            
        except Exception as e:
            logger.error(f"Error logging session start: {str(e)}")
    
    def log_message(self, session_id, sender, content):
        """Log a message exchange between user and assistant"""
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            
            now = datetime.datetime.now()
            message_id = str(uuid.uuid4())
            
            cursor.execute('''
            INSERT INTO messages (message_id, session_id, timestamp, sender, content)
            VALUES (?, ?, ?, ?, ?)
            ''', (message_id, session_id, now, sender, content))
            
            # Update last activity for the session
            cursor.execute('''
            UPDATE sessions SET last_activity = ? WHERE session_id = ?
            ''', (now, session_id))
            
            conn.commit()
            conn.close()
            logger.info(f"Logged {sender} message for session: {session_id}")
            
            return message_id
            
        except Exception as e:
            logger.error(f"Error logging message: {str(e)}")
            return None
    
    def log_file_upload(self, session_id, file_type, file_name, file_size, metadata=None):
        """Log a file upload event"""
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            
            now = datetime.datetime.now()
            upload_id = str(uuid.uuid4())
            
            if isinstance(metadata, dict):
                metadata_json = json.dumps(metadata)
            else:
                metadata_json = metadata
            
            cursor.execute('''
            INSERT INTO file_uploads 
            (upload_id, session_id, timestamp, file_type, file_name, file_size, metadata)
            VALUES (?, ?, ?, ?, ?, ?, ?)
            ''', (upload_id, session_id, now, file_type, file_name, file_size, metadata_json))
            
            # Update last activity for the session
            cursor.execute('''
            UPDATE sessions SET last_activity = ? WHERE session_id = ?
            ''', (now, session_id))
            
            conn.commit()
            conn.close()
            logger.info(f"Logged file upload: {file_name} for session: {session_id}")
            
            return upload_id
            
        except Exception as e:
            logger.error(f"Error logging file upload: {str(e)}")
            return None
    
    def log_analysis_event(self, session_id, event_type, details, success=True):
        """Log an analysis event (visualization, report generation, etc.)"""
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            
            now = datetime.datetime.now()
            event_id = str(uuid.uuid4())
            
            if isinstance(details, dict):
                details_json = json.dumps(details)
            else:
                details_json = details
            
            cursor.execute('''
            INSERT INTO analysis_events 
            (event_id, session_id, timestamp, event_type, details, success)
            VALUES (?, ?, ?, ?, ?, ?)
            ''', (event_id, session_id, now, event_type, details_json, success))
            
            # Update last activity for the session
            cursor.execute('''
            UPDATE sessions SET last_activity = ? WHERE session_id = ?
            ''', (now, session_id))
            
            conn.commit()
            conn.close()
            logger.info(f"Logged {event_type} event for session: {session_id}, success: {success}")
            
            return event_id
            
        except Exception as e:
            logger.error(f"Error logging analysis event: {str(e)}")
            return None
    
    def log_error(self, session_id, error_type, error_message, stack_trace=None):
        """Log an error that occurred during interaction"""
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            
            now = datetime.datetime.now()
            error_id = str(uuid.uuid4())
            
            cursor.execute('''
            INSERT INTO errors 
            (error_id, session_id, timestamp, error_type, error_message, stack_trace)
            VALUES (?, ?, ?, ?, ?, ?)
            ''', (error_id, session_id, now, error_type, error_message, stack_trace))
            
            conn.commit()
            conn.close()
            logger.info(f"Logged error: {error_type} for session: {session_id}")
            
            return error_id
            
        except Exception as e:
            logger.error(f"Error logging error event: {str(e)}")
            return None
    
    def update_session_language(self, session_id, language):
        """Update the language preference for a session"""
        try:
            conn = sqlite3.connect(self.db_path)
            cursor = conn.cursor()
            
            cursor.execute('''
            UPDATE sessions SET user_language = ? WHERE session_id = ?
            ''', (language, session_id))
            
            conn.commit()
            conn.close()
            logger.info(f"Updated language to {language} for session: {session_id}")
            
        except Exception as e:
            logger.error(f"Error updating session language: {str(e)}")