# ChatMRPT - Malaria Reprioritization Tool (Chat Interface)

An AI-powered chat interface for the Malaria Reprioritization Tool.

## Overview

The ChatMRPT module provides an interactive chat interface for malaria risk analysis and intervention prioritization. It allows users to:

- Upload and analyze CSV/Excel data and shapefiles
- Generate various visualizations of malaria risk factors
- Create composite risk scores and vulnerability rankings
- Analyze urban extent for intervention targeting
- Generate comprehensive reports

## Files in this Module

### Backend Files
- `__init__.py` - Flask application initialization with configuration
- `analysis.py` - Core analysis functions for data processing and risk scoring
- `data_handler.py` - Manages data loading and transformation
- `visualization.py` - Creates interactive maps and plots using Plotly
- `report_generator.py` - Generates PDF/HTML reports
- `routes.py` - Flask routes handling API endpoints

### Frontend Files
- `index.html` - Main chat interface with modals for file upload and report generation
- `main.js` - Client-side logic for the chat interface and visualization display
- `styles.css` - Custom styling for the interface

## Features

- Interactive chat interface with AI assistance
- Data upload capabilities (CSV/Excel and Shapefile)
- Multiple visualization types:
  - Variable distribution maps
  - Normalized variable maps
  - Composite risk maps
  - Vulnerability ranking plots
  - Urban extent maps
  - Decision tree workflow diagrams
- Report generation in multiple formats
- Multi-language support
