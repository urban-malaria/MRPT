# MRPT (Malaria Reprioritization Tool)

An AI-powered tool for malaria risk analysis and intervention prioritization.

## Overview

The Malaria Reprioritization Tool (MRPT) helps stakeholders analyze malaria risk factors and prioritize resources for bed net distribution and other interventions. This AI-powered assistant provides an interactive interface to:

- Analyze geospatial and tabular data
- Create variable distribution maps
- Generate composite risk scores
- Identify vulnerable areas
- Visualize urban extent analysis
- Produce comprehensive reports

## Project Structure

### Backend Files
- `__init__.py` - Flask application initialization with configuration for uploads, logging, and error handling
- `analysis.py` - Core analysis functions for data processing, normalization, and risk scoring
- `data_handler.py` - Manages data loading, validation, and transformation
- `visualization.py` - Creates interactive visualizations (maps, plots, etc.) using Plotly
- `report_generator.py` - Generates PDF/HTML reports summarizing analysis results
- `routes.py` - Flask routes handling API endpoints and user interactions

### Frontend Files
- `index.html` - Main application interface with chat interface and modals
- `main.js` - Client-side logic for handling user interactions and visualizations
- `styles.css` - Custom styling for the application interface

## Technical Details

### Key Technologies
- **Flask**: Backend web framework
- **GeoPandas**: Geospatial data analysis
- **Plotly**: Interactive visualizations
- **Bootstrap**: Frontend styling
- **OpenAI Integration**: For natural language processing

### Key Features
- **Data Upload**: Support for CSV/Excel files and shapefiles (ZIP)
- **Data Analysis**: Normalization, composite score calculation, vulnerability ranking
- **Visualizations**:
  - Variable distribution maps
  - Normalized variable maps
  - Composite risk maps
  - Vulnerability ranking plots
  - Urban extent maps
  - Decision tree workflow
- **Reporting**: Generate PDF/HTML reports summarizing findings
- **Multi-language Support**: English, Hausa, Yoruba, Igbo, French, Arabic

## Getting Started

1. Clone this repository
2. Install dependencies: `pip install -r requirements.txt`
3. Set up environment variables (see .env.example)
4. Run the app: `python app.py`

## Usage

Upload your CSV/Excel data and shapefiles, or use the sample data, and follow the AI assistant's guidance to analyze malaria risk factors.

### Data Requirements
- CSV/Excel file should contain ward-level data with variables like rainfall, temperature, elevation, etc.
- Shapefile should contain ward boundaries in a standard GIS format (zipped)
- Both files should have a 'WardName' column for joining data

## Analysis Methodology

The tool employs a multi-step approach:
1. **Data Cleaning**: Handling missing values using spatial, mean, or mode imputation
2. **Variable Normalization**: Scaling values based on relationship with malaria risk
3. **Composite Scoring**: Combining variables using mean, weighted mean, or PCA methods
4. **Vulnerability Ranking**: Prioritizing wards based on composite scores
5. **Urban Extent Analysis**: Applying urban thresholds for intervention targeting
