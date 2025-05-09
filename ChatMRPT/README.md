## Overview

ChatMRPT is an interactive chat-based interface for the Malaria Reprioritization Tool (MRPT), providing stakeholders with an intuitive way to analyze malaria risk factors and prioritize resource allocation for interventions like bed net distribution. This module combines advanced geospatial analysis with a natural language interface to make complex data analysis accessible to non-technical users.

## Folder Structure
ChatMRPT/
├── .env                       # Environment variables configuration
├── init_project.py            # Project initialization script
├── requirements.txt           # Python package dependencies
├── run.py                     # Application entry point
│
├── app/                       # Main application package
│   ├── init.py            # Flask application initialization
│   ├── routes.py              # API endpoints and request handling
│   │
│   ├── models/                # Core analytical components
│   │   ├── analysis.py        # Data analysis functions
│   │   ├── data_handler.py    # Data loading and processing
│   │   ├── report_generator.py # Report creation utilities
│   │   └── visualization.py   # Visualization generation
│   │
│   ├── static/                # Static files
│   │   ├── css/               # Stylesheets
│   │   │   └── styles.css     # Main CSS styling
│   │   ├── js/                # JavaScript files
│   │   │   └── main.js        # Frontend interaction logic
│   │   └── uploads/           # User uploaded files (not in git)
│   │
│   ├── templates/             # HTML templates
│   │   └── index.html         # Main application interface
│   │
│   └── sample_data/           # Example datasets for testing
│       ├── sample_data_template.csv       # Example CSV data
│       └── sample_boundary_template.zip   # Example shapefile
│
└── instance/                  # Instance-specific files (not in git)
├── uploads/               # User uploaded data
└── reports/               # Generated reports

## Key Features

### Data Processing & Analysis
- **Multi-format Data Support**: Process CSV, Excel, and GIS shapefiles
- **Intelligent Missing Value Handling**: Spatial, mean, mode, and KNN-based imputation methods
- **Variable Relationship Analysis**: Determine direct/inverse relationships with malaria risk
- **Normalization Pipeline**: Scale diverse variables for meaningful comparison
- **Composite Scoring Models**: Generate risk assessments using multiple methodologies
- **Urban Extent Analysis**: Apply configurable urban thresholds for intervention targeting

### Interactive Visualization
- **Variable Distribution Maps**: Explore the geographic distribution of risk factors
- **Normalized Variable Maps**: View standardized risk indicators
- **Composite Risk Maps**: Visualize combined risk assessments
- **Vulnerability Ranking Plots**: Identify priority areas for intervention
- **Decision Tree Visualization**: Understand the analysis workflow
- **Interactive Legends and Tooltips**: Enhance data interpretation

### User Experience
- **Natural Language Interface**: Communicate with the system in plain language
- **Multi-language Support**: Accommodate diverse user populations
- **Guided Analysis Workflow**: Step-by-step assistance through the analysis process
- **Custom Analysis Options**: Select specific variables for targeted risk assessment
- **Comprehensive Reporting**: Generate detailed PDF/HTML reports of findings

## Technical Architecture

### Backend Components
- `__init__.py`: Flask application initialization and configuration
- `routes.py`: API endpoints and request handling
- `models/`:
  - `data_handler.py`: Data loading, validation, and transformation
  - `analysis.py`: Core analytical functions and algorithms
  - `visualization.py`: Plotly-based interactive visualization generation
  - `report_generator.py`: PDF/HTML report compilation

### Frontend Components
- `templates/index.html`: Main application interface
- `static/js/main.js`: Client-side interaction handling
- `static/css/styles.css`: UI styling and responsive design

### Data Flow
1. User uploads data or uses sample datasets
2. System performs data validation and preprocessing
3. AI assistant guides user through analysis options
4. Backend processes data and generates visualizations
5. Results are presented through interactive visualizations
6. User can explore different aspects of the analysis
7. Comprehensive reports can be exported for sharing

## Implementation Details

### Data Processing
The system employs a sophisticated data processing pipeline:
- **Integrity Checking**: Validate data structure and relationships
- **Geospatial Alignment**: Ensure proper coordinate reference systems
- **Ward Name Matching**: Reconcile differences between datasets
- **Missing Value Detection**: Identify and characterize gaps in data
- **Smart Imputation**: Select appropriate methods based on data patterns

### Analytical Methods
Multiple analytical approaches ensure robust assessment:
- **Variable Normalization**: Scale factors to comparable ranges using direct/inverse relationships
- **Spatial Analysis**: Consider geographic relationships in data
- **Combinatorial Modeling**: Evaluate all valid variable combinations
- **Risk Stratification**: Classify areas by vulnerability level
- **Urban Threshold Application**: Apply configurable definitions of urbanicity

## Usage

1. **Data Preparation**:
   - Prepare a CSV/Excel file with ward-level data containing variables like rainfall, temperature, etc.
   - Prepare a shapefile (zipped) containing ward boundaries
   - Ensure both datasets have a "WardName" field for joining

2. **Analysis**:
   - Upload data through the interface or use sample data
   - Follow the AI assistant's guidance to analyze your data
   - Select specific variables for custom analysis if desired
   - Explore different visualizations to understand risk patterns

3. **Interpretation & Export**:
   - Identify highest-priority areas for intervention
   - Generate a comprehensive report for stakeholders
   - Download visualizations for presentations

## Development

### Requirements
- Python 3.8+
- Flask web framework
- GeoPandas for spatial analysis
- Plotly for interactive visualizations
- Other dependencies listed in requirements.txt
