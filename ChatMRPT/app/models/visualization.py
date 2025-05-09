# Import necessary libraries
import json
import logging
from flask import current_app, session # Ensure session is imported if used directly
import os
import numpy as np
import pandas as pd
import geopandas as gpd
import io
import base64
import re
from flask import current_app, session
import plotly.graph_objects as go
import plotly.express as px
from plotly.subplots import make_subplots
import pyproj
from shapely.ops import transform
from functools import partial
from werkzeug.utils import secure_filename # Import secure_filename

# Set up logging
logger = logging.getLogger(__name__)

# Dictionary mapping variable codes to full names
VARIABLE_FULL_NAMES = {
    'tpr': 'Test Positivity Rate',
    'tpr_u5': 'Test Positivity Rate (Under 5)',
    'settlement_type': 'Settlement Type',
    'distance_to_water': 'Distance to Water Bodies',
    'mean_rainfall': 'Mean Rainfall',
    'mean_soil_wetness': 'Mean Soil Wetness',
    'mean_evi': 'Mean Enhanced Vegetation Index',
    'mean_ndvi': 'Mean Normalized Difference Vegetation Index',
    'mean_ndwi': 'Mean Normalized Difference Water Index',
    'pfpr': 'Plasmodium Falciparum Parasite Rate',
    'elevation': 'Elevation',
    'population': 'Population',
    'housing_quality': 'Housing Quality',
    'temp_mean': 'Mean Temperature',
    'rh_mean': 'Mean Relative Humidity',
    'flood': 'Flood Risk',
    'urbanpercent': 'Urban Percentage',
    'urbanarea': 'Urban Area',
    'avgrad': 'Average Radiation',
    'precipitation': 'Precipitation',
    'rainfall': 'Rainfall',
    'temp': 'Temperature',
    'temperature': 'Temperature',
    'soil_wetness': 'Soil Wetness',
    'evi': 'Enhanced Vegetation Index',
    'ndvi': 'Normalized Difference Vegetation Index',
    'ndwi': 'Normalized Difference Water Index',
}

def get_full_variable_name(var_code):
    """
    Get the full descriptive name for a variable code
    
    Args:
        var_code: Variable code/short name
        
    Returns:
        str: Full descriptive name
    """
    # Standardize the variable code (lowercase)
    var_code_lower = var_code.lower() if var_code else ""
    
    # Check the dictionary first with exact match
    if var_code_lower in VARIABLE_FULL_NAMES:
        return VARIABLE_FULL_NAMES[var_code_lower]
    
    # Try matching with standardized keys (without underscores, etc.)
    normalized_inputs = {re.sub(r'[_\s]', '', k.lower()): v for k, v in VARIABLE_FULL_NAMES.items()}
    normalized_var = re.sub(r'[_\s]', '', var_code_lower)
    
    if normalized_var in normalized_inputs:
        return normalized_inputs[normalized_var]
    
    # If still not found, check for partial matches
    for key, value in VARIABLE_FULL_NAMES.items():
        if key in var_code_lower or var_code_lower in key:
            return value
    
    # If not found, try some heuristics to make the name more readable
    if '_' in var_code:
        # Split by underscore and capitalize each word
        parts = var_code.split('_')
        return ' '.join(word.capitalize() for word in parts)
    
    # Just capitalize the first letter as fallback
    return var_code.capitalize()

def ensure_wgs84_crs(gdf):
    """
    Ensure the GeoDataFrame is using WGS84 (EPSG:4326) CRS
    
    Args:
        gdf: GeoDataFrame to check/transform
        
    Returns:
        GeoDataFrame in WGS84 CRS
    """
    # Create a copy to avoid modifying the original
    gdf_copy = gdf.copy()
    
    # Check if the GeoDataFrame has a CRS
    if gdf_copy.crs is None:
        logger.warning("GeoDataFrame has no CRS. Assuming WGS84.")
        gdf_copy.set_crs(epsg=4326, inplace=True)
        return gdf_copy
    
    # Check if the CRS is already WGS84
    if gdf_copy.crs == "EPSG:4326" or gdf_copy.crs == 4326:
        return gdf_copy
    
    try:
        # Log the transformation
        logger.info(f"Transforming GeoDataFrame from {gdf_copy.crs} to WGS84 (EPSG:4326)")
        
        # Reproject to WGS84
        gdf_copy = gdf_copy.to_crs(epsg=4326)
        return gdf_copy
    except Exception as e:
        logger.error(f"Error transforming CRS: {str(e)}")
        # Return original if transformation fails
        return gdf

def prepare_geodataframe_for_json(gdf):
    """
    Prepare a GeoDataFrame for JSON serialization by converting non-serializable types
    
    Args:
        gdf: GeoDataFrame to prepare
        
    Returns:
        GeoDataFrame with serializable types
    """
    # Create a copy to avoid modifying the original
    gdf_copy = gdf.copy()
    
    # Convert any datetime/timestamp columns to strings
    for col in gdf_copy.columns:
        if pd.api.types.is_datetime64_any_dtype(gdf_copy[col]):
            gdf_copy[col] = gdf_copy[col].astype(str)
    
    return gdf_copy

def create_plotly_html(fig, filename, include_plotlyjs='cdn'):
    """
    Convert plotly figure to HTML file, saving to the INSTANCE path's upload folder.

    Args:
        fig: Plotly figure object
        filename: Desired output filename (will be secured)
        include_plotlyjs: How to include plotly.js ('cdn' or True for full)

    Returns:
        str: Web-accessible path using the /serve_viz_file/ route, or None on error.
    """
    if not filename:
        # Generate a random filename if none provided
        safe_filename = f"plotly_{np.random.randint(1000000)}.html"
    else:
        # Ensure the provided filename is web-safe and has .html extension
        safe_filename = secure_filename(filename)
        if not safe_filename.endswith('.html'):
             safe_filename += '.html'

    session_id = session.get('session_id', 'default')

    # --- THIS IS THE CRUCIAL PART ---
    # Get the UPLOAD_FOLDER path (which should point to instance/uploads) from the app config
    upload_dir = current_app.config.get('UPLOAD_FOLDER')
    if not upload_dir:
        logger.error("UPLOAD_FOLDER not configured in Flask app config.")
        return None # Cannot save without upload folder config

    # Define the specific session folder path ON DISK within the configured UPLOAD_FOLDER
    session_folder_disk = os.path.join(upload_dir, session_id)
    # ================================

    # Ensure the target directory exists
    try:
        os.makedirs(session_folder_disk, exist_ok=True)
    except OSError as e:
        logger.error(f"Could not create session upload directory {session_folder_disk}: {e}")
        return None

    # Define the full path to the file on disk
    file_path_disk = os.path.join(session_folder_disk, safe_filename)

    config = {
        'responsive': True,
        'displayModeBar': True,
        'scrollZoom': True
    }

    # Write HTML file to the correct disk path
    try:
        fig.write_html(file_path_disk, include_plotlyjs=include_plotlyjs, full_html=True, config=config)
        logger.info(f"Successfully saved visualization to disk: {file_path_disk}")
    except Exception as e:
        logger.error(f"Failed to write Plotly HTML to {file_path_disk}: {e}")
        return None # Indicate failure

    # Return the web-accessible path using the dedicated route /serve_viz_file/
    # This URL tells the browser where to REQUEST the file from the server.
    web_path = f"/serve_viz_file/{session_id}/{safe_filename}"
    logger.info(f"Returning web path for visualization: {web_path}")
    return web_path

def is_id_column(column_name):
    """
    Check if a column name appears to be an ID or placeholder column
    
    Args:
        column_name: Name of the column to check
        
    Returns:
        bool: True if it appears to be an ID column
    """
    id_patterns = ['id', 'x.1', 'x', 'index', 'lga_code', 'wardid', 'ward_id']
    column_lower = column_name.lower()
    
    # Check if it matches common ID patterns
    for pattern in id_patterns:
        if pattern == column_lower or f"{pattern}_" in column_lower:
            return True
    
    return False

def get_variable_by_name(data_handler, variable_name):
    """
    Get the actual variable name that best matches the requested name
    
    Args:
        data_handler: DataHandler instance
        variable_name: Requested variable name (may be inexact)
        
    Returns:
        str: Best matching variable name, or None if not found
    """
    # IMPORTANT: Always allow access to all variables in the CSV data, not just cleaned or analysis variables
    if data_handler.csv_data is None:
        return None
    
    if not variable_name:
        logger.warning("No variable name provided")
        return None
    
    # Convert to lowercase for case-insensitive matching
    variable_lower = variable_name.lower()
    
    # Clean up the variable name (remove articles, common words)
    clean_variable = re.sub(r'\b(the|of|for|in|a|an)\b', '', variable_lower).strip()
    clean_variable = re.sub(r'\s+', ' ', clean_variable)
    
    # Get all column names from the original CSV data
    columns = list(data_handler.csv_data.columns)
    
    # Try exact match first
    for col in columns:
        if col.lower() == variable_lower or col.lower() == clean_variable:
            # Check if it's not an ID column
            if not is_id_column(col) and col != 'WardName':
                logger.info(f"Found exact match: {col} for {variable_name}")
                return col
    
    # Known variable name mappings and common variations
    variable_mappings = {
        'rainfall': ['rain', 'precipitation', 'precip', 'mean_rainfall', 'rainfall'],
        'temperature': ['temp', 'temperature', 'climate', 'mean_temperature', 'temp_mean'],
        'elevation': ['elev', 'altitude', 'height', 'dem'],
        'population': ['pop', 'people', 'inhabitants', 'population_density'],
        'distance_to_water': ['distance', 'dist', 'proximity', 'water_dist'],
        'housing_quality': ['house', 'dwelling', 'home', 'housing', 'building'],
        'ndvi': ['ndvi', 'vegetation', 'greenness', 'mean_ndvi'],
        'evi': ['evi', 'enhanced', 'mean_evi'],
        'mean_soil_wetness': ['soil', 'wetness', 'moisture', 'soil_wetness'],
        'flood': ['flood', 'inundation', 'water_extent'],
        'water': ['water', 'hydro', 'hydrologic'],
        'urban': ['urban', 'built', 'city']
    }
    
    # Check for matches using the mappings
    for standard_name, variants in variable_mappings.items():
        if any(variant in variable_lower for variant in variants):
            # Look for columns that match this variable
            for col in columns:
                col_lower = col.lower()
                if any(variant in col_lower for variant in variants):
                    if not is_id_column(col) and col != 'WardName':
                        logger.info(f"Found mapped match: {col} for {variable_name} via {standard_name}")
                        return col
    
    # Try partial match as fallback
    for col in columns:
        col_lower = col.lower()
        if (variable_lower in col_lower or clean_variable in col_lower or 
            any(term in col_lower for term in variable_lower.split())):
            if not is_id_column(col) and col != 'WardName':
                logger.info(f"Found partial match: {col} for {variable_name}")
                return col
    
    # Last resort: return first numeric column that's not an ID
    for col in columns:
        if col != 'WardName' and not is_id_column(col) and pd.api.types.is_numeric_dtype(data_handler.csv_data[col]):
            logger.warning(f"No match found for {variable_name}, using {col} as fallback")
            return col
            
    logger.error(f"Could not find any suitable variable match for {variable_name}")
    return None

def create_variable_map(data_handler, variable_name=None):
    """
    Create a map visualizing a variable's distribution
    
    Args:
        data_handler: DataHandler instance
        variable_name: Name of the variable to visualize
        
    Returns:
        dict: Status and visualization information
    """
    try:
        # Check if shapefile data is available
        if data_handler.shapefile_data is None:
            return {
                'status': 'error',
                'message': 'Shapefile data not loaded'
            }
        
        # If no variable specified, pick the first suitable variable
        if variable_name is None:
            if data_handler.csv_data is not None:
                var_columns = [col for col in data_handler.csv_data.columns 
                            if col != 'WardName' and not is_id_column(col)]
                if var_columns:
                    variable_name = var_columns[0]
        
        # Find the best matching variable - IMPORTANT: Always allow access to all variables in CSV data
        actual_variable = get_variable_by_name(data_handler, variable_name)
        
        if not actual_variable:
            available_vars = []
            if data_handler.csv_data is not None:
                available_vars = [col for col in data_handler.csv_data.columns 
                               if col != 'WardName' and pd.api.types.is_numeric_dtype(data_handler.csv_data[col]) 
                               and not is_id_column(col)]
            
            return {
                'status': 'error',
                'message': f'Variable similar to "{variable_name}" not found in data',
                'available_variables': available_vars
            }
        
        # Check if this variable has missing values that were cleaned
        has_missing = False
        missing_count = 0
        
        # Use csv_data for original values
        df = data_handler.csv_data
        
        if actual_variable in df.columns:
            missing_count = df[actual_variable].isna().sum()
            has_missing = missing_count > 0
        
        # Get full variable name for display
        full_variable_name = get_full_variable_name(actual_variable)
        
        # Get a copy of the shapefile with standardized CRS
        shapefile_data = ensure_wgs84_crs(data_handler.shapefile_data)
        
        # If we have missing values and cleaned data, show both maps
        if has_missing and data_handler.cleaned_data is not None:
            # Create figure with two subplots side by side
            fig = make_subplots(
                rows=1, cols=2,
                specs=[[{"type": "mapbox"}, {"type": "mapbox"}]],
                subplot_titles=[f"Original Data ({missing_count} missing values)", "Cleaned Data"],
                horizontal_spacing=0.02
            )
            
            # 1. Original data map
            # Create combined dataframe for plotting
            gdf_original = shapefile_data.merge(df[['WardName', actual_variable]], on='WardName', how='left')
            
            # Convert geometry to geojson with proper serialization
            gdf_prepared = prepare_geodataframe_for_json(gdf_original)
            geojson = json.loads(gdf_prepared.to_json())
            
            # Add choropleth for original data
            fig.add_trace(
                go.Choroplethmapbox(
                    geojson=geojson,
                    locations=gdf_original.index,
                    z=gdf_original[actual_variable],
                    colorscale='Blues',
                    marker_opacity=0.8,
                    marker_line_width=0.5,
                    marker_line_color='black',
                    hovertemplate='<b>%{customdata}</b><br>' + f'{full_variable_name}: ' + '%{z:.2f}<extra></extra>',
                    customdata=gdf_original['WardName'],
                    showscale=False
                ),
                row=1, col=1
            )
            
            # 2. Cleaned data map
            # Create combined dataframe for plotting
            gdf_cleaned = shapefile_data.merge(data_handler.cleaned_data[['WardName', actual_variable]], on='WardName', how='left')
            
            # Convert geometry to geojson with proper serialization
            gdf_prepared = prepare_geodataframe_for_json(gdf_cleaned)
            geojson = json.loads(gdf_prepared.to_json())
            
            # Add choropleth for cleaned data
            fig.add_trace(
                go.Choroplethmapbox(
                    geojson=geojson,
                    locations=gdf_cleaned.index,
                    z=gdf_cleaned[actual_variable],
                    colorscale='Blues',
                    marker_opacity=0.8,
                    marker_line_width=0.5,
                    marker_line_color='black',
                    hovertemplate='<b>%{customdata}</b><br>' + f'{full_variable_name}: ' + '%{z:.2f}<extra></extra>',
                    customdata=gdf_cleaned['WardName'],
                    colorbar=dict(
                        title=dict(
                            text=full_variable_name,
                            font=dict(size=12)
                        )
                    )
                ),
                row=1, col=2
            )
            
            # Get proper map centering
            center_lat = gdf_original.geometry.centroid.y.mean()
            center_lon = gdf_original.geometry.centroid.x.mean()
            
            # Calculate appropriate zoom level based on the bounding box
            bounds = gdf_original.geometry.total_bounds  # minx, miny, maxx, maxy
            span_x = max(0.01, bounds[2] - bounds[0])  # Ensure minimum span to avoid zoom errors
            span_y = max(0.01, bounds[3] - bounds[1])
            
            # Calculate zoom level - ensure it's reasonable
            zoom_level = min(10, max(4, 6 - np.log(max(span_x, span_y))))
            
            # Update mapbox settings for both subplots
            fig.update_mapboxes(
                style="carto-positron",
                center={"lat": center_lat, "lon": center_lon},
                zoom=zoom_level
            )
        
        else:
            # Single map - just show the data we have
            fig = go.Figure()
            
            # Use cleaned data if available and the variable exists there, otherwise use original
            if data_handler.cleaned_data is not None and actual_variable in data_handler.cleaned_data.columns:
                df_to_use = data_handler.cleaned_data
            else:
                df_to_use = df
            
            # Create combined dataframe for plotting
            gdf = shapefile_data.merge(df_to_use[['WardName', actual_variable]], on='WardName', how='left')
            
            # Convert geometry to geojson with proper serialization
            gdf_prepared = prepare_geodataframe_for_json(gdf)
            geojson = json.loads(gdf_prepared.to_json())
            
            # Get proper map centering
            center_lat = gdf.geometry.centroid.y.mean()
            center_lon = gdf.geometry.centroid.x.mean()
            
            # Calculate appropriate zoom level based on the bounding box
            bounds = gdf.geometry.total_bounds  # minx, miny, maxx, maxy
            span_x = max(0.01, bounds[2] - bounds[0])  # Ensure minimum span to avoid zoom errors
            span_y = max(0.01, bounds[3] - bounds[1])
            
            # Calculate zoom level - ensure it's reasonable
            zoom_level = min(10, max(4, 6 - np.log(max(span_x, span_y))))
            
            # Add choropleth
            fig.add_trace(
                go.Choroplethmapbox(
                    geojson=geojson,
                    locations=gdf.index,
                    z=gdf[actual_variable],
                    colorscale='Blues',
                    marker_opacity=0.8,
                    marker_line_width=0.5,
                    marker_line_color='black',
                    hovertemplate='<b>%{customdata}</b><br>' + f'{full_variable_name}: ' + '%{z:.2f}<extra></extra>',
                    customdata=gdf['WardName'],
                    colorbar=dict(
                        title=dict(
                            text=full_variable_name,
                            font=dict(size=12)
                        )
                    )
                )
            )
            
            # Update mapbox settings
            fig.update_layout(
                mapbox=dict(
                    style="carto-positron",
                    center={"lat": center_lat, "lon": center_lon},
                    zoom=zoom_level
                )
            )
        
        # Update overall layout
        fig.update_layout(
            title={
                'text': f"Distribution of {full_variable_name}",
                'x': 0.5,
                'xanchor': 'center',
                'font': {'size': 20}
            },
            height=480,  # Reduced height for better fit
            width=800,   # Reduced width for better fit
            margin=dict(l=20, r=20, t=80, b=20),  # Adjusted margins
            autosize=True  # Enable autosize for responsiveness
        )
        
        # Create HTML file
        html_path = create_plotly_html(fig, f"variable_map_{actual_variable}.html")
        
        # Return success with paths and metadata
        message = "Missing values detected and cleaned." if has_missing else "No missing values detected."
        
        return {
            'status': 'success',
            'message': f'Successfully created map for {full_variable_name}. {message}',
            'image_path': html_path,
            'variable': actual_variable,
            'full_variable_name': full_variable_name,
            'missing_values': missing_count,
            'viz_type': 'variable_map',
            'ai_response': f"Here's the distribution map for {full_variable_name}. {message} The darker blue areas indicate higher values, while lighter areas show lower values. This map gives you a geographical view of how {full_variable_name} varies across the region."
        }
        
    except Exception as e:
        logger.error(f"Error creating variable map: {str(e)}")
        import traceback
        logger.error(traceback.format_exc())
        return {
            'status': 'error',
            'message': f'Error creating variable map: {str(e)}'
        }

def create_normalized_map(data_handler, variable_name=None):
    """
    Create a map visualizing a normalized variable
    
    Args:
        data_handler: DataHandler instance
        variable_name: Name of the variable to visualize
        
    Returns:
        dict: Status and visualization information
    """
    try:
        # If no variable specified, pick the first suitable variable
        if variable_name is None:
            if data_handler.normalized_data is not None:
                norm_cols = [col for col in data_handler.normalized_data.columns 
                            if col.startswith('normalization_')]
                if norm_cols:
                    variable_name = norm_cols[0].replace('normalization_', '')
            elif data_handler.cleaned_data is not None:
                var_columns = [col for col in data_handler.cleaned_data.columns 
                            if col != 'WardName' and not is_id_column(col)]
                if var_columns:
                    variable_name = var_columns[0]
            elif data_handler.csv_data is not None:
                var_columns = [col for col in data_handler.csv_data.columns 
                            if col != 'WardName' and not is_id_column(col)]
                if var_columns:
                    variable_name = var_columns[0]
        
        # Check if normalized data is available
        if data_handler.normalized_data is None:
            # Try to normalize the data now
            try:
                norm_result = data_handler.normalize_data()
                if norm_result['status'] != 'success':
                    return {
                        'status': 'error',
                        'message': 'Could not normalize data: ' + norm_result.get('message', 'Unknown error')
                    }
            except Exception as e:
                return {
                    'status': 'error',
                    'message': f'Normalized data not available. Error: {str(e)}'
                }
        
        if data_handler.shapefile_data is None:
            return {
                'status': 'error',
                'message': 'Shapefile data not loaded'
            }
        
        # Find the best matching variable
        actual_variable = get_variable_by_name(data_handler, variable_name)
        
        if not actual_variable:
            # Try to check available normalized columns
            norm_vars = []
            if data_handler.normalized_data is not None:
                norm_vars = [col.replace('normalization_', '') for col in data_handler.normalized_data.columns 
                           if col.startswith('normalization_')]
            
            # If no match but we do have normalized variables, use the first one
            if norm_vars:
                actual_variable = norm_vars[0]
            else:
                available_vars = []
                if data_handler.csv_data is not None:
                    available_vars = [col for col in data_handler.csv_data.columns 
                                   if col != 'WardName' and pd.api.types.is_numeric_dtype(data_handler.csv_data[col]) 
                                   and not is_id_column(col)]
                
                return {
                    'status': 'error',
                    'message': f'Variable similar to "{variable_name}" not found and no normalized variables available',
                    'available_variables': available_vars
                }
        
        # Normalized column name
        norm_col = f"normalization_{actual_variable.lower()}"
        
        # Check if the normalized column exists
        if norm_col not in data_handler.normalized_data.columns:
            # Try to find a similar normalized column
            all_norm_cols = [col for col in data_handler.normalized_data.columns if col.startswith('normalization_')]
            
            # Try direct match with variable name (case insensitive)
            similar_cols = [col for col in all_norm_cols 
                          if actual_variable.lower() == col.replace('normalization_', '').lower()]
            
            # If no direct match, try partial match
            if not similar_cols:
                similar_cols = [col for col in all_norm_cols 
                              if actual_variable.lower() in col.replace('normalization_', '').lower()]
            
            if similar_cols:
                norm_col = similar_cols[0]
                # Extract original variable name from normalized column name
                actual_variable = norm_col.replace('normalization_', '')
                logger.info(f"Found normalized column '{norm_col}' for variable '{variable_name}'")
            else:
                # If we can't find it, normalize it now
                try:
                    # Determine relationship
                    relationship = 'direct'
                    if hasattr(data_handler, 'variable_relationships') and actual_variable in data_handler.variable_relationships:
                        relationship = data_handler.variable_relationships[actual_variable]
                    
                    # Normalize this specific variable
                    if data_handler.cleaned_data is not None and actual_variable in data_handler.cleaned_data.columns:
                        values = data_handler.cleaned_data[actual_variable].values
                        min_val = np.min(values)
                        max_val = np.max(values)
                        
                        if relationship == 'inverse':
                            inverted = 1 / (values + 1e-10)
                            inv_min = np.min(inverted)
                            inv_max = np.max(inverted)
                            normalized = (inverted - inv_min) / (inv_max - inv_min)
                        else:
                            normalized = (values - min_val) / (max_val - min_val)
                        
                        # Add to normalized data
                        if data_handler.normalized_data is None:
                            data_handler.normalized_data = data_handler.cleaned_data[['WardName']].copy()
                        
                        norm_col = f"normalization_{actual_variable.lower()}"
                        data_handler.normalized_data[norm_col] = normalized
                        logger.info(f"Created normalized column '{norm_col}' for variable '{actual_variable}'")
                    else:
                        return {
                            'status': 'error',
                            'message': f'Variable {actual_variable} not found in cleaned data'
                        }
                except Exception as e:
                    return {
                        'status': 'error',
                        'message': f'Error normalizing variable {actual_variable}: {str(e)}'
                    }
        
        # Double-check that the normalized column exists now
        if norm_col not in data_handler.normalized_data.columns:
            return {
                'status': 'error', 
                'message': f'Failed to create normalized column for {actual_variable}'
            }
        
        # Get variable relationship
        relationship = 'direct'
        if hasattr(data_handler, 'variable_relationships') and actual_variable in data_handler.variable_relationships:
            relationship = data_handler.variable_relationships[actual_variable]
        
        # Get full variable name for display
        full_variable_name = get_full_variable_name(actual_variable)
        
        # Get a copy of the shapefile with standardized CRS
        shapefile_data = ensure_wgs84_crs(data_handler.shapefile_data)
        
        # Create combined dataframe for plotting
        gdf = shapefile_data.merge(
            data_handler.normalized_data[['WardName', norm_col]], 
            on='WardName', 
            how='left'
        )
        
        # Convert geometry to geojson with proper serialization
        gdf_prepared = prepare_geodataframe_for_json(gdf)
        geojson = json.loads(gdf_prepared.to_json())
        
        # Get proper map centering
        center_lat = gdf.geometry.centroid.y.mean()
        center_lon = gdf.geometry.centroid.x.mean()
        
        # Calculate appropriate zoom level based on the bounding box
        bounds = gdf.geometry.total_bounds  # minx, miny, maxx, maxy
        span_x = max(0.01, bounds[2] - bounds[0])  # Ensure minimum span to avoid zoom errors
        span_y = max(0.01, bounds[3] - bounds[1])
        
        # Calculate zoom level - ensure it's reasonable
        zoom_level = min(10, max(4, 6 - np.log(max(span_x, span_y))))
        
        # Create choropleth map with Plotly
        fig = go.Figure()
        
        fig.add_trace(go.Choroplethmapbox(
            geojson=geojson,
            locations=gdf.index,
            z=gdf[norm_col],
            colorscale='YlOrRd',
            marker_opacity=0.8,
            marker_line_width=0.5,
            marker_line_color='black',
            hovertemplate='<b>%{customdata}</b><br>Normalized Value: %{z:.3f}<extra></extra>',
            customdata=gdf['WardName'],
            zmin=0,
            zmax=1,
            colorbar=dict(
                title=dict(
                    text='Risk Contribution' if relationship == 'direct' else 'Risk Contribution (Inverted)',
                    font=dict(size=12)
                ),
                tickvals=[0, 0.25, 0.5, 0.75, 1],
                ticktext=['Very Low', 'Low', 'Medium', 'High', 'Very High']
            )
        ))
        
        # Update layout
        fig.update_layout(
            title={
                'text': f"Normalized {full_variable_name} ({relationship} relationship)",
                'x': 0.5,
                'xanchor': 'center',
                'font': {'size': 20}
            },
            mapbox=dict(
                style="carto-positron",
                center={"lat": center_lat, "lon": center_lon},
                zoom=zoom_level
            ),
            height=480,  # Reduced height
            width=800,   # Reduced width
            margin=dict(l=20, r=20, t=80, b=20),  # Adjusted margins
            autosize=True  # Enable autosize for responsiveness
        )
        
        # Create HTML file
        html_path = create_plotly_html(fig, f"normalized_map_{actual_variable}.html")
        
        relationship_explanation = (
            "higher values correspond to higher malaria risk" 
            if relationship == "direct" 
            else "higher values correspond to lower malaria risk (the relationship is inverted)"
        )
        
        # Return success with paths and metadata
        return {
            'status': 'success',
            'message': f'Successfully created normalized map for {full_variable_name}',
            'image_path': html_path,
            'variable': actual_variable,
            'full_variable_name': full_variable_name,
            'relationship': relationship,
            'viz_type': 'normalized_map',
            'ai_response': f"Here's the normalized map for {full_variable_name}, showing its {relationship} relationship with malaria risk. The color scale ranges from yellow (lower risk contribution) to dark red (higher risk contribution). For this variable, {relationship_explanation}."
        }
        
    except Exception as e:
            logger.error(f"Error creating normalized map: {str(e)}")
            import traceback
            logger.error(traceback.format_exc())
            return {
                'status': 'error',
                'message': f'Error creating normalized map: {str(e)}'
            }

def create_composite_map(data_handler, model_index=None):
    """
    Create composite risk score maps
    
    Args:
        data_handler: DataHandler instance
        model_index: Index of the model/page to visualize (None for first page)
        
    Returns:
        dict: Status and visualization information
    """
    try:
        # Check if composite scores are available
        if not hasattr(data_handler, 'composite_scores') or data_handler.composite_scores is None:
            # Try to reload from saved files
            try:
                scores_path = os.path.join(data_handler.session_folder, 'composite_scores.csv')
                formulas_path = os.path.join(data_handler.session_folder, 'model_formulas.csv')
                
                if os.path.exists(scores_path) and os.path.exists(formulas_path):
                    scores_df = pd.read_csv(scores_path)
                    formulas_df = pd.read_csv(formulas_path)
                    
                    # Recreate composite_scores structure
                    data_handler.composite_scores = {
                        'scores': scores_df,
                        'model_formulas': []
                    }
                    
                    # Convert formulas DataFrame to list of dicts
                    for _, row in formulas_df.iterrows():
                        formula_dict = {
                            'model': row['model'],
                            'variables': row['variables'].split(',') if isinstance(row['variables'], str) else []
                        }
                        data_handler.composite_scores['model_formulas'].append(formula_dict)
                else:
                    return {
                        'status': 'error',
                        'message': 'Composite scores not available. Calculate composite scores first.'
                    }
            except Exception as e:
                return {
                    'status': 'error',
                    'message': f'Error loading composite scores: {str(e)}'
                }
        
        if data_handler.shapefile_data is None:
            return {
                'status': 'error',
                'message': 'Shapefile data not loaded'
            }
        
        # Get all model columns
        model_columns = [col for col in data_handler.composite_scores['scores'].columns if col.startswith('model_')]
        model_formulas = data_handler.composite_scores['model_formulas']
        
        # Determine number of models and pages
        n_models = len(model_columns)
        models_per_page = 4
        n_pages = (n_models + models_per_page - 1) // models_per_page
        
        # If model_index is a number, treat it as a page number
        page = 1
        if isinstance(model_index, int) or isinstance(model_index, float) or (isinstance(model_index, str) and model_index.isdigit()):
            page = int(model_index)
            # Ensure page is within bounds
            page = max(1, min(page, n_pages))
        
        # Calculate start and end indices for this page
        start_idx = (page - 1) * models_per_page
        end_idx = min(start_idx + models_per_page, n_models)
        
        # Get models for this page
        page_models = model_columns[start_idx:end_idx]
        page_formulas = model_formulas[start_idx:end_idx]
        
        # Get a copy of the shapefile with standardized CRS
        shapefile_data = ensure_wgs84_crs(data_handler.shapefile_data)
        
        # Check if the shapefile has an Urban column to identify non-urban wards
        urban_column = None
        for col in ['Urban', 'urban', 'URBAN', 'UrbanStatus']:
            if col in shapefile_data.columns:
                urban_column = col
                break
        
        # Combine with shapefile
        gdf = shapefile_data.merge(
            data_handler.composite_scores['scores'],
            on='WardName',
            how='left'
        )
        
        # If we have an Urban column, identify "Not Ideal" models
        not_ideal_models = {}
        if urban_column is not None:
            # For each model, check if non-urban wards (Urban="No") are in the top 5 for vulnerability
            for model in model_columns:
                # Sort wards by model score (descending) to find top 5
                top_wards = gdf.sort_values(model, ascending=False).head(5)
                
                # Check if any of these wards are non-urban
                non_urban_top_wards = top_wards[top_wards[urban_column].str.lower().isin(['no', 'false', '0', 'n'])]
                
                # If there are non-urban wards in top 5, flag this model as "Not Ideal"
                if len(non_urban_top_wards) > 0:
                    not_ideal_models[model] = non_urban_top_wards['WardName'].tolist()
        
        # Convert geometry to geojson with proper serialization
        gdf_prepared = prepare_geodataframe_for_json(gdf)
        geojson = json.loads(gdf_prepared.to_json())
        
        # Get proper map centering
        center_lat = gdf.geometry.centroid.y.mean()
        center_lon = gdf.geometry.centroid.x.mean()
        
        # Calculate appropriate zoom level based on the bounding box
        bounds = gdf.geometry.total_bounds  # minx, miny, maxx, maxy
        span_x = max(0.01, bounds[2] - bounds[0])  # Ensure minimum span to avoid zoom errors
        span_y = max(0.01, bounds[3] - bounds[1])
        
        # Calculate zoom level - ensure it's reasonable
        zoom_level = min(10, max(4, 6 - np.log(max(span_x, span_y))))
        
        # Determine grid layout for subplots
        if len(page_models) == 1:
            rows, cols = 1, 1
        elif len(page_models) == 2:
            rows, cols = 1, 2
        else:
            rows, cols = 2, 2
        
        # Create subplot titles with variables on separate lines
        subplot_titles = []
        for model, formula in zip(page_models, page_formulas):
            # Get variables
            variables = formula['variables']
            
            # Check if we have any variables
            if variables and len(variables) > 0:
                # Create title with variables on separate lines
                var_names = []
                for var in variables:
                    # Get full name if available
                    var_name = get_full_variable_name(var.lower())
                    var_names.append(var_name)
                
                # Join with line breaks
                title = "<br>".join(var_names)
                
                # Add "Not Ideal" designation if this model is flagged
                if model in not_ideal_models:
                    title = f"{title}<br><span class='not-ideal-label'>(Not Ideal)</span>"
            else:
                # Fallback if no variables
                title = f"{model.replace('model_', 'Model ')}"
                if model in not_ideal_models:
                    title = f"{title}<br><span class='not-ideal-label'>(Not Ideal)</span>"
                
            subplot_titles.append(title)
        
        # Create subplots
        fig = make_subplots(
            rows=rows,
            cols=cols,
            specs=[[{"type": "mapbox"}] * cols for _ in range(rows)],
            subplot_titles=subplot_titles,
            vertical_spacing=0.22,  # Increased vertical spacing significantly
            horizontal_spacing=0.05 # Can adjust this too if needed
        )
        
        # Add choropleth for each model
        for idx, model in enumerate(page_models):
            row = idx // cols + 1
            col = idx % cols + 1
            
            # Add choropleth trace for the model
            fig.add_trace(
                go.Choroplethmapbox(
                    geojson=geojson,
                    locations=gdf.index,
                    z=gdf[model],
                    colorscale='YlOrRd',
                    marker_line_color='black',
                    marker_line_width=0.5,
                    showscale=(idx == 0),  # Only show scale for first plot
                    colorbar=dict(
                        title=dict(
                            text="Risk Score",
                            font=dict(size=12)
                        ),
                        tickvals=[0, 0.25, 0.5, 0.75, 1],  # Five tick values
                        ticktext=["Very Low", "Low", "Medium", "High", "Very High"]  # Five labels
                    ) if idx == 0 else None,
                    hovertemplate='<b>%{customdata}</b><br>Risk Score: %{z:.3f}<extra></extra>',
                    customdata=gdf['WardName'],
                    zmin=0,
                    zmax=1
                ),
                row=row, col=col
            )
            
            # If this model is flagged as "Not Ideal", add blue outlines to the non-urban wards
            if model in not_ideal_models and urban_column is not None:
                # Get non-urban wards in the top 5
                non_urban_wards = not_ideal_models[model]
                
                # Create mask for these wards
                ward_mask = gdf['WardName'].isin(non_urban_wards)
                
                # Add a separate trace with blue outlines for these wards
                if any(ward_mask):
                    fig.add_trace(
                        go.Choroplethmapbox(
                            geojson=geojson,
                            locations=gdf[ward_mask].index,
                            z=gdf[ward_mask][model],
                            colorscale='YlOrRd',
                            marker_line_color='blue',
                            marker_line_width=3,
                            showscale=False,
                            hovertemplate='<b>%{customdata}</b><br>Risk Score: %{z:.3f}<br><span style="color:blue;">Non-Urban Ward</span><extra></extra>',
                            customdata=gdf[ward_mask]['WardName'],
                            zmin=0,
                            zmax=1
                        ),
                        row=row, col=col
                    )
        
        # Update mapbox settings for each subplot
        for i in range(1, rows * cols + 1):
            if i <= len(page_models):
                fig.update_mapboxes(
                    style="carto-positron",
                    center={"lat": center_lat, "lon": center_lon},
                    zoom=zoom_level,
                    row=((i-1)//cols)+1, col=((i-1)%cols)+1
                )
        
        # Update overall layout - ensuring title doesn't overlap with subplot titles
        fig.update_layout(
            title={
                'text': f"Composite Score Distribution by Model<br><span style='font-size:16px'>Page {page} of {n_pages}</span>",
                'x': 0.5,
                'xanchor': 'center',
                'font': {'size': 18}, # Slightly smaller main title
                'y': 0.97,  # Adjusted Y position for title
                'yanchor': 'top'
            },
            height=600, # Increased height slightly for 2x2 plots
            width=800,  # Can adjust based on modal size preference
            margin=dict(t=100, b=60, l=50, r=50),  # Increased top margin, adjusted others
            # vertical_spacing=0.18, # Already set in make_subplots, ensure it's enough
            # horizontal_spacing=0.08, # Already set in make_subplots
            autosize=True # Let Plotly try to size within the iframe
        )
        
        # Add a caption explaining the "Not Ideal" designation
        if any(model in not_ideal_models for model in page_models):
            fig.add_annotation(
                x=0.5,
                y=-0.05,
                xref="paper",
                yref="paper",
                text="Blue outlines indicate non-urban wards ranked in top 5 for vulnerability (not ideal for prioritization)",
                showarrow=False,
                font=dict(size=12, color="blue"),
                align="center"
            )
        
        # Create HTML file
        html_path = create_plotly_html(fig, f"composite_map_page{page}.html")
        
        # Get list of unique variables used across models
        all_variables = set()
        for formula in page_formulas:
            for variable in formula['variables']:
                all_variables.add(variable)
        
        # Create comma-separated list of full variable names
        full_var_names = [get_full_variable_name(var) for var in sorted(list(all_variables))]
        variables_text = ", ".join(full_var_names)
        
        # Create info about "Not Ideal" models on this page
        not_ideal_text = ""
        not_ideal_count = sum(1 for model in page_models if model in not_ideal_models)
        if not_ideal_count > 0:
            not_ideal_text = f" {not_ideal_count} model(s) are marked 'Not Ideal' because they rank non-urban wards in the top 5 for vulnerability, which may present logistical challenges for interventions."
        
        # Return success with pagination info
        return {
            'status': 'success',
            'message': f'Successfully created composite risk maps (page {page} of {n_pages})',
            'image_path': html_path,
            'current_page': page,
            'total_pages': n_pages,
            'viz_type': 'composite_map',
            'ai_response': f"Here are the composite risk maps (page {page} of {n_pages}) showing malaria risk scores based on different combinations of variables. The color scale ranges from yellow (very low risk) to dark red (very high risk).{not_ideal_text} These maps combine the following variables in different ways: {variables_text}."
        }
        
    except Exception as e:
        logger.error(f"Error creating composite maps: {str(e)}")
        import traceback
        logger.error(traceback.format_exc())
        return {
            'status': 'error',
            'message': f'Error creating composite maps: {str(e)}'
        }

def box_plot_function(processed_scores, wards_per_page=20):
    """
    Create paginated box plots of ward rankings
    
    Args:
        processed_scores: DataFrame with processed model scores data
        wards_per_page: Number of wards to display per page (default: 20)
        
    Returns:
        Dict with plotly objects for each page and ward rankings
    """
    try:
        # Create a copy to avoid modifying original
        df_long = processed_scores.copy()
        
        # Get model columns (starting with 'model_')
        model_cols = [col for col in df_long.columns if col.startswith('model_')]
        
        if not model_cols:
            return {
                'status': 'error',
                'message': 'No model scores found in data'
            }
        
        # Melt the dataframe to long format for plotting
        melted_df = pd.melt(
            df_long, 
            id_vars=['WardName'], 
            value_vars=model_cols,
            var_name='variable', 
            value_name='value'
        )
        
        # Calculate ward rankings - lower rank value = HIGHER vulnerability
        ward_rankings = melted_df.groupby('WardName')['value'].median().reset_index()
        ward_rankings = ward_rankings.sort_values('value', ascending=False)
        ward_rankings['overall_rank'] = range(1, len(ward_rankings) + 1)
        
        # Create vulnerability categories (high, medium, low)
        ward_rankings['vulnerability_category'] = pd.cut(
            ward_rankings['overall_rank'],
            bins=[0, len(ward_rankings)//3, 2*len(ward_rankings)//3, len(ward_rankings)],
            labels=['High', 'Medium', 'Low']
        )
        
        # Merge rankings back to the melted dataframe
        df_long = pd.merge(melted_df, ward_rankings[['WardName', 'overall_rank', 'vulnerability_category']], on='WardName')
        
        # Sort by overall rank (most vulnerable wards at the top)
        df_long['WardName'] = pd.Categorical(
            df_long['WardName'],
            categories=ward_rankings.sort_values('overall_rank')['WardName'],
            ordered=True
        )
        
        # Calculate the number of pages needed
        total_wards = len(ward_rankings)
        total_pages = (total_wards + wards_per_page - 1) // wards_per_page
        
        # Create a list to store the plots for each page
        plot_list = []
        
        # Generate plot for each page
        for page in range(1, total_pages + 1):
            # Calculate start and end indices for this page
            start_idx = (page - 1) * wards_per_page
            end_idx = min(start_idx + wards_per_page, total_wards)
            
            # Get ward names for this page based on ranking
            page_wards = ward_rankings.sort_values('overall_rank')['WardName'].iloc[start_idx:end_idx].tolist()
            
            # Filter data for these wards
            page_data = df_long[df_long['WardName'].isin(page_wards)].copy()
            
            # Create figure
            fig = go.Figure()
            
            # Add temporary helper column for sorting
            page_data = page_data.merge(
                pd.DataFrame({'WardName': page_wards, 'sort_order': range(len(page_wards))}),
                on='WardName'
            )
            page_data = page_data.sort_values('sort_order')
            
            # For each ward, add a box plot
            for i, ward in enumerate(page_wards):
                ward_data = page_data[page_data['WardName'] == ward]
                rank = ward_rankings[ward_rankings['WardName'] == ward]['overall_rank'].values[0]
                category = ward_rankings[ward_rankings['WardName'] == ward]['vulnerability_category'].values[0]
                
                # Set color based on vulnerability category - match the R Shiny version color scheme
                if category == 'High':
                    box_color = '#69b3a2'  # Green-blue
                elif category == 'Medium':
                    box_color = '#a8d8b9'  # Light green
                else:
                    box_color = '#c7e9c0'  # Very light green
                
                fig.add_trace(go.Box(
                    x=ward_data['value'],
                    y=[ward] * len(ward_data),
                    name=ward,
                    orientation='h',
                    marker_color=box_color,
                    marker_line=dict(color='#3c5e8b', width=1.5),  # Blue border
                    line=dict(color='#3c5e8b', width=1.5),  # Blue border for box
                    hoverinfo='all',
                    hovertemplate=f"<b>{ward}</b><br>Rank: {rank}<br>Category: {category}<br>Score: %{{x:.3f}}<extra></extra>",
                    boxmean=True,  # Show mean as a dashed line
                    showlegend=False
                ))
            
            # Update layout to match the R version - cleaned up for better UI
            fig.update_layout(
                title={
                    'text': f'Ward Rankings Distribution (Page {page} of {total_pages})',
                    'x': 0.5,
                    'y': 0.98,
                    'xanchor': 'center',
                    'yanchor': 'top',
                    'font': {'size': 20, 'color': '#333', 'family': 'Arial, sans-serif'}
                },
                xaxis={
                    'title': {
                        'text': 'Risk Score',
                        'font': {'size': 14}
                    },
                    'zeroline': True,
                    'gridcolor': '#E5E5E5',
                    'showgrid': True
                },
                yaxis={
                    'title': '',
                    'categoryorder': 'array',
                    'categoryarray': page_wards,
                    'gridcolor': '#E5E5E5',
                    'showgrid': True
                },
                height=520,
                width=800,
                margin=dict(l=150, r=20, t=80, b=50),  # Left margin for ward names
                plot_bgcolor='#F8F9FA',
                paper_bgcolor='#F8F9FA',
                annotations=[
                    dict(
                        x=0.5, y=-0.15,
                        text="Most vulnerable wards at top | Least vulnerable at bottom",
                        showarrow=False,
                        xref="paper", yref="paper",
                        font=dict(size=14, color='darkred')
                    )
                ],
                autosize=True
            )
            
            # Store the plot in the list
            plot_list.append(fig)
        
        # Return the results as a dictionary
        return {
            'plots': plot_list, 
            'ward_rankings': ward_rankings,
            'total_pages': total_pages,
            'current_page': 1,
            'status': 'success',
            'message': 'Successfully created vulnerability box plots'
        }
    
    except Exception as e:
        logger.error(f"Error creating vulnerability plot: {str(e)}")
        import traceback
        logger.error(traceback.format_exc())
        return {
            'status': 'error',
            'message': f'Error creating vulnerability plot: {str(e)}'
        }

def create_vulnerability_map(data_handler):
    """
    Create vulnerability ranking map
    
    Args:
        data_handler: DataHandler instance
        
    Returns:
        dict: Status and visualization information
    """
    try:
        # Check if vulnerability rankings are available
        if not hasattr(data_handler, 'vulnerability_rankings') or data_handler.vulnerability_rankings is None:
            # Check if box plot function has been run
            if hasattr(data_handler, 'boxwhisker_plot') and data_handler.boxwhisker_plot:
                # Extract ward rankings from box plot data
                data_handler.vulnerability_rankings = data_handler.boxwhisker_plot['ward_rankings']
            else:
                # Try to load from file
                rankings_file = os.path.join(data_handler.session_folder, 'vulnerability_rankings.csv')
                if os.path.exists(rankings_file):
                    data_handler.vulnerability_rankings = pd.read_csv(rankings_file)
                else:
                    # If not available, try to run the box plot function to generate rankings
                    if hasattr(data_handler, 'composite_scores') and 'scores' in data_handler.composite_scores:
                        box_plot_result = box_plot_function(data_handler.composite_scores['scores'])
                        if isinstance(box_plot_result, dict) and 'ward_rankings' in box_plot_result:
                            data_handler.vulnerability_rankings = box_plot_result['ward_rankings']
                            data_handler.boxwhisker_plot = box_plot_result
                        else:
                            return {
                                'status': 'error',
                                'message': 'Could not generate vulnerability rankings'
                            }
                    else:
                        return {
                            'status': 'error',
                            'message': 'Vulnerability rankings not available. Run vulnerability analysis first.'
                        }
        
        if data_handler.shapefile_data is None:
            return {
                'status': 'error',
                'message': 'Shapefile data not loaded'
            }
        
        # Get a copy of the shapefile with standardized CRS
        shapefile_data = ensure_wgs84_crs(data_handler.shapefile_data)
        
        # Ensure vulnerability_rankings has the right data types
        # Convert columns that should be numeric
        for col in ['overall_rank', 'value']:
            if col in data_handler.vulnerability_rankings.columns:
                data_handler.vulnerability_rankings[col] = pd.to_numeric(data_handler.vulnerability_rankings[col], errors='coerce')
        
        # Merge shapefile with vulnerability rankings
        gdf = shapefile_data.merge(
            data_handler.vulnerability_rankings,
            on='WardName',
            how='left'
        )
        
        # Handle any NaN values in overall_rank (wards not in the rankings)
        if 'overall_rank' in gdf.columns:
            gdf['overall_rank'] = gdf['overall_rank'].fillna(-1).astype(int)
        
        # Convert geometry to geojson with proper serialization
        gdf_prepared = prepare_geodataframe_for_json(gdf)
        geojson = json.loads(gdf_prepared.to_json())
        
        # Get proper map centering
        center_lat = gdf.geometry.centroid.y.mean()
        center_lon = gdf.geometry.centroid.x.mean()
        
        # Calculate appropriate zoom level based on the bounding box
        bounds = gdf.geometry.total_bounds  # minx, miny, maxx, maxy
        span_x = max(0.01, bounds[2] - bounds[0])  # Ensure minimum span to avoid zoom errors
        span_y = max(0.01, bounds[3] - bounds[1])
        
        # Calculate zoom level - ensure it's reasonable
        zoom_level = min(10, max(4, 6 - np.log(max(span_x, span_y))))
        
        # Create choropleth map with Plotly
        fig = go.Figure()
        
        # Create hover text with proper formatting
        hover_text = []
        for i, row in gdf.iterrows():
            ward_name = row['WardName']
            rank = row['overall_rank'] if 'overall_rank' in gdf.columns and row['overall_rank'] != -1 else "Not ranked"
            category = row['vulnerability_category'] if 'vulnerability_category' in gdf.columns else "Unknown"
            hover_text.append(f"{ward_name}<br>Rank: {rank}<br>Category: {category}")
        
        # Add the choropleth layer
        fig.add_trace(go.Choroplethmapbox(
            geojson=geojson,
            locations=gdf.index,
            z=gdf['overall_rank'] if 'overall_rank' in gdf.columns else None,
            colorscale='Plasma_r',  # Reverse plasma so high vulnerability (low rank) is dark
            marker_opacity=0.8,
            marker_line_width=0.5,
            marker_line_color='black',
            hovertemplate='%{hovertext}<extra></extra>',
            hovertext=hover_text,
            colorbar=dict(
                title=dict(
                    text="Vulnerability Rank",
                    font=dict(size=12)
                ),
                tickmode='array',
                tickvals=[1, gdf['overall_rank'].max() / 2, gdf['overall_rank'].max()],
                ticktext=['High', 'Medium', 'Low']
            )
        ))
        
        # Update layout
        fig.update_layout(
            title={
                'text': "Ward Vulnerability Map",
                'x': 0.5,
                'xanchor': 'center',
                'font': {'size': 20}
            },
            mapbox=dict(
                style="carto-positron",
                center={"lat": center_lat, "lon": center_lon},
                zoom=zoom_level
            ),
            height=480,
            width=800,
            margin=dict(l=20, r=20, t=80, b=20),
            autosize=True
        )
        
        # Create HTML file
        html_path = create_plotly_html(fig, "vulnerability_map.html")
        
        # Return success with paths and metadata
        return {
            'status': 'success',
            'message': f'Successfully created vulnerability map',
            'image_path': html_path,
            'viz_type': 'vulnerability_map',
            'ai_response': "Here's the vulnerability map showing the geographical distribution of risk across wards. Darker colors indicate higher vulnerability - these are the areas that should be prioritized for bed net distribution and other interventions. You can see how vulnerability is distributed spatially, which is essential for planning logistics and coordinating intervention efforts."
        }
        
    except Exception as e:
        logger.error(f"Error creating vulnerability map: {str(e)}")
        import traceback
        logger.error(traceback.format_exc())
        return {
            'status': 'error',
            'message': f'Error creating vulnerability map: {str(e)}'
        }

def create_urban_extent_map(data_handler, threshold=30):
    """
    Create urban extent map at a specific threshold
    
    Args:
        data_handler: DataHandler instance
        threshold: Urban threshold percentage
        
    Returns:
        dict: Status and visualization information
    """
    try:
        # Check if shapefile data is available
        if data_handler.shapefile_data is None:
            return {
                'status': 'error',
                'message': 'Shapefile data not loaded'
            }
        
        # Get a copy of the shapefile with standardized CRS
        shapefile_data = ensure_wgs84_crs(data_handler.shapefile_data)
        
        # Check for UrbanPercent column
        urban_percent_col = None
        for col_name in ['UrbanPercent', 'UrbanPerce', 'Urban_Percent', 'urban_percent']:
            if col_name in shapefile_data.columns:
                urban_percent_col = col_name
                break
        
        if urban_percent_col is None:
            # If no urban percent column, check for binary Urban column
            if 'Urban' in shapefile_data.columns:
                # Convert binary Urban to percentage (Yes/No, True/False, etc.)
                shapefile_data['UrbanPercent'] = shapefile_data['Urban'].apply(
                    lambda x: 100 if str(x).lower() in ['yes', 'true', '1', 'y'] else 0
                )
                urban_percent_col = 'UrbanPercent'
            else:
                return {
                    'status': 'error',
                    'message': 'No Urban Percentage column found in shapefile data'
                }
        
        # Create a copy of shapefile data with threshold classification
        gdf = shapefile_data.copy()
        
        # Add threshold classification column
        meets_threshold_field = f'MeetsThreshold_{threshold}'
        gdf[meets_threshold_field] = gdf[urban_percent_col] >= threshold
        
        # Get counts for each category
        meets_count = gdf[gdf[meets_threshold_field]].shape[0]
        below_count = gdf[~gdf[meets_threshold_field]].shape[0]
        
        # Convert geometry to geojson with proper serialization
        gdf_prepared = prepare_geodataframe_for_json(gdf)
        geojson = json.loads(gdf_prepared.to_json())
        
        # Get proper map centering
        center_lat = gdf.geometry.centroid.y.mean()
        center_lon = gdf.geometry.centroid.x.mean()
        
        # Calculate appropriate zoom level based on the bounding box
        bounds = gdf.geometry.total_bounds  # minx, miny, maxx, maxy
        span_x = max(0.01, bounds[2] - bounds[0])  # Ensure minimum span to avoid zoom errors
        span_y = max(0.01, bounds[3] - bounds[1])
        
        # Calculate zoom level - ensure it's reasonable
        zoom_level = min(10, max(4, 6 - np.log(max(span_x, span_y))))
        
        # Create choropleth map with Plotly
        fig = go.Figure()
        
        # Create hover text with proper formatting
        hover_text = []
        for i, row in gdf.iterrows():
            ward_name = row['WardName']
            urban_pct = row[urban_percent_col]
            meets = "Above threshold" if row[meets_threshold_field] else "Below threshold"
            hover_text.append(f"{ward_name}<br>Urban%: {urban_pct:.1f}%<br>Status: {meets}")
        
        # Add the choropleth layer
        fig.add_trace(go.Choroplethmapbox(
            geojson=geojson,
            locations=gdf.index,
            z=gdf[meets_threshold_field].astype(int),  # Convert boolean to int (0/1)
            colorscale=[[0, '#E8F8F5'], [1, '#1ABC9C']],  # Light teal to dark teal
            marker_opacity=0.8,
            marker_line_width=0.5,
            marker_line_color='black',
            hovertemplate='%{hovertext}<extra></extra>',
            hovertext=hover_text,
            showscale=False
        ))
        
        # Update layout
        fig.update_layout(
            title={
                'text': f"Urban Extent at {threshold}% Threshold",
                'x': 0.5,
                'xanchor': 'center',
                'font': {'size': 20}
            },
            mapbox=dict(
                style="carto-positron",
                center={"lat": center_lat, "lon": center_lon},
                zoom=zoom_level
            ),
            height=480,
            width=800,
            margin=dict(l=20, r=20, t=80, b=20),
            annotations=[
                dict(
                    text=f'Above threshold: {meets_count} wards | Below threshold: {below_count} wards',
                    showarrow=False,
                    xref="paper", yref="paper",
                    x=0.5, y=-0.05,
                    font=dict(size=14),
                    align="center"
                )
            ],
            autosize=True
        )
        
        # Add custom legend as annotations
        fig.add_annotation(
            x=0.02, y=0.98,
            text=f'<b>Above {threshold}% threshold</b>',
            showarrow=False,
            xref="paper", yref="paper",
            bordercolor='black',
            borderwidth=1,
            borderpad=4,
            bgcolor='#1ABC9C',
            font=dict(size=14, color='white')
        )
        
        fig.add_annotation(
            x=0.02, y=0.90,
            text=f'<b>Below {threshold}% threshold</b>',
            showarrow=False,
            xref="paper", yref="paper",
            bordercolor='black',
            borderwidth=1,
            borderpad=4,
            bgcolor='#E8F8F5',
            font=dict(size=14, color='black')
        )
        
        # Create HTML file
        html_path = create_plotly_html(fig, f"urban_extent_{threshold}.html")
        
        # Return success with paths and metadata
        return {
            'status': 'success',
            'message': f'Successfully created urban extent map at {threshold}% threshold',
            'image_path': html_path,
            'threshold': threshold,
            'meets_threshold': meets_count,
            'below_threshold': below_count,
            'viz_type': 'urban_extent_map',
            'ai_response': f"Here's the urban extent map at {threshold}% threshold. Dark blue areas are above the threshold (more urban), while light teal areas are below the threshold (less urban). Areas below this {threshold}% urban threshold would typically be prioritized for bed net distribution. There are {below_count} wards below the threshold and {meets_count} wards above it."
        }
        
    except Exception as e:
        logger.error(f"Error creating urban extent map: {str(e)}")
        import traceback
        logger.error(traceback.format_exc())
        return {
            'status': 'error',
            'message': f'Error creating urban extent map: {str(e)}'
        }

def create_decision_tree_plot(data_handler):
    """
    Create a decision tree visualization flowing from left to right
    
    Args:
        data_handler: DataHandler instance
        
    Returns:
        dict: Status and visualization information
    """
    try:
        # Get all variables and selected variables
        all_variables = []
        selected_variables = []
        excluded_variables = []
        top_5_wards = []
        
        # Get all variables from original data
        if data_handler.csv_data is not None:
            all_variables = [col for col in data_handler.csv_data.columns 
                           if col != 'WardName' and pd.api.types.is_numeric_dtype(data_handler.csv_data[col]) and not is_id_column(col)]
        
        # Get selected variables from composite scores
        if hasattr(data_handler, 'composite_variables') and data_handler.composite_variables:
            selected_variables = data_handler.composite_variables
        elif data_handler.composite_scores is not None and 'model_formulas' in data_handler.composite_scores:
            # Use variables from the first model
            if data_handler.composite_scores['model_formulas']:
                selected_variables = data_handler.composite_scores['model_formulas'][0]['variables']
                # Clean up variable names if needed
                selected_variables = [var.replace('normalization_', '') for var in selected_variables]
        
        # Get excluded variables
        excluded_variables = [var for var in all_variables if var not in selected_variables]
        
        # Get top 5 vulnerable wards
        if hasattr(data_handler, 'vulnerability_rankings') and data_handler.vulnerability_rankings is not None:
            top_5 = data_handler.vulnerability_rankings.sort_values('overall_rank').head(5)
            top_5_wards = top_5['WardName'].tolist()
        
        # Get full variable names
        full_all_variables = [f"{var} ({get_full_variable_name(var)})" for var in all_variables]
        full_selected_variables = [f"{var} ({get_full_variable_name(var)})" for var in selected_variables]
        full_excluded_variables = [f"{var} ({get_full_variable_name(var)})" for var in excluded_variables]
        
        # Create HTML content for the decision tree
        html_content = """
        <!DOCTYPE html>
        <html>
        <head>
            <meta charset="UTF-8">
            <title>Decision Tree Visualization</title>
            <style>
                body {
                    font-family: 'Arial', sans-serif;
                    background-color: #ffffff;
                    margin: 0;
                    padding: 0;
                    display: flex;
                    justify-content: center;
                }
                .decision-tree-container {
                    width: 100%;
                    max-width: 900px;
                    padding: 20px;
                }
                .tree-row {
                    display: flex;
                    justify-content: center;
                    margin-bottom: 20px;
                    position: relative;
                }
                .node {
                    background-color: #f5f5f5;
                    border-radius: 8px;
                    padding: 15px;
                    box-shadow: 0 2px 4px rgba(0,0,0,0.1);
                    text-align: center;
                    margin: 0 10px;
                    width: 100%;
                    max-width: 250px;
                }
                .node-title {
                    font-weight: bold;
                    margin-bottom: 8px;
                    font-size: 16px;
                }
                .list-container {
                    max-height: 150px;
                    overflow-y: auto;
                    text-align: left;
                    margin-top: 10px;
                }
                .list-container ul, .list-container ol {
                    padding-left: 20px;
                    margin: 5px 0;
                }
                .list-container li {
                    margin-bottom: 6px;
                    font-size: 13px;
                }
                .navy {
                    background-color: #1B2631;
                    color: white;
                }
                .orange {
                    background-color: #E67E22;
                    color: white;
                }
                .teal {
                    background-color: #16A596;
                    color: white;
                }
                .gray {
                    background-color: #7F8C8D;
                    color: white;
                }
                .green {
                    background-color: #27AE60;
                    color: white;
                }
                .blue {
                    background-color: #2980B9;
                    color: white;
                }
                .purple {
                    background-color: #8E44AD;
                    color: white;
                }
                .arrow {
                    position: absolute;
                    width: 0;
                    height: 0;
                    border-left: 10px solid transparent;
                    border-right: 10px solid transparent;
                    border-top: 10px solid #666;
                    left: 50%;
                    bottom: -15px;
                    transform: translateX(-50%);
                }
                .arrow-label {
                    position: absolute;
                    background-color: white;
                    padding: 2px 8px;
                    border-radius: 10px;
                    font-size: 12px;
                    font-weight: bold;
                }
                .arrow-container {
                    position: relative;
                    height: 30px;
                    width: 100%;
                }
                .vertical-line {
                    position: absolute;
                    width: 2px;
                    background-color: #666;
                    left: 50%;
                    transform: translateX(-50%);
                    top: 0;
                    bottom: 0;
                }
                .branch-container {
                    display: flex;
                    justify-content: space-around;
                    width: 100%;
                    position: relative;
                }
                .branch-line {
                    position: absolute;
                    top: 0;
                    height: 2px;
                    background-color: #666;
                }
                .branch-label {
                    position: absolute;
                    top: -10px;
                    background-color: white;
                    padding: 0 5px;
                    font-size: 12px;
                }
            </style>
        </head>
        <body>
            <div class="decision-tree-container">
                <h1 style="text-align: center; margin-bottom: 30px;">Malaria Risk Analysis Workflow</h1>
                
                <!-- Row 1: Start Node -->
                <div class="tree-row">
                    <div class="node navy">
                        <div class="node-title">Malaria Risk Assessment</div>
                        <div>Variable Selection</div>
                    </div>
                </div>
                
                <!-- Arrow between Row 1 and 2 -->
                <div class="arrow-container">
                    <div class="vertical-line"></div>
                </div>
                
                <!-- Row 2: Variables List -->
                <div class="tree-row">
                    <div class="node navy">
                        <div class="node-title">Variables</div>
                        <div class="list-container">
                            <ul>
        """
        
        # Add all variables to HTML
        for var in full_all_variables[:10]:  # Limit to first 10 for space
            html_content += f"                                <li>{var}</li>\n"
        
        if len(full_all_variables) > 10:
            html_content += f"                                <li>...and {len(full_all_variables) - 10} more</li>\n"
            
        html_content += """
                            </ul>
                        </div>
                    </div>
                </div>
                
                <!-- Arrow between Row 2 and 3 -->
                <div class="arrow-container">
                    <div class="vertical-line"></div>
                </div>
                
                <!-- Row 3: Evaluation Diamond -->
                <div class="tree-row">
                    <div class="node orange">
                        <div class="node-title">Variable Evaluation</div>
                        <div>Assessment of variable relationships with malaria risk</div>
                    </div>
                </div>
                
                <!-- Branch Lines for Include/Exclude -->
                <div class="branch-container" style="height: 50px;">
                    <div class="branch-line" style="left: 25%; width: 25%;"></div>
                    <div class="branch-label" style="left: 32%;">Include</div>
                    
                    <div class="branch-line" style="left: 50%; width: 25%;"></div>
                    <div class="branch-label" style="left: 62%;">Exclude</div>
                </div>
                
                <!-- Row 4: Included and Excluded Variables -->
                <div class="tree-row">
                    <div class="node teal" style="flex: 1;">
                        <div class="node-title">Included Variables</div>
                        <div class="list-container">
                            <ul>
        """
        
        # Add included variables to HTML
        for var in full_selected_variables:
            html_content += f"                                <li>{var}</li>\n"
        
        if not full_selected_variables:
            html_content += "                                <li>No variables selected yet</li>\n"
            
        html_content += """
                            </ul>
                        </div>
                    </div>
                    
                    <div class="node gray" style="flex: 1;">
                        <div class="node-title">Excluded Variables</div>
                        <div class="list-container">
                            <ul>
        """
        
        # Add excluded variables to HTML
        for var in full_excluded_variables[:10]:  # Limit to first 10 for space
            html_content += f"                                <li>{var}</li>\n"
        
        if len(full_excluded_variables) > 10:
            html_content += f"                                <li>...and {len(full_excluded_variables) - 10} more</li>\n"
        
        if not full_excluded_variables:
            html_content += "                                <li>No variables excluded yet</li>\n"
            
        html_content += """
                            </ul>
                        </div>
                    </div>
                </div>
                
                <!-- Arrow from Included Variables to Normalization -->
                <div class="arrow-container">
                    <div class="vertical-line" style="left: 25%;"></div>
                </div>
                
                <!-- Row 5: Normalization and Calculation -->
                <div class="tree-row">
                    <div class="node green" style="margin-left: 0;">
                        <div class="node-title">Data Normalization &<br>Composite Score Calculation</div>
                        <div>Converting variables to common scale and calculating risk scores</div>
                    </div>
                </div>
                
                <!-- Arrow between Row 5 and 6 -->
                <div class="arrow-container">
                    <div class="vertical-line"></div>
                </div>
                
                <!-- Row 6: Risk Maps -->
                <div class="tree-row">
                    <div class="node blue">
                        <div class="node-title">Generated Risk Maps<br>for All Combinations</div>
                        <div>Maps showing risk scores for different variable combinations</div>
                    </div>
                </div>
                
                <!-- Arrow between Row 6 and 7 -->
                <div class="arrow-container">
                    <div class="vertical-line"></div>
                </div>
                
                <!-- Row 7: Vulnerability Analysis -->
                <div class="tree-row">
                    <div class="node purple">
                        <div class="node-title">Vulnerability Analysis</div>
                        <div>Box and whisker plot of ward vulnerability rankings</div>
                    </div>
                </div>
                
                <!-- Arrow between Row 7 and 8 -->
                <div class="arrow-container">
                    <div class="vertical-line"></div>
                </div>
                
                <!-- Row 8: Priority Wards -->
                <div class="tree-row">
                    <div class="node purple">
                        <div class="node-title">Top 5 Wards<br>for Reprioritization</div>
                        <div class="list-container">
                            <ol>
        """
        
        # Add top 5 wards to HTML
        for ward in top_5_wards:
            html_content += f"                                <li>{ward}</li>\n"
        
        if not top_5_wards:
            html_content += "                                <li>No wards ranked yet</li>\n"
            
        html_content += """
                            </ol>
                        </div>
                    </div>
                </div>
            </div>
        </body>
        </html>
        """
        
        # Save HTML to a file
        session_id = session.get('session_id', 'default')
        file_path = os.path.join(current_app.static_folder, 'uploads', session_id, 'decision_tree.html')
        
        os.makedirs(os.path.dirname(file_path), exist_ok=True)
        with open(file_path, 'w', encoding='utf-8') as f:
            f.write(html_content)
        
        web_path = f"/static/uploads/{session_id}/decision_tree.html"
        
        # Return success with paths and metadata
        return {
            'status': 'success',
            'message': 'Successfully created decision tree visualization',
            'image_path': web_path,
            'viz_type': 'decision_tree',
            'ai_response': "Here's the decision tree visualization showing the workflow of our malaria risk analysis. It illustrates how variables were selected, normalized, and combined to create risk maps. The tree flows from top to bottom, showing the entire process from variable selection to the identification of priority wards for bed net distribution."
        }
        
    except Exception as e:
        logger.error(f"Error creating decision tree plot: {str(e)}")
        import traceback
        logger.error(traceback.format_exc())
        return {
            'status': 'error',
            'message': f'Error creating decision tree plot: {str(e)}'
        }