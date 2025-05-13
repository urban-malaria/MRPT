import os
import zipfile
import tempfile
import logging
import pandas as pd
import geopandas as gpd
import numpy as np
from shapely.geometry import Point, Polygon
import shutil
import itertools
import json
import requests
from typing import List, Dict, Tuple, Optional, Union, Any
from pysal.lib import weights
from libpysal.weights import Queen 

# Set up logging
logger = logging.getLogger(__name__)

class DataHandler:
    """Class to handle data loading, cleaning, and processing for MRPT"""
    
    def __init__(self, session_folder):
        """Initialize with session folder path"""
        self.session_folder = session_folder
        self.csv_data = None
        self.shapefile_data = None
        self.cleaned_data = None
        self.normalized_data = None
        self.composite_scores = None
        self.variable_relationships = {}
        self.missing_columns = []
        self.mismatched_wards = None
        self.composite_variables = None
        self.vulnerability_rankings = None
        self.boxwhisker_plot = None
        self.urban_extent_results = None
        self.na_handling_methods = []
        
        # Create session folder if it doesn't exist
        os.makedirs(self.session_folder, exist_ok=True)
        
        # Set up logging
        self.logger = logging.getLogger(__name__)
    
    def load_csv(self, file_path):
        """
        Load and process CSV or Excel file
        
        Args:
            file_path: Path to the CSV or Excel file
            
        Returns:
            dict: Status and information about the loaded data
        """
        try:
            # Check file extension to determine loading method
            file_extension = os.path.splitext(file_path)[1].lower()
            
            if file_extension in ['.xlsx', '.xls']:
                # Load Excel file
                self.csv_data = pd.read_excel(file_path)
            else:
                # Load CSV file with robust parameters
                self.csv_data = pd.read_csv(
                    file_path,
                    na_values=['NA', '', 'N/A'],
                    keep_default_na=True
                )
            
            # Ensure column names are valid
            self.csv_data.columns = self.csv_data.columns.str.strip()
            
            # Ensure WardName column exists - rename if needed
            if 'Ward' in self.csv_data.columns and 'WardName' not in self.csv_data.columns:
                self.csv_data = self.csv_data.rename(columns={'Ward': 'WardName'})
            
            # Handle duplicate ward names if WardCode exists
            if 'WardName' in self.csv_data.columns and 'WardCode' in self.csv_data.columns:
                self.csv_data = self._handle_duplicate_wardnames(self.csv_data)
            
            # Check for missing values
            self.missing_columns = self._check_missing_values(self.csv_data)
            
            # Save processed CSV locally
            self.csv_data.to_csv(os.path.join(self.session_folder, 'processed_data.csv'), index=False)
            
            return {
                'status': 'success',
                'message': f'CSV file loaded successfully with {len(self.csv_data)} rows and {len(self.csv_data.columns)} columns',
                'rows': len(self.csv_data),
                'columns': len(self.csv_data.columns),
                'missing_values': len(self.missing_columns)
            }
            
        except Exception as e:
            self.logger.error(f"Error loading CSV file: {str(e)}")
            return {
                'status': 'error',
                'message': f'Error loading CSV file: {str(e)}'
            }
    
    def standardize_shapefile_crs(self, target_crs=4326):
        """
        Standardize the shapefile's coordinate reference system
        
        Args:
            target_crs: Target CRS EPSG code (default: 4326 for WGS84)
            
        Returns:
            dict: Status and information
        """
        try:
            if self.shapefile_data is None:
                return {
                    'status': 'error',
                    'message': 'No shapefile data loaded'
                }
            
            # Check if CRS is already the target
            current_crs = self.shapefile_data.crs
            if current_crs == f"EPSG:{target_crs}" or current_crs == target_crs:
                self.logger.info(f"Shapefile already in target CRS (EPSG:{target_crs})")
                return {
                    'status': 'success',
                    'message': f'Shapefile already in target CRS (EPSG:{target_crs})'
                }
            
            # Convert to target CRS
            self.logger.info(f"Converting shapefile from {current_crs} to EPSG:{target_crs}")
            self.shapefile_data = self.shapefile_data.to_crs(epsg=target_crs)
            
            # Save the standardized shapefile
            shp_output_dir = os.path.join(self.session_folder, 'shapefile')
            os.makedirs(shp_output_dir, exist_ok=True)
            self.shapefile_data.to_file(os.path.join(shp_output_dir, 'processed_standardized.shp'))
            
            return {
                'status': 'success',
                'message': f'Successfully converted shapefile to EPSG:{target_crs}'
            }
            
        except Exception as e:
            self.logger.error(f"Error standardizing shapefile CRS: {str(e)}")
            return {
                'status': 'error',
                'message': f'Error standardizing shapefile CRS: {str(e)}'
            }

    def load_shapefile(self, zip_file_path):
        """
        Extract and load shapefile from ZIP
        
        Args:
            zip_file_path: Path to the ZIP file containing shapefile
            
        Returns:
            dict: Status and information about the loaded shapefile
        """
        try:
            # Create a temporary directory to extract the ZIP
            with tempfile.TemporaryDirectory() as temp_dir:
                # Extract the ZIP file
                with zipfile.ZipFile(zip_file_path, 'r') as zip_ref:
                    zip_ref.extractall(temp_dir)
                
                # Find shapefile(s) in the extracted directory
                shp_files = []
                for root, dirs, files in os.walk(temp_dir):
                    for file in files:
                        if file.endswith('.shp'):
                            shp_files.append(os.path.join(root, file))
                
                if not shp_files:
                    return {
                        'status': 'error',
                        'message': 'No shapefile (.shp) found in the ZIP file'
                    }
                
                # Load the first shapefile found
                self.shapefile_data = gpd.read_file(shp_files[0])
                
                # Log the original CRS
                self.logger.info(f"Loaded shapefile with CRS: {self.shapefile_data.crs}")
                
                # Ensure WardName column exists
                if 'WardName' not in self.shapefile_data.columns:
                    # Look for potential ward name columns
                    potential_columns = [col for col in self.shapefile_data.columns if 
                                        any(name in col.lower() for name in ['ward', 'name', 'area'])]
                    
                    if potential_columns:
                        # Use the first potential column
                        self.shapefile_data = self.shapefile_data.rename(
                            columns={potential_columns[0]: 'WardName'}
                        )
                    else:
                        # Create sequential ward names if no suitable column found
                        self.shapefile_data['WardName'] = [f'Ward_{i+1}' for i in range(len(self.shapefile_data))]
                
                # Handle duplicate ward names if WardCode exists
                if 'WardCode' in self.shapefile_data.columns:
                    self.shapefile_data = self._handle_duplicate_wardnames(self.shapefile_data)
                
                # Ensure UrbanPercent column exists (for urban extent analysis)
                if 'UrbanPercent' not in self.shapefile_data.columns:
                    if 'Urban' in self.shapefile_data.columns:
                        # If Urban column exists, convert to percentage
                        self.shapefile_data['Urban'] = self.shapefile_data['Urban'].astype(str)
                        # Convert Yes/No to 100/0
                        self.shapefile_data['UrbanPercent'] = self.shapefile_data['Urban'].apply(
                            lambda x: 100 if x.lower() in ['yes', 'true', '1'] else 0
                        )
                    else:
                        # Create random urban percentages for demonstration
                        self.shapefile_data['UrbanPercent'] = np.random.uniform(0, 100, len(self.shapefile_data))
                        self.logger.warning("Created random UrbanPercent values as column was missing")
                
                # Standardize the CRS to WGS84 (EPSG:4326)
                # Check if CRS needs conversion
                if self.shapefile_data.crs and self.shapefile_data.crs != "EPSG:4326":
                    self.logger.info(f"Converting shapefile from {self.shapefile_data.crs} to EPSG:4326")
                    try:
                        self.shapefile_data = self.shapefile_data.to_crs(epsg=4326)
                        self.logger.info("CRS conversion successful")
                    except Exception as crs_error:
                        self.logger.warning(f"CRS conversion error: {str(crs_error)}. Using original CRS.")
                
                # Save shapefile locally for future use
                shp_output_dir = os.path.join(self.session_folder, 'shapefile')
                os.makedirs(shp_output_dir, exist_ok=True)
                self.shapefile_data.to_file(os.path.join(shp_output_dir, 'processed.shp'))
                
                # Check for ward name mismatches if CSV is already loaded
                if self.csv_data is not None:
                    self.mismatched_wards = self.check_wardname_mismatches()
                
                return {
                    'status': 'success',
                    'message': f'Shapefile loaded successfully with {len(self.shapefile_data)} features',
                    'features': len(self.shapefile_data),
                    'crs': str(self.shapefile_data.crs)
                }
        
        except Exception as e:
            self.logger.error(f"Error loading shapefile: {str(e)}")
            return {
                'status': 'error',
                'message': f'Error loading shapefile: {str(e)}'
            }
    
    def check_wardname_mismatches(self):
        """
        Check for ward name mismatches between CSV and shapefile
        
        Returns:
            list: List of mismatched ward names, or None if no mismatches or data not loaded
        """
        if self.csv_data is None or self.shapefile_data is None:
            return None
        
        if 'WardName' not in self.csv_data.columns or 'WardName' not in self.shapefile_data.columns:
            return None
        
        # Get ward names from both datasets
        csv_wardnames = set(self.csv_data['WardName'].unique())
        shp_wardnames = set(self.shapefile_data['WardName'].unique())
        
        # Find ward names in CSV that don't exist in shapefile
        mismatched_wards = csv_wardnames - shp_wardnames
        
        if mismatched_wards:
            # Create a list of mismatches with potential matches from shapefile
            mismatches = []
            for ward in mismatched_wards:
                mismatches.append({
                    'csv_wardname': ward,
                    'potential_matches': list(shp_wardnames)[:10]  # Limit to 10 potential matches
                })
            
            return mismatches
        
        return None
    
    def clean_data(self, na_methods=None):
        """
        Clean data by handling missing values. Tries spatial neighbor mean first by default.

        Args:
            na_methods: Optional dict mapping columns to specific methods ('mean', 'mode').
                        If None or a column is missing, 'spatial' will be attempted first.

        Returns:
            dict: Status and information about the cleaning process
        """
        if self.csv_data is None:
            return {'status': 'error', 'message': 'No CSV data loaded'}

        # Identify columns with missing values before cleaning
        missing_cols_initial = self._check_missing_values(self.csv_data)
        if not missing_cols_initial:
            self.cleaned_data = self.csv_data.copy()
            self.logger.info("No missing values found in the initial dataset.")
            return {'status': 'success', 'message': 'No missing values found'}

        try:
            self.cleaned_data = self.csv_data.copy() # Start with a fresh copy
            methods_actually_used = {}
            self.na_handling_methods = [] # Reset list

            self.logger.info(f"Starting cleaning process for columns: {', '.join(missing_cols_initial)}")

            for col in missing_cols_initial:
                # Check if column still exists and has missing values in the *current* state of cleaned_data
                if col not in self.cleaned_data.columns or not self.cleaned_data[col].isna().any():
                    continue

                # Determine method: Use provided method OR default to attempting spatial
                user_specified_method = na_methods.get(col) if na_methods else None
                method_used_for_col = "None" # Reset for this column

                # Attempt Spatial Mean First (if numeric and not explicitly told otherwise)
                spatial_attempted = False
                if user_specified_method != 'mean' and user_specified_method != 'mode' and pd.api.types.is_numeric_dtype(self.cleaned_data[col]):
                     spatial_attempted = self._handle_na_spatial_mean(self.cleaned_data, col) # Modifies self.cleaned_data in place
                     if spatial_attempted:
                           method_used_for_col = 'spatial (with potential fallbacks)' # Indicate spatial was primary attempt

                # Fallback or User Specified Methods
                if not spatial_attempted:
                     if pd.api.types.is_numeric_dtype(self.cleaned_data[col]):
                          # Use mean if specified or if spatial wasn't attempted/failed for numeric
                          if user_specified_method == 'mean' or not spatial_attempted:
                               self.logger.info(f"Using mean imputation for numeric column: {col}")
                               result_df = self._handle_na_mean(self.cleaned_data, col) # Modifies in place
                               if result_df is not None: method_used_for_col = 'mean'
                               else: self.logger.error(f"Mean imputation failed for {col}")
                          else: # Fallback to mode if mean wasn't specified and spatial failed
                                self.logger.info(f"Spatial/Mean failed for {col}, falling back to mode.")
                                result_df = self._handle_na_mode(self.cleaned_data, col)
                                if result_df is not None: method_used_for_col = 'mode'
                                else: self.logger.error(f"Mode fallback imputation failed for {col}")
                     else:
                           # Use mode for non-numeric columns (or if specified)
                           self.logger.info(f"Using mode imputation for non-numeric column: {col}")
                           result_df = self._handle_na_mode(self.cleaned_data, col) # Modifies in place
                           if result_df is not None: method_used_for_col = 'mode'
                           else: self.logger.error(f"Mode imputation failed for {col}")

                # Record the method finally used
                if method_used_for_col != "None":
                    methods_actually_used[col] = method_used_for_col
                    self.na_handling_methods.append({'column': col, 'method': method_used_for_col})

            # Final check for any remaining NaNs (shouldn't happen with fallbacks, but good practice)
            final_missing_check = self._check_missing_values(self.cleaned_data)
            if final_missing_check:
                 self.logger.warning(f"Warning: Missing values remain after cleaning in columns: {', '.join(final_missing_check)}")

            # Save cleaned data
            self.cleaned_data.to_csv(os.path.join(self.session_folder, 'cleaned_data.csv'), index=False)

            return {
                'status': 'success',
                'message': f'Successfully cleaned {len(missing_cols_initial)} columns with missing values.',
                'cleaned_columns': missing_cols_initial, # Report original columns identified
                'methods_used': methods_actually_used
            }

        except Exception as e:
            self.logger.error(f"Error cleaning data: {str(e)}", exc_info=True)
            return {'status': 'error', 'message': f'Error cleaning data: {str(e)}'}
    
    def determine_variable_relationships(self, variables=None):
        """
        Determine the relationship of variables with malaria risk
        
        Args:
            variables: List of variables to determine relationships for
                    If None, use all variables in cleaned data
                    
        Returns:
            dict: Variable relationships (direct/inverse)
        """
        self.logger.info("===== DETERMINING VARIABLE RELATIONSHIPS =====")
        if self.cleaned_data is None:
            self.logger.info("Cleaned data not found. Cleaning data first...")
            self.clean_data()
            self.logger.info("Data cleaning completed.")
        
        if variables is None:
            variables = self._get_numeric_columns()
            self.logger.info(f"Using all {len(variables)} numeric columns for relationship determination")
        else:
            self.logger.info(f"Using {len(variables)} specified variables for relationship determination")
        
        self.logger.info("Analyzing variables for relationship with malaria risk...")
        
        # Simple determination based on variable name patterns
        for var in variables:
            var_lower = var.lower()
            # Variables typically having inverse relationship with malaria risk
            if any(keyword in var_lower for keyword in ['distance', 'elevation', 'altitude', 
                                                    'slope', 'housing', 'quality', 'income', 
                                                    'education', 'access', 'urban']):
                self.variable_relationships[var] = 'inverse'
                self.logger.info(f"  - {var}: INVERSE relationship (increases in {var} decrease malaria risk)")
            else:
                # Default to direct relationship
                self.variable_relationships[var] = 'direct'
                self.logger.info(f"  - {var}: DIRECT relationship (increases in {var} increase malaria risk)")
        
        self.logger.info(f"Determined relationships for {len(self.variable_relationships)} variables")
        self.logger.info("===== COMPLETED VARIABLE RELATIONSHIP DETERMINATION =====")
        return self.variable_relationships
    
    def normalize_data(self, relationships=None):
        """
        Normalize data based on variable relationships
        
        Args:
            relationships: Dict mapping variables to relationships (direct/inverse)
                        If None, use determined relationships
                        
        Returns:
            dict: Status and information about normalization
        """
        self.logger.info("===== STARTING DATA NORMALIZATION =====")
        if self.cleaned_data is None:
            self.logger.info("Cleaned data not found. Cleaning data first...")
            clean_result = self.clean_data()
            if clean_result['status'] != 'success':
                self.logger.error(f"ERROR: Data cleaning failed - {clean_result['message']}")
                return clean_result
            self.logger.info("Data cleaning completed successfully.")
        
        if relationships is None:
            # If relationships not provided, determine them
            if not self.variable_relationships:
                self.logger.info("No variable relationships defined. Determining relationships first...")
                self.determine_variable_relationships()
                self.logger.info("Relationship determination completed.")
            relationships = self.variable_relationships
        
        try:
            self.logger.info("Initializing normalized dataframe...")
            # Initialize normalized dataframe with WardName
            self.normalized_data = self.cleaned_data[['WardName']].copy()
            
            # Normalize each numeric column based on its relationship
            numeric_cols = self._get_numeric_columns()
            self.logger.info(f"Normalizing {len(numeric_cols)} numeric columns...")
            
            for col in numeric_cols:
                if col in relationships:
                    self.logger.info(f"  - Normalizing {col} with {relationships[col]} relationship...")
                    # Get original values
                    values = self.cleaned_data[col].values
                    
                    # Calculate min and max
                    min_val = np.min(values)
                    max_val = np.max(values)
                    self.logger.info(f"    Range: {min_val} to {max_val}")
                    
                    if relationships[col] == 'inverse':
                        self.logger.info(f"    Using inverse normalization for {col}")
                        # For inverse relationship, invert the values first
                        # Add small constant to avoid division by zero
                        inverted = 1 / (values + 1e-10)
                        
                        # Then normalize
                        inv_min = np.min(inverted)
                        inv_max = np.max(inverted)
                        
                        normalized = (inverted - inv_min) / (inv_max - inv_min)
                    else:
                        self.logger.info(f"    Using direct normalization for {col}")
                        # For direct relationship, normalize directly
                        normalized = (values - min_val) / (max_val - min_val)
                    
                    # Add to normalized dataframe with standardized name
                    norm_col_name = f"normalization_{col.lower()}"
                    self.normalized_data[norm_col_name] = normalized
                    self.logger.info(f"    Normalized {col} to range 0-1 as {norm_col_name}")
                else:
                    self.logger.info(f"  - SKIPPING {col} - no relationship defined")
            
            self.logger.info("Saving normalized data to CSV...")
            # Save normalized data
            self.normalized_data.to_csv(os.path.join(self.session_folder, 'normalized_data.csv'), index=False)
            
            self.logger.info("===== COMPLETED DATA NORMALIZATION SUCCESSFULLY =====")
            return {
                'status': 'success',
                'message': f'Successfully normalized {len(numeric_cols)} variables',
                'normalized_columns': numeric_cols
            }
            
        except Exception as e:
            self.logger.error(f"ERROR in data normalization: {str(e)}")
            return {
                'status': 'error',
                'message': f'Error normalizing data: {str(e)}'
            }
    
    def suggest_composite_variables(self, variable_names, relationships, openai_api_key=None):
        """
        Use OpenAI to suggest the best variables for a composite malaria risk score.
        
        Args:
            variable_names: List of available variable names
            relationships: Dict mapping variable names to relationships (direct/inverse)
            openai_api_key: OpenAI API key (if None, uses environment variable)
        
        Returns:
            List of suggested variable names
        """
        # Get API key from environment if not provided
        if openai_api_key is None:
            openai_api_key = os.environ.get('OPENAI_API_KEY')
        
        if not openai_api_key:
            self.logger.warning("No OpenAI API key provided. Using fallback heuristic selection.")
            return self._fallback_variable_selection(variable_names, relationships)
        
        # Create prompt for suggesting variables
        suggestion_prompt = f"""
    You are a malaria epidemiology expert tasked with selecting variables for a composite malaria risk score.
    Below are variables with their relationships to malaria risk (direct or inverse):

    {self._format_relationships(variable_names, relationships)}

    Based on epidemiological principles, select 3-5 variables that would create the most informative and balanced 
    malaria risk assessment. Consider these factors:

    1. Include a balanced mix of environmental, demographic, and infrastructure variables if available
    2. Avoid redundant variables (e.g., if two variables measure similar things, choose the more reliable one)
    3. Prioritize variables with strong established relationships to malaria risk
    4. Ensure both direct and inverse relationships are represented if possible
    5. DO NOT include variables with "Urban" or "urban" in their name (these will be handled separately)
    6. Prioritize variables like rainfall, temperature, vegetation indices, housing quality, elevation, and 
       distance to water which are known to influence malaria transmission

    Return ONLY a comma-separated list of the variable names you recommend including.
    """

        try:
            # Prepare messages for the API call
            messages = [
                {"role": "system", "content": "You are a malaria epidemiology expert. Answer with ONLY the recommended variable names as a comma-separated list."},
                {"role": "user", "content": suggestion_prompt}
            ]
            
            # Call OpenAI API
            headers = {
                "Content-Type": "application/json",
                "Authorization": f"Bearer {openai_api_key}"
            }
            
            data = {
                "model": "gpt-4o",  # Using GPT-4o for better domain knowledge
                "messages": messages,
                "temperature": 0.2  # Lower temperature for more consistent results
            }
            
            response = requests.post(
                "https://api.openai.com/v1/chat/completions",
                headers=headers,
                data=json.dumps(data)
            )
            
            # Check for successful response
            if response.status_code == 200:
                # Extract and clean the response
                response_data = response.json()
                response_text = response_data["choices"][0]["message"]["content"]
                
                # Clean up response - remove any explanations, just get the list
                clean_response = self._clean_gpt_response(response_text)
                
                # Split by commas and clean up each variable name
                suggested_vars = [var.strip() for var in clean_response.split(',')]
                
                # Filter out any urban variables (double check)
                suggested_vars = [var for var in suggested_vars if 'urban' not in var.lower()]
                
                # Verify suggestions are valid variables
                valid_suggestions = [var for var in suggested_vars if var in variable_names]
                
                if len(valid_suggestions) >= 2:
                    self.logger.info(f"LLM suggested variables: {', '.join(valid_suggestions)}")
                    return valid_suggestions
                else:
                    self.logger.warning(f"LLM didn't suggest enough valid variables: {valid_suggestions}. Using fallback.")
                    return self._fallback_variable_selection(variable_names, relationships)
                    
            else:
                self.logger.error(f"Error from OpenAI API: {response.status_code} - {response.text}")
                return self._fallback_variable_selection(variable_names, relationships)
                
        except Exception as e:
            self.logger.error(f"Error in suggest_composite_variables: {str(e)}")
            return self._fallback_variable_selection(variable_names, relationships)

    def _clean_gpt_response(self, response_text):
        """
        Clean up the GPT response to extract just the comma-separated variable list
        """
        # Remove any markdown formatting or extra text
        lines = response_text.strip().split("\n")
        
        # Look for lines that contain commas - likely our variable list
        comma_lines = [line for line in lines if ',' in line]
        
        if comma_lines:
            # Use the first line with commas
            return comma_lines[0]
        
        # If no commas found, just return the whole response
        return response_text.strip()

    def _format_relationships(self, variable_names, relationships):
        """
        Format the variable relationships for the prompt
        """
        formatted_list = []
        
        for var in variable_names:
            if var in relationships:
                rel = relationships[var]
                formatted_list.append(f"{var} ({rel})")
            else:
                # Assume direct relationship if not specified
                formatted_list.append(f"{var} (direct)")
        
        return "\n".join(formatted_list)

    def _fallback_variable_selection(self, variable_names, relationships):
        """
        Fallback method for selecting variables when LLM is not available
        Uses domain knowledge and heuristics to select variables
        """
        # List of terms commonly associated with malaria
        malaria_relevant_terms = [
            'rainfall', 'precipitation', 'humidity', 'temperature', 'moisture',
            'vegetation', 'ndvi', 'evi', 'forest', 'canopy',
            'water', 'wetness', 'breeding', 'swamp', 'river', 'lake',
            'elevation', 'altitude', 'slope', 'terrain',
            'housing', 'building', 'settlement', 'dwelling',
            'population', 'density', 'poverty',
            'malaria', 'parasite', 'pfpr', 'tpr', 'incidence', 'prevalence'
        ]
        
        # Score each variable based on relevance
        scored_vars = []
        for var in variable_names:
            var_lower = var.lower()
            
            # Skip urban variables
            if 'urban' in var_lower:
                continue
                
            # Count matches with relevant terms
            score = sum(1 for term in malaria_relevant_terms if term in var_lower)
            
            # Boost score for particularly important variables
            if any(x in var_lower for x in ['malaria', 'parasite', 'pfpr', 'tpr']):
                score += 5
            elif any(x in var_lower for x in ['rainfall', 'precipitation', 'temperature']):
                score += 3
            elif any(x in var_lower for x in ['housing', 'elevation', 'vegetation']):
                score += 2
                
            scored_vars.append((var, score))
        
        # Sort by relevance score descending
        scored_vars.sort(key=lambda x: x[1], reverse=True)
        
        # Take top 5 or fewer if not enough
        top_vars = [var for var, score in scored_vars[:5] if score > 0]
        
        # If not enough relevant variables found, just take the first 5 (excluding urban ones)
        if len(top_vars) < 2:
            non_urban_vars = [var for var in variable_names if 'urban' not in var.lower()]
            top_vars = non_urban_vars[:5]
        
        # Ensure we have at least 2 variables but no more than 5
        return top_vars[:5] if len(top_vars) > 5 else top_vars
    
    def compute_composite_scores(self, selected_variables=None, method='mean'):
       """
       Calculate composite scores using LLM-selected or user-specified normalized variables
       
       Args:
           selected_variables: List of variables to use (if None, selects using LLM)
           method: Aggregation method ('mean')
           
       Returns:
           dict: Status and information about the composite scores
       """
       self.logger.info("===== STARTING COMPOSITE SCORE CALCULATION =====")
       if self.normalized_data is None:
           self.logger.info("Normalized data not found. Running normalization first...")
           norm_result = self.normalize_data()
           if norm_result['status'] != 'success':
               self.logger.error(f"ERROR: Normalization failed - {norm_result['message']}")
               return norm_result
           self.logger.info("Normalization completed successfully.")
           
       try:
           # Get normalized columns (starting with "normalization_")
           norm_cols = [col for col in self.normalized_data.columns if col.startswith('normalization_')]
           self.logger.info(f"Found {len(norm_cols)} normalized columns: {', '.join(norm_cols)}")
           
           # Extract original variable names from normalized columns
           var_names = [col.replace('normalization_', '') for col in norm_cols]
           
           # If specific variables are selected, process and validate them
           if selected_variables:
               self.logger.info(f"Processing user-selected variables: {', '.join(selected_variables)}")
               selected_norm_cols = []
               
               # For each selected variable, try to find matching normalized column
               for var in selected_variables:
                   # Try exact match with normalized column
                   norm_col = f"normalization_{var.lower()}"
                   if norm_col in norm_cols:
                       selected_norm_cols.append(norm_col)
                       self.logger.info(f"  - Found exact match: {var} -> {norm_col}")
                   # Try exact match with already normalized name
                   elif var in norm_cols:
                       selected_norm_cols.append(var)
                       self.logger.info(f"  - Found exact match with normalized column: {var}")
                   # Try case-insensitive match
                   else:
                       # Try with normalized prefix
                       for col in norm_cols:
                           if col.lower() == f"normalization_{var.lower()}":
                               selected_norm_cols.append(col)
                               self.logger.info(f"  - Found case-insensitive match: {var} -> {col}")
                               break
                       else:
                           # Try matching against original variable names
                           for i, name in enumerate(var_names):
                               if name.lower() == var.lower():
                                   selected_norm_cols.append(norm_cols[i])
                                   self.logger.info(f"  - Found match with original variable: {var} -> {norm_cols[i]}")
                                   break
                           else:
                               # Try partial matches
                               for col in norm_cols:
                                   if var.lower() in col.lower():
                                       selected_norm_cols.append(col)
                                       self.logger.info(f"  - Found partial match: {var} -> {col}")
                                       break
                               
               # Remove duplicates while preserving order
               selected_norm_cols = list(dict.fromkeys(selected_norm_cols))
               self.logger.info(f"After processing, using {len(selected_norm_cols)} columns: {', '.join(selected_norm_cols)}")
               
               # Update norm_cols with validated selection
               norm_cols = selected_norm_cols
               
               # Save the composite variables selection for future use
               self.composite_variables = [col.replace('normalization_', '') for col in norm_cols]
               
           else:
               # Use LLM to select the best 3-5 variables
               # Get OpenAI API key from environment or settings
               openai_api_key = os.environ.get('OPENAI_API_KEY')
               
               selected_vars = self.suggest_composite_variables(var_names, self.variable_relationships, openai_api_key)
               self.logger.info(f"LLM selected variables: {', '.join(selected_vars)}")
               self.composite_variables = selected_vars
               
               # Convert selected variable names to normalized column names
               selected_norm_cols = [f"normalization_{var.lower()}" for var in selected_vars 
                                   if f"normalization_{var.lower()}" in norm_cols]
               norm_cols = selected_norm_cols
           
           # Need at least 2 variables for composite score
           if len(norm_cols) < 2:
               self.logger.error(f"ERROR: Need at least 2 normalized variables. Found {len(norm_cols)}.")
               return {
                   'status': 'error',
                   'message': f'Need at least 2 normalized variables. Found {len(norm_cols)}.'
               }
           
           self.logger.info(f"Using {len(norm_cols)} variables for composite scores: {', '.join(norm_cols)}")
           
           # Initialize result dataframe with WardName
           result = pd.DataFrame({'WardName': self.normalized_data['WardName']})
           
           # Generate all combinations
           combinations = []
           for r in range(2, len(norm_cols) + 1):
               combinations.extend(list(itertools.combinations(norm_cols, r)))
           
           self.logger.info(f"Created a total of {len(combinations)} combinations")
           
           # Create a list to store model formulas
           model_formulas = []
           
           # Calculate composite score for each combination
           for i, combo in enumerate(combinations):
               if i % 10 == 0:  # Print progress every 10 combinations
                   self.logger.info(f"  - Processing combination {i+1}/{len(combinations)} ({(i+1)/len(combinations)*100:.1f}%)")
                   
               model_name = f"model_{i+1}"
               
               # Simple mean of normalized values
               result[model_name] = self.normalized_data[list(combo)].mean(axis=1)
               
               # Store model formula
               variables_used = [col.replace('normalization_', '') for col in combo]
               model_formulas.append({
                   'model': model_name,
                   'variables': variables_used
               })
           
           self.logger.info("Creating DataFrame with all scores (avoiding fragmentation)...")
           
           # Convert any int64, float64 values to int, float for better JSON compatibility
           # Convert all NumPy types to native Python types
           for col in result.columns:
               if col != 'WardName':
                   if np.issubdtype(result[col].dtype, np.integer):
                       result[col] = result[col].astype(int)
                   elif np.issubdtype(result[col].dtype, np.floating):
                       result[col] = result[col].astype(float)
           
           # Store composite scores using the new structure
           self.composite_scores = {
               'scores': result,
               'model_formulas': model_formulas
           }
           
           self.logger.info("Saving composite scores to CSV...")
           # Save composite scores
           result.to_csv(os.path.join(self.session_folder, 'composite_scores.csv'), index=False)
           
           self.logger.info("Saving model formulas...")
           # Also save model formulas for reference
           formulas_df = pd.DataFrame(model_formulas)
           # Convert lists of variables to comma-separated strings for CSV
           formulas_df['variables'] = formulas_df['variables'].apply(lambda x: ','.join(x) if isinstance(x, list) else x)
           formulas_df.to_csv(
               os.path.join(self.session_folder, 'model_formulas.csv'),
               index=False
           )
           
           self.logger.info("===== COMPLETED COMPOSITE SCORE CALCULATION SUCCESSFULLY =====")
           return {
               'status': 'success',
               'message': f'Successfully calculated {len(combinations)} composite score models',
               'models': len(combinations),
               'variables_used': selected_variables or self.composite_variables
           }
           
       except Exception as e:
           self.logger.error(f"ERROR in composite score calculation: {str(e)}")
           return {
               'status': 'error',
               'message': f'Error calculating composite scores: {str(e)}'
           }
   
    def calculate_vulnerability_rankings(self, n_categories=3):
        """
        Calculate vulnerability rankings based on composite scores
        
        Args:
            n_categories: Number of vulnerability categories
            
        Returns:
            dict: Status and vulnerability ranking information
        """
        if self.composite_scores is None:
            return {
                'status': 'error',
                'message': 'No composite scores calculated'
            }
        
        try:
            # Get all model columns
            model_cols = [col for col in self.composite_scores['scores'].columns 
                            if col.startswith('model_')]
            
            # Calculate median rank for each ward across all models
            scores_df = self.composite_scores['scores']
            
            # Initialize rankings dataframe
            rankings = scores_df[['WardName']].copy()
            
            # Calculate median composite score for each ward
            rankings['median_score'] = scores_df[model_cols].median(axis=1)
            
            # Order by median score (descending) to get overall rank
            rankings = rankings.sort_values('median_score', ascending=False)
            rankings['overall_rank'] = range(1, len(rankings) + 1)
            
            # Reset index
            rankings = rankings.reset_index(drop=True)
            
            # Add ward vulnerability category
            n_wards = len(rankings)
            category_bins = np.linspace(0, n_wards, n_categories + 1).astype(int)
            category_labels = ['High', 'Medium', 'Low'][:n_categories]
            
            rankings['vulnerability_category'] = pd.cut(
                rankings['overall_rank'],
                bins=category_bins,
                labels=category_labels,
                include_lowest=True
            )
            
            # Ensure all columns are Python native types, not NumPy types
            for col in rankings.columns:
                if col != 'WardName' and col != 'vulnerability_category':
                    if np.issubdtype(rankings[col].dtype, np.integer):
                        rankings[col] = rankings[col].astype(int)
                    elif np.issubdtype(rankings[col].dtype, np.floating):
                        rankings[col] = rankings[col].astype(float)
            
            # Save rankings
            self.vulnerability_rankings = rankings
            rankings.to_csv(os.path.join(self.session_folder, 'vulnerability_rankings.csv'), index=False)
            
            return {
                'status': 'success',
                'message': f'Successfully ranked {len(rankings)} wards by vulnerability',
                'vulnerable_wards': rankings['WardName'].tolist()
            }
            
        except Exception as e:
            self.logger.error(f"Error calculating vulnerability rankings: {str(e)}")
            return {
                'status': 'error',
                'message': f'Error calculating vulnerability rankings: {str(e)}'
            }
    
    def process_urban_extent(self, thresholds=None):
        """
        Process urban extent analysis at different thresholds
        
        Args:
            thresholds: List of urban percentage thresholds to analyze
                        If None, use default thresholds [30, 50, 75, 100]
                        
        Returns:
            dict: Status and urban extent information
        """
        if self.shapefile_data is None:
            return {
                'status': 'error',
                'message': 'No shapefile data loaded'
            }
        
        # Find urban percentage column, checking for truncated names
        urban_col = None
        for col_name in ['UrbanPercent', 'UrbanPerce', 'Urban_Percent', 'urban_percent']:
            if col_name in self.shapefile_data.columns:
                urban_col = col_name
                break
        
        if not urban_col:
            return {
                'status': 'error',
                'message': 'Urban Percentage column not found in shapefile data'
            }
        
        if thresholds is None:
            thresholds = [30, 50, 75, 100]
        
        try:
            # Process each threshold
            urban_extent_results = {}
            
            for threshold in thresholds:
                # Add threshold classification column
                result_df = self.shapefile_data.copy()
                meets_threshold_field = f'MeetsThreshold_{threshold}'
                result_df[meets_threshold_field] = result_df[urban_col] >= threshold
                
                # Count wards meeting/not meeting threshold
                meets_count = result_df[result_df[meets_threshold_field]].shape[0]
                not_meets_count = result_df[~result_df[meets_threshold_field]].shape[0]
                
                # Store in results
                urban_extent_results[threshold] = {
                    'meets_threshold': meets_count,
                    'below_threshold': not_meets_count,
                    'meets_threshold_wards': result_df[result_df[meets_threshold_field]]['WardName'].tolist(),
                    'below_threshold_wards': result_df[~result_df[meets_threshold_field]]['WardName'].tolist()
                }
                
                # Save threshold results
                result_df.to_file(os.path.join(self.session_folder, f'urban_extent_{threshold}.shp'))
            
            # Store urban extent results
            self.urban_extent_results = urban_extent_results
            
            # Save summary as CSV for easy reference
            summary_rows = []
            for threshold, results in urban_extent_results.items():
                summary_rows.append({
                    'threshold': threshold,
                    'meets_threshold': results['meets_threshold'],
                    'below_threshold': results['below_threshold']
                })
            
            summary_df = pd.DataFrame(summary_rows)
            summary_df.to_csv(os.path.join(self.session_folder, 'urban_extent_summary.csv'), index=False)
            
            return {
                'status': 'success',
                'message': f'Successfully analyzed urban extent at {len(thresholds)} thresholds',
                'thresholds': thresholds,
                'results': urban_extent_results
            }
            
        except Exception as e:
            self.logger.error(f"Error processing urban extent: {str(e)}")
            return {
                'status': 'error',
                'message': f'Error processing urban extent: {str(e)}'
            }
    
    def run_full_analysis(self, selected_variables=None, na_methods=None, custom_relationships=None):
        """
        Run the complete analysis pipeline with optional custom parameters
        
        Args:
            selected_variables: List of variables to use for composite scores (optional)
            na_methods: Dict mapping columns to methods for handling missing values (optional)
            custom_relationships: Dict mapping variables to relationships (direct/inverse) (optional)
            
        Returns:
            Dict with analysis results
        """
        try:
            # Track which steps need to be re-run
            rerun_stages = {
                'clean': True,
                'relationships': True,
                'normalize': True,
                'composite': True,
                'ranking': True,
                'urban': True
            }
            
            # If we already have cleaned data and no custom NA methods, skip cleaning
            if self.cleaned_data is not None and na_methods is None:
                rerun_stages['clean'] = False
                self.logger.info("Reusing existing cleaned data (no custom NA methods specified)")
            
            # If we already have variable relationships and no custom relationships, skip relationship determination
            if self.variable_relationships and not custom_relationships:
                rerun_stages['relationships'] = False
                self.logger.info("Reusing existing variable relationships (no custom relationships specified)")
            
            # If we have normalized data, custom relationships, but no custom NA methods, 
            # rerun from normalization stage using existing cleaned data
            if self.normalized_data is not None and custom_relationships and not na_methods:
                # Need to rerun normalization due to relationship changes
                rerun_stages['clean'] = False
                rerun_stages['relationships'] = True
                self.logger.info("Reusing cleaned data but redetermining relationships and renormalizing")
            
            # If selected_variables is provided but no other customizations,
            # reuse everything up to composite score calculation
            if selected_variables and not custom_relationships and not na_methods and self.normalized_data is not None:
                rerun_stages['clean'] = False
                rerun_stages['relationships'] = False
                rerun_stages['normalize'] = False
                self.logger.info("Only rerunning composite score calculation with selected variables")
            
            # 1. Clean data if needed
            self.logger.info("Step 1: Cleaning data")
            clean_result = None
            if rerun_stages['clean']:
                clean_result = self.clean_data(na_methods)
                if clean_result['status'] != 'success':
                    return clean_result
            else:
                clean_result = {'status': 'success', 'message': 'Using previously cleaned data'}
            
            # 2. Determine variable relationships if needed
            self.logger.info("Step 2: Determining variable relationships")
            if rerun_stages['relationships']:
                self.determine_variable_relationships()
                
                # Apply custom relationships if provided
                if custom_relationships:
                    self.logger.info(f"Applying custom variable relationships: {custom_relationships}")
                    for var, rel in custom_relationships.items():
                        if var in self.variable_relationships:
                            old_rel = self.variable_relationships[var]
                            self.variable_relationships[var] = rel
                            self.logger.info(f"Changed relationship for {var} from {old_rel} to {rel}")
                        else:
                            self.variable_relationships[var] = rel
                            self.logger.info(f"Added new relationship for {var}: {rel}")
            
            # 3. Normalize data if needed
            self.logger.info("Step 3: Normalizing data")
            norm_result = None
            if rerun_stages['normalize']:
                norm_result = self.normalize_data()
                if norm_result['status'] != 'success':
                    return norm_result
            else:
                norm_result = {'status': 'success', 'message': 'Using previously normalized data'}
            
            # 4. Calculate composite scores
            self.logger.info("Step 4: Calculating composite scores")
            composite_result = None
            if rerun_stages['composite'] or selected_variables:
                composite_result = self.compute_composite_scores(selected_variables)
                if composite_result['status'] != 'success':
                    return composite_result
            else:
                if hasattr(self, 'composite_scores') and self.composite_scores:
                    composite_result = {
                        'status': 'success', 
                        'message': 'Using previously calculated composite scores',
                        'variables_used': self.composite_variables or []
                    }
                else:
                    # If we don't have composite scores yet, calculate them
                    composite_result = self.compute_composite_scores(selected_variables)
                    if composite_result['status'] != 'success':
                        return composite_result
            
            # 5. Calculate vulnerability rankings
            self.logger.info("Step 5: Calculating vulnerability rankings")
            ranking_result = None
            if rerun_stages['ranking'] or selected_variables:
                ranking_result = self.calculate_vulnerability_rankings()
                if ranking_result['status'] != 'success':
                    return ranking_result
            else:
                if hasattr(self, 'vulnerability_rankings') and self.vulnerability_rankings is not None:
                    ranking_result = {
                        'status': 'success', 
                        'message': 'Using previously calculated vulnerability rankings',
                        'vulnerable_wards': self.vulnerability_rankings['WardName'].tolist()[:5] if 'WardName' in self.vulnerability_rankings.columns else []
                    }
                else:
                    ranking_result = self.calculate_vulnerability_rankings()
                    if ranking_result['status'] != 'success':
                        return ranking_result
            
            # 6. Process urban extent
            self.logger.info("Step 6: Processing urban extent")
            urban_result = None
            if rerun_stages['urban']:
                urban_result = self.process_urban_extent()
                if urban_result['status'] != 'success':
                    return urban_result
            else:
                if hasattr(self, 'urban_extent_results') and self.urban_extent_results:
                    urban_result = {
                        'status': 'success', 
                        'message': 'Using previously calculated urban extent results'
                    }
                else:
                    urban_result = self.process_urban_extent()
                    if urban_result['status'] != 'success':
                        return urban_result
            
            # Compile summary of all steps
            self.logger.info("Analysis pipeline complete")
            summary = {
                'status': 'success',
                'message': 'Complete analysis pipeline successfully executed',
                'steps': {
                    'clean': clean_result,
                    'normalize': norm_result,
                    'composite': composite_result,
                    'ranking': ranking_result,
                    'urban': urban_result
                },
                'variables_used': composite_result.get('variables_used', []),
                'vulnerable_wards': ranking_result.get('vulnerable_wards', [])
            }
            
            return summary
            
        except Exception as e:
            self.logger.error(f"Error in full analysis pipeline: {str(e)}")
            return {
                'status': 'error',
                'message': f'Error in full analysis pipeline: {str(e)}'
            }
    
    # Helper Methods
    
    def _handle_duplicate_wardnames(self, df):
        """
        Handle duplicate ward names in a dataframe
        
        Args:
            df: Dataframe with potentially duplicate ward names
            
        Returns:
            Dataframe with unique ward names
        """
        # Check if WardName column exists
        if 'WardName' not in df.columns:
            self.logger.warning("WardName column not found, cannot handle duplicates")
            return df
        
        # Check if WardCode column exists
        if 'WardCode' not in df.columns:
            self.logger.warning("WardCode column not found, cannot disambiguate duplicates")
            return df
        
        # Find duplicate ward names
        duplicate_mask = df['WardName'].duplicated(keep=False)
        if not duplicate_mask.any():
            return df  # No duplicates found
        
        # Get duplicate ward names
        duplicate_wards = df.loc[duplicate_mask, 'WardName'].unique()
        
        # For each duplicate ward name, create unique names using WardCode_WardName
        for ward in duplicate_wards:
            # Find indices of rows with this ward name
            indices = df[df['WardName'] == ward].index
            
            # Rename only if there's more than one occurrence
            if len(indices) > 1:
                for idx in indices:
                    # Create new name using WardCode_WardName
                    df.loc[idx, 'WardName'] = f"{df.loc[idx, 'WardCode']}_{df.loc[idx, 'WardName']}"
        
        return df
    
    def _check_missing_values(self, df):
        """
        Check for columns with missing values
        
        Args:
            df: Dataframe to check
            
        Returns:
            list: Names of columns with missing values
        """
        cols_with_missing = []
        
        for col in df.columns:
            if df[col].isna().any():
                cols_with_missing.append(col)
        
        return cols_with_missing
    
    def _handle_na_spatial_mean(self, df, column):
        """
        Handle missing values using spatial neighbor mean with improved robust matching.
        This implementation is more robust by:
        1. Creating a bidirectional mapping between ward names and indices
        2. Using position-based weights properly while maintaining ward name lookups
        3. Adding extensive error handling and fallback options

        Args:
            df: DataFrame with missing values (will be modified in place)
            column: Column name to process

        Returns:
            bool: True if spatial imputation was successfully attempted, False otherwise
        """
        if column not in df.columns or self.shapefile_data is None:
            self.logger.warning(f"Cannot use spatial method for {column} (missing column or shapefile)")
            return False # Indicate spatial method was not possible

        if not pd.api.types.is_numeric_dtype(df[column]):
            self.logger.info(f"Column {column} is not numeric. Spatial mean not applicable.")
            return False # Indicate spatial method was not applicable

        self.logger.info(f"Attempting spatial neighbor mean imputation for column: {column}...")

        try:
            # Ensure WardName exists in both dataframes
            if 'WardName' not in df.columns or 'WardName' not in self.shapefile_data.columns:
                self.logger.warning(f"WardName column missing for spatial imputation of {column}. Cannot proceed.")
                return False

            # --- Create a ward-to-position mapping for both datasets ---
            # This is crucial for correct matching between datasets
            csv_ward_to_index = {ward: idx for idx, ward in enumerate(df['WardName'])}
            shp_ward_to_index = {ward: idx for idx, ward in enumerate(self.shapefile_data['WardName'])}
            
            # Also create reverse mappings for shapefile
            shp_index_to_ward = {idx: ward for ward, idx in shp_ward_to_index.items()}

            # --- Create spatial weights using shapefile ---
            # Don't modify the original shapefile
            temp_shp = self.shapefile_data.copy()
            
            # Create Queen contiguity weights
            w = Queen.from_dataframe(temp_shp)
            
            # --- Find missing values to impute ---
            missing_indices = df.index[df[column].isna()]
            missing_wards = df.loc[missing_indices, 'WardName'].tolist()
            self.logger.info(f"  - Found {len(missing_wards)} missing values in {column} to impute.")

            # --- Track results for reporting ---
            successful_imputation = 0
            neighbor_mean_count = 0
            no_neighbors_count = 0
            no_valid_neighbor_values_count = 0
            not_in_shapefile_count = 0
            global_mean_fallback_count = 0

            # --- Calculate global mean for fallback ---
            global_mean = df[column].mean()
            if pd.isna(global_mean):
                global_mean = 0 # Use 0 as last resort fallback
                self.logger.warning(f"  - All values in {column} are NA. Using 0 as fallback.")

            # --- Process each missing value ---
            for idx in missing_indices:
                ward_name = df.loc[idx, 'WardName']
                
                # Check if this ward exists in the shapefile
                if ward_name in shp_ward_to_index:
                    # Get shapefile index for this ward
                    shp_idx = shp_ward_to_index[ward_name]
                    
                    # Get neighbor indices from weights
                    neighbor_indices = w.neighbors[shp_idx]
                    
                    if neighbor_indices:
                        # Convert shapefile neighbor indices to ward names
                        neighbor_wards = [shp_index_to_ward[i] for i in neighbor_indices]
                        
                        # Find these wards in the CSV data
                        neighbor_values = []
                        for nward in neighbor_wards:
                            if nward in csv_ward_to_index:
                                # Get the value for this ward from the CSV
                                csv_idx = csv_ward_to_index[nward]
                                val = df.iloc[csv_idx][column]
                                if not pd.isna(val):
                                    neighbor_values.append(val)
                        
                        # If we found valid neighbor values, use their mean
                        if neighbor_values:
                            imputed_value = sum(neighbor_values) / len(neighbor_values)
                            df.loc[idx, column] = imputed_value
                            neighbor_mean_count += 1
                            successful_imputation += 1
                        else:
                            # No valid values from neighbors
                            df.loc[idx, column] = global_mean
                            no_valid_neighbor_values_count += 1
                            global_mean_fallback_count += 1
                    else:
                        # Ward has no neighbors
                        df.loc[idx, column] = global_mean
                        no_neighbors_count += 1
                        global_mean_fallback_count += 1
                else:
                    # Ward not found in shapefile
                    df.loc[idx, column] = global_mean
                    not_in_shapefile_count += 1
                    global_mean_fallback_count += 1

            # --- Report results ---
            self.logger.info(f"  - Spatial imputation completed for {column}:")
            self.logger.info(f"    * Total missing values: {len(missing_indices)}")
            self.logger.info(f"    * Successful neighbor mean imputations: {neighbor_mean_count}")
            self.logger.info(f"    * Fallbacks used: {global_mean_fallback_count}")
            self.logger.info(f"      - Wards not in shapefile: {not_in_shapefile_count}")
            self.logger.info(f"      - Wards with no neighbors: {no_neighbors_count}")
            self.logger.info(f"      - Wards with no valid neighbor values: {no_valid_neighbor_values_count}")

            return True # Indicate the spatial method was attempted

        except Exception as e:
            self.logger.error(f"  - ERROR during spatial imputation for {column}: {str(e)}", exc_info=True)
            return False # Indicate spatial method failed
    
    def _handle_na_mean(self, df, column):
        """Imputes missing values with column mean. Returns df or None."""
        if column not in df.columns or not pd.api.types.is_numeric_dtype(df[column]):
            self.logger.warning(f"Cannot apply mean imputation to non-numeric or missing column: {column}")
            return None
        try:
            # Calculate mean excluding NaN values already present
            mean_value = df[column].mean() # Default skipna=True
            if pd.isna(mean_value): # Handle case where all existing values are NaN
                mean_value = 0 # Default to 0 if mean is NaN
                self.logger.warning(f"Column {column} has no valid values to calculate mean. Imputing missing with 0.")
            df[column] = df[column].fillna(mean_value)
            return df
        except Exception as e:
                self.logger.error(f"Error during mean imputation for {column}: {e}")
                return None

    def _handle_na_mode(self, df, column):
        """
        Handle missing values using column mode
        
        Args:
            df: Dataframe with missing values
            column: Column name to process
            
        Returns:
            Dataframe with missing values filled with mode
        """
        if column not in df.columns:
            return df
        
        try:
            mode_result = df[column].mode()
            if not mode_result.empty:
                mode_value = mode_result[0]
                df[column] = df[column].fillna(mode_value)
                return df
            else:
                # Fallback if mode cannot be determined
                self.logger.warning(f"Could not determine mode for column {column}. Attempting ffill/bfill.")
                df[column] = df[column].ffill().bfill()
                # Check if still NaN after ffill/bfill (e.g., all values were NaN)
                if df[column].isna().any():
                    self.logger.warning(f"ffill/bfill failed for column {column}. Imputing with placeholder 'Unknown'.")
                    df[column] = df[column].fillna('Unknown') # Or another appropriate placeholder
                return df
        except Exception as e:
            self.logger.error(f"Error during mode imputation for {column}: {e}")
            # Attempt ffill/bfill as a last resort
            try:
                    df[column] = df[column].ffill().bfill()
                    if df[column].isna().any(): df[column] = df[column].fillna('Unknown')
                    return df
            except:
                    return None
    
    def _get_numeric_columns(self):
        """
        Get names of numeric columns in the cleaned data
        
        Returns:
            list: Names of numeric columns (excluding WardName)
        """
        if self.cleaned_data is None:
            return []
        
        numeric_cols = []
        
        for col in self.cleaned_data.columns:
            if col != 'WardName' and pd.api.types.is_numeric_dtype(self.cleaned_data[col]):
                numeric_cols.append(col)
        
        return numeric_cols