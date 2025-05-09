import os
import logging
import numpy as np
import pandas as pd
import geopandas as gpd
from libpysal.weights import Queen
import matplotlib.pyplot as plt
from scipy import stats

# Set up logging
logger = logging.getLogger(__name__)

def normalize_data(data, relationships, exclude_cols=None):
    """
    Normalize data based on variable relationships with malaria risk
    
    Args:
        data: DataFrame with data to normalize
        relationships: Dict mapping variable names to relationships (direct/inverse)
        exclude_cols: List of columns to exclude from normalization
        
    Returns:
        DataFrame with normalized variables
    """
    try:
        # Create a copy to avoid modifying original
        normalized_df = data.copy()
        
        # Determine columns to normalize
        if 'WardName' in normalized_df.columns:
            # Get numeric columns excluding WardName
            numeric_cols = [col for col in normalized_df.columns 
                          if col != 'WardName' and pd.api.types.is_numeric_dtype(normalized_df[col])]
        else:
            numeric_cols = [col for col in normalized_df.columns 
                          if pd.api.types.is_numeric_dtype(normalized_df[col])]
        
        # Exclude specified columns
        if exclude_cols:
            numeric_cols = [col for col in numeric_cols if col not in exclude_cols]
        
        for col in numeric_cols:
            if col in relationships:
                # Get column values as numpy array for faster processing
                values = normalized_df[col].values
                
                # Skip columns with all same values (would cause division by zero)
                if np.all(values == values[0]):
                    logger.warning(f"Column {col} has all identical values. Skipping normalization.")
                    normalized_df[f"normalization_{col.lower()}"] = 0.5  # Default to middle value
                    continue
                
                # Handle relationship type
                if relationships[col] == 'inverse':
                    # For inverse relationship, invert values then normalize
                    # Add small constant to avoid division by zero
                    inverted = 1 / (values + 1e-10)
                    
                    # Normalize inverted values
                    inv_min, inv_max = np.min(inverted), np.max(inverted)
                    if inv_min == inv_max:
                        normalized = np.full_like(inverted, 0.5)  # Default to middle value
                    else:
                        normalized = (inverted - inv_min) / (inv_max - inv_min)
                    
                else:  # direct relationship
                    # Normalize directly
                    min_val, max_val = np.min(values), np.max(values)
                    if min_val == max_val:
                        normalized = np.full_like(values, 0.5)  # Default to middle value
                    else:
                        normalized = (values - min_val) / (max_val - min_val)
                
                # Store normalized values with standardized column name
                normalized_df[f"normalization_{col.lower()}"] = normalized
        
        return normalized_df
    
    except Exception as e:
        logger.error(f"Error in normalize_data: {str(e)}")
        raise
        
def determine_variable_relationships(variables, descriptions=None):
    """
    Determine relationship of each variable with malaria risk
    
    Args:
        variables: List of variable names
        descriptions: Optional dict of variable descriptions to help determine relationships
        
    Returns:
        Dict mapping variable names to relationships (direct/inverse)
    """
    # Keywords that typically indicate inverse relationship with malaria risk
    inverse_keywords = [
        'distance', 'elevation', 'altitude', 'slope', 'housing', 'quality',
        'income', 'education', 'access', 'urban', 'facility'
    ]
    
    # Keywords that typically indicate direct relationship with malaria risk
    direct_keywords = [
        'rainfall', 'precipitation', 'humidity', 'temperature', 'vegetation',
        'poverty', 'population', 'density', 'water', 'breeding'
    ]
    
    relationships = {}
    
    for var in variables:
        var_lower = var.lower()
        
        # Check description first if available
        relationship_found = False
        if descriptions and var in descriptions:
            desc = descriptions[var].lower()
            
            # Check each keyword in description
            for keyword in inverse_keywords:
                if keyword in desc:
                    relationships[var] = 'inverse'
                    relationship_found = True
                    break
                    
            if not relationship_found:
                for keyword in direct_keywords:
                    if keyword in desc:
                        relationships[var] = 'direct'
                        relationship_found = True
                        break
        
        # If no relationship found from description, check variable name
        if not relationship_found:
            if any(keyword in var_lower for keyword in inverse_keywords):
                relationships[var] = 'inverse'
            else:
                # Default to direct relationship
                relationships[var] = 'direct'
    
    return relationships

def handle_missing_values(data, methods=None, shapefile=None):
    """
    Handle missing values using specified methods
    
    Args:
        data: DataFrame with missing values
        methods: Dict mapping column names to cleaning methods
                 ('mean', 'mode', 'spatial', 'knn')
        shapefile: GeoDataFrame with spatial information (required for spatial method)
        
    Returns:
        DataFrame with missing values handled
    """
    # Create a copy to avoid modifying original
    cleaned_df = data.copy()
    
    # Find columns with missing values
    cols_with_missing = [col for col in cleaned_df.columns if cleaned_df[col].isna().any()]
    
    if not cols_with_missing:
        return cleaned_df  # No missing values
    
    # Default method if none specified
    if methods is None:
        methods = {}
    
    # Process each column with missing values
    for col in cols_with_missing:
        # Determine method for this column
        if col in methods:
            method = methods[col]
        else:
            # Select default method based on data type
            if pd.api.types.is_numeric_dtype(cleaned_df[col]):
                method = 'mean'
            else:
                method = 'mode'
        
        # Apply appropriate method
        if method == 'spatial' and shapefile is not None:
            cleaned_df = handle_spatial_imputation(cleaned_df, col, shapefile)
        elif method == 'knn':
            cleaned_df = handle_knn_imputation(cleaned_df, col)
        elif method == 'mode':
            cleaned_df = handle_mode_imputation(cleaned_df, col)
        else:  # Default to mean for numeric, mode for non-numeric
            if pd.api.types.is_numeric_dtype(cleaned_df[col]):
                cleaned_df = handle_mean_imputation(cleaned_df, col)
            else:
                cleaned_df = handle_mode_imputation(cleaned_df, col)
    
    return cleaned_df

def handle_mean_imputation(data, column):
    """
    Impute missing values with column mean
    
    Args:
        data: DataFrame with missing values
        column: Column name to process
        
    Returns:
        DataFrame with missing values imputed
    """
    if not pd.api.types.is_numeric_dtype(data[column]):
        return data  # Skip non-numeric columns
    
    # Calculate mean excluding NaN
    mean_value = data[column].mean()
    
    # Impute missing values
    data[column] = data[column].fillna(mean_value)
    
    return data

def handle_mode_imputation(data, column):
    """
    Impute missing values with column mode (most frequent value)
    
    Args:
        data: DataFrame with missing values
        column: Column name to process
        
    Returns:
        DataFrame with missing values imputed
    """
    # Get mode (most frequent value)
    mode_value = data[column].mode()[0]
    
    # Impute missing values
    data[column] = data[column].fillna(mode_value)
    
    return data

def handle_spatial_imputation(data, column, shapefile):
    """
    Impute missing values using spatial neighbors
    
    Args:
        data: DataFrame with missing values
        column: Column name to process
        shapefile: GeoDataFrame with spatial information
        
    Returns:
        DataFrame with missing values imputed using spatial information
    """
    if not pd.api.types.is_numeric_dtype(data[column]):
        return handle_mode_imputation(data, column)  # Fall back
    try:
        # Check required columns
        if 'WardName' not in data.columns or 'WardName' not in shapefile.columns:
            return handle_mean_imputation(data, column)  # Fall back to mean
        
        # Merge data with shapefile
        merged = shapefile.merge(data[['WardName', column]], on='WardName', how='inner')
        
        # Create spatial weights using queen contiguity
        weights = Queen.from_dataframe(merged)
        
        # Create copy to avoid overwriting original
        result = data.copy()
        
        # Find rows with missing values
        missing_indices = result.index[result[column].isna()]
        
        for idx in missing_indices:
            # Get ward name
            ward_name = result.loc[idx, 'WardName']
            
            # Find ward in merged dataframe
            merged_idx = merged[merged['WardName'] == ward_name].index
            
            if len(merged_idx) == 0:
                # Ward not found in spatial data, use mean
                result.loc[idx, column] = result[column].mean()
                continue
            
            merged_idx = merged_idx[0]
            
            # Get neighbor indices
            neighbor_indices = weights.neighbors[merged_idx]
            
            if not neighbor_indices:
                # No neighbors, use mean
                result.loc[idx, column] = result[column].mean()
                continue
            
            # Get neighbor values
            neighbor_values = merged.iloc[neighbor_indices][column].dropna()
            
            if len(neighbor_values) == 0:
                # No valid neighbor values, use mean
                result.loc[idx, column] = result[column].mean()
            else:
                # Use mean of neighbors
                result.loc[idx, column] = neighbor_values.mean()
        
        return result
    
    except Exception as e:
        logger.error(f"Error in spatial imputation for {column}: {str(e)}")
        return handle_mean_imputation(data, column)  # Fall back to mean

def handle_knn_imputation(data, column, k=5):
    """
    Impute missing values using K-nearest neighbors
    
    Args:
        data: DataFrame with missing values
        column: Column name to process
        k: Number of neighbors to consider
        
    Returns:
        DataFrame with missing values imputed using KNN
    """
    try:
        from sklearn.impute import KNNImputer
        
        # Create a copy
        result = data.copy()
        
        # Get numeric columns (for feature matrix)
        numeric_cols = [col for col in result.columns 
                       if pd.api.types.is_numeric_dtype(result[col])]
        
        # Skip non-numeric columns
        if column not in numeric_cols:
            return handle_mode_imputation(result, column)
        
        # Create imputer
        imputer = KNNImputer(n_neighbors=k)
        
        # Create feature matrix
        X = result[numeric_cols].copy()
        
        # Impute values
        X_imputed = imputer.fit_transform(X)
        
        # Update the result with imputed values for target column
        col_idx = numeric_cols.index(column)
        result[column] = X_imputed[:, col_idx]
        
        return result
    
    except Exception as e:
        logger.error(f"Error in KNN imputation for {column}: {str(e)}")
        return handle_mean_imputation(data, column)  # Fall back to mean

def compute_composite_scores(normalized_data, selected_vars=None, method='mean'):
    """
    Calculate composite scores using selected normalized variables
    
    Args:
        normalized_data: DataFrame with normalized variables
        selected_vars: List of variables to use (if None, use all normalized variables)
        method: Aggregation method ('mean', 'weighted_mean', 'pca')
        
    Returns:
        DataFrame with composite scores
    """
    try:
        # Make sure WardName column is present
        if 'WardName' not in normalized_data.columns:
            raise ValueError("WardName column must be present in normalized data")
        
        # Get normalized columns (starting with "normalization_")
        norm_cols = [col for col in normalized_data.columns if col.startswith('normalization_')]
        
        # If specific variables are selected, filter columns
        if selected_vars:
            selected_norm_cols = []
            for var in selected_vars:
                norm_col = f"normalization_{var.lower()}"
                if norm_col in norm_cols:
                    selected_norm_cols.append(norm_col)
                elif var in norm_cols:  # Allow already normalized column names
                    selected_norm_cols.append(var)
            norm_cols = selected_norm_cols
        
        # Need at least 2 variables for composite score
        if len(norm_cols) < 2:
            raise ValueError(f"Need at least 2 normalized variables. Found {len(norm_cols)}.")
        
        # Initialize result dataframe with WardName
        result = pd.DataFrame({'WardName': normalized_data['WardName']})
        
        # Generate all combinations
        import itertools
        
        # Create a list to store model formulas
        model_formulas = []
        
        # If only 2 variables, use just one model
        if len(norm_cols) == 2:
            combinations = [norm_cols]
        else:
            # For 3+ variables, generate all valid combinations
            combinations = []
            for r in range(2, len(norm_cols) + 1):
                combinations.extend(list(itertools.combinations(norm_cols, r)))
        
        # Calculate composite score for each combination
        for i, combo in enumerate(combinations):
            model_name = f"model_{i+1}"
            
            # Calculate composite score based on method
            if method == 'mean':
                # Simple mean of normalized values
                result[model_name] = normalized_data[list(combo)].mean(axis=1)
            
            elif method == 'weighted_mean':
                # Weighted mean (equal weights for now)
                weights = np.ones(len(combo)) / len(combo)
                result[model_name] = np.average(normalized_data[list(combo)], axis=1, weights=weights)
            
            elif method == 'pca':
                # Principal Component Analysis
                from sklearn.decomposition import PCA
                from sklearn.preprocessing import StandardScaler
                
                # Standardize data
                X = StandardScaler().fit_transform(normalized_data[list(combo)])
                
                # Apply PCA
                pca = PCA(n_components=1)
                pca_result = pca.fit_transform(X)
                
                # Normalize to 0-1 scale
                min_val = np.min(pca_result)
                max_val = np.max(pca_result)
                normalized_pca = (pca_result - min_val) / (max_val - min_val)
                
                result[model_name] = normalized_pca.flatten()
            
            else:
                # Default to mean
                result[model_name] = normalized_data[list(combo)].mean(axis=1)
            
            # Store model formula
            variables_used = [col.replace('normalization_', '') for col in combo]
            model_formulas.append({
                'model': model_name,
                'variables': variables_used
            })
        
        # Return dictionary with results and formulas
        return {
            'scores': result,
            'model_formulas': model_formulas
        }
    
    except Exception as e:
        logger.error(f"Error computing composite scores: {str(e)}")
        raise

def analyze_vulnerability(composite_scores, n_categories=3):
    """
    Analyze vulnerability based on composite scores
    
    Args:
        composite_scores: Dict with scores DataFrame and model formulas
        n_categories: Number of vulnerability categories
        
    Returns:
        DataFrame with vulnerability rankings
    """
    try:
        # Extract scores dataframe
        scores_df = composite_scores['scores']
        
        # Get model columns
        model_cols = [col for col in scores_df.columns if col.startswith('model_')]
        
        if not model_cols:
            raise ValueError("No model scores found in composite scores")
        
        # Initialize results dataframe with WardName
        result = scores_df[['WardName']].copy()
        
        # Calculate median score across all models
        result['median_score'] = scores_df[model_cols].median(axis=1)
        
        # Sort by median score (descending) to get overall rank
        result = result.sort_values('median_score', ascending=False)
        result['overall_rank'] = range(1, len(result) + 1)
        
        # Reset index
        result = result.reset_index(drop=True)
        
        # Categorize into vulnerability levels
        n_wards = len(result)
        category_bins = np.linspace(0, n_wards, n_categories + 1).astype(int)
        category_labels = ['High', 'Medium', 'Low'][:n_categories]
        
        result['vulnerability_category'] = pd.cut(
            result['overall_rank'],
            bins=category_bins,
            labels=category_labels,
            include_lowest=True
        )
        
        return result
    
    except Exception as e:
        logger.error(f"Error analyzing vulnerability: {str(e)}")
        raise

def analyze_urban_extent(shapefile_data, thresholds=None):
    """
    Analyze urban extent at different thresholds
    
    Args:
        shapefile_data: GeoDataFrame with UrbanPercent column
        thresholds: List of thresholds to analyze (default: [30, 50, 75, 100])
        
    Returns:
        Dict with results for each threshold
    """
    try:
        # Default thresholds if none provided
        if thresholds is None:
            thresholds = [30, 50, 75, 100]
        
        # Check for UrbanPercent column
        if 'UrbanPercent' not in shapefile_data.columns:
            raise ValueError("UrbanPercent column not found in shapefile data")
        
        # Make sure UrbanPercent is numeric
        shapefile_data['UrbanPercent'] = pd.to_numeric(shapefile_data['UrbanPercent'], errors='coerce')
        
        # Initialize results dictionary
        results = {}
        
        # Process each threshold
        for threshold in thresholds:
            # Create threshold classification
            gdf = shapefile_data.copy()
            gdf[f'MeetsThreshold_{threshold}'] = gdf['UrbanPercent'] >= threshold
            
            # Count wards in each category
            meets_count = gdf[gdf[f'MeetsThreshold_{threshold}']].shape[0]
            below_count = gdf[~gdf[f'MeetsThreshold_{threshold}']].shape[0]
            
            # Get ward names for each category
            meets_wards = gdf[gdf[f'MeetsThreshold_{threshold}']]['WardName'].tolist()
            below_wards = gdf[~gdf[f'MeetsThreshold_{threshold}']]['WardName'].tolist()
            
            # Store results
            results[threshold] = {
                'threshold': threshold,
                'meets_threshold': meets_count,
                'below_threshold': below_count,
                'meets_threshold_wards': meets_wards,
                'below_threshold_wards': below_wards
            }
        
        return results
    
    except Exception as e:
        logger.error(f"Error analyzing urban extent: {str(e)}")
        raise

def run_full_analysis_pipeline(data_handler, selected_variables=None, na_methods=None):
    """
    Run the complete analysis pipeline
    
    Args:
        data_handler: DataHandler instance
        selected_variables: List of variables to use for composite scores
        na_methods: Dict mapping columns to methods for handling missing values
        
    Returns:
        Dict with analysis results
    """
    try:
        # 1. Clean data
        logger.info("Step 1: Cleaning data")
        clean_result = data_handler.clean_data(na_methods)
        if clean_result['status'] != 'success':
            return clean_result
        
        # 2. Determine variable relationships
        logger.info("Step 2: Determining variable relationships")
        data_handler.determine_variable_relationships()
        
        # 3. Normalize data
        logger.info("Step 3: Normalizing data")
        norm_result = data_handler.normalize_data()
        if norm_result['status'] != 'success':
            return norm_result
        
        # 4. Calculate composite scores
        logger.info("Step 4: Calculating composite scores")
        composite_result = data_handler.compute_composite_scores(selected_variables)
        if composite_result['status'] != 'success':
            return composite_result
        
        # 5. Calculate vulnerability rankings
        logger.info("Step 5: Calculating vulnerability rankings")
        ranking_result = data_handler.calculate_vulnerability_rankings()
        if ranking_result['status'] != 'success':
            return ranking_result
        
        # 6. Process urban extent
        logger.info("Step 6: Processing urban extent")
        urban_result = data_handler.process_urban_extent()
        if urban_result['status'] != 'success':
            return urban_result
        
        # Compile summary of all steps
        logger.info("Analysis pipeline complete")
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
        logger.error(f"Error in full analysis pipeline: {str(e)}")
        return {
            'status': 'error',
            'message': f'Error in full analysis pipeline: {str(e)}'
        }