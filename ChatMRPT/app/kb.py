# app/kb.py
"""
Knowledge base for the MRPT AI Assistant
Contains methodology explanations and variable rationales
"""

import logging

# Set up logging
logger = logging.getLogger(__name__)

# Methodology explanations
METHODOLOGY_KB = {
    "data_cleaning": {
        "missing_values": """
            Missing values in the dataset are handled through several methods:
            
            1. Spatial Neighbor Mean: For geographic data, missing values are imputed using the average value of adjacent wards (neighbors). This preserves spatial patterns and is the preferred method when possible.
            
            2. Mean Imputation: If spatial methods aren't possible, missing numeric values are replaced with the column mean (average value).
            
            3. Mode Imputation: For categorical variables, missing values are replaced with the most frequent value (mode).
            
            4. Forward/Backward Fill: As a last resort for categorical data without a clear mode, values are propagated forward or backward.
            
            The tool automatically tries these methods in sequence, starting with spatial methods when possible, and falling back to simpler methods as needed.
        """,
        
        "normalization": """
            Normalization is a critical step that converts variables with different units and scales into a common 0-1 range. This allows fair comparison across variables.
            
            The process involves:
            
            1. Determining each variable's relationship with malaria risk (direct or inverse)
            
            2. For direct relationships (where higher values mean higher risk):
               - Formula: (value - min) / (max - min)
            
            3. For inverse relationships (where higher values mean lower risk):
               - Values are first inverted: 1 / (value + small constant)
               - Then normalized: (inverted - min_inverted) / (max_inverted - min_inverted)
            
            This ensures all variables are on a 0-1 scale where 1 always represents higher malaria risk.
        """,
        
        "composite_scores": """
            Composite risk scores combine multiple normalized variables to create an overall malaria risk assessment.
            
            The process involves:
            
            1. Variable Selection: Either default variables (selected by an LLM based on epidemiological literature) or custom variables specified by the user.
            
            2. Model Generation: All possible combinations of the selected variables are created (e.g., with 5 variables, we generate models using pairs, triplets, etc.).
            
            3. Score Calculation: For each model, the normalized values of its variables are averaged to create a composite score between 0-1.
            
            4. Visualization: All models are plotted as separate maps, allowing comparison of how different variable combinations affect risk assessment.
            
            This approach provides robustness by not relying on a single model, and transparency by showing how different variables contribute to risk.
        """,
        
        "vulnerability_ranking": """
            Vulnerability ranking orders wards based on their composite risk scores to identify priority areas.
            
            The methodology:
            
            1. For each ward, calculate the median score across all composite models to get a single representative value.
            
            2. Sort wards by this median score (higher scores = higher vulnerability).
            
            3. Assign ranks (1, 2, 3, etc.) where rank 1 is the most vulnerable.
            
            4. Group wards into vulnerability categories (High, Medium, Low) based on rank terciles.
            
            5. Visualize as a box plot showing the distribution of scores across models for each ward.
            
            The box plot is particularly valuable as it shows not just the median score, but the consistency/uncertainty of the ranking across different variable combinations.
        """,
        
        "urban_extent": """
            Urban extent analysis identifies areas that exceed a specified urbanicity threshold, which is crucial for intervention planning.
            
            The process:
            
            1. Urban percentage values are extracted from the shapefile data for each ward.
            
            2. Wards are classified as "above threshold" or "below threshold" based on a user-specified percentage (default 30%).
            
            3. This classification helps determine appropriate intervention strategies - for example, conventional bed nets might be prioritized for less urban areas.
            
            4. The analysis can be repeated at different thresholds (30%, 50%, 75%) to understand sensitivity to the urban/rural classification.
            
            The urban extent map visually shows which wards exceed the specified urbanicity threshold, helping planners allocate resources appropriately.
        """
    },
    
    "variables": {
        "environmental": """
            Environmental variables capture aspects of the natural environment that influence mosquito breeding and survival:
            
            - Rainfall/Precipitation: Affects standing water availability for mosquito breeding
            - Temperature: Influences mosquito development rate and parasite development
            - Elevation: Higher elevations typically have lower malaria risk
            - NDVI/EVI (vegetation indices): Indicate vegetation density, affecting mosquito habitats
            - Soil moisture/wetness: Reflects potential for water pooling
            - Distance to water bodies: Proximity to breeding sites increases risk
            
            These variables help identify areas with environmental conditions conducive to malaria transmission.
        """,
        
        "demographic": """
            Demographic variables capture human factors that influence malaria risk:
            
            - Population density: Affects human-mosquito contact rates
            - Housing quality: Better housing reduces mosquito entry
            - Urban/rural classification: Urban areas often have lower transmission
            - Access to healthcare: Influences treatment seeking behavior
            - Socioeconomic status: Related to preventive measures and housing
            
            These variables help identify vulnerable populations and social risk factors.
        """,
        
        "epidemiological": """
            Epidemiological variables directly measure malaria burden:
            
            - Parasite rate (PfPR): Percentage of population infected with malaria parasites
            - Test positivity rate (TPR): Percentage of diagnostic tests that are positive
            - Reported cases: Official case counts from health facilities
            - Historical incidence: Past patterns of malaria transmission
            
            These variables provide the most direct measure of malaria burden but may be affected by reporting biases and healthcare access.
        """
    }
}

# Variable rationales - explanations for why specific variables are important
VARIABLE_RATIONALES = {
    "rainfall": """
        Rainfall is a critical environmental variable for malaria risk assessment because:
        
        1. It creates standing water bodies that serve as breeding sites for Anopheles mosquitoes
        2. Seasonal patterns of rainfall strongly influence the timing of malaria transmission
        3. Extreme rainfall can flush out breeding sites (reducing risk) or create new ones (increasing risk)
        
        Rainfall typically has a direct relationship with malaria risk (more rain = higher risk) up to a point, after which extremely heavy rainfall may reduce risk through breeding site destruction.
    """,
    
    "temperature": """
        Temperature is a fundamental variable for malaria risk because:
        
        1. It controls the development rate of the malaria parasite within the mosquito (sporogonic cycle)
        2. It affects mosquito development, survival, and biting rates
        3. Optimal transmission occurs between 25-30°C, with reduced transmission below 18°C or above 32°C
        
        Temperature has a non-linear relationship with risk, but in the MRPT tool, it's often modeled as having a direct relationship within the typical range found in endemic areas.
    """,
    
    "elevation": """
        Elevation (altitude) is a powerful predictor of malaria risk because:
        
        1. Higher elevations have lower temperatures, reducing parasite and mosquito development
        2. Highland areas typically have fewer suitable vector habitats
        3. There's often a clear elevation threshold above which transmission becomes rare
        
        Elevation has an inverse relationship with malaria risk (higher elevation = lower risk), and is often one of the strongest geographical predictors of transmission.
    """,
    
    "ndvi": """
        Normalized Difference Vegetation Index (NDVI) measures vegetation greenness and is important because:
        
        1. It indicates areas with sufficient moisture to support both vegetation and mosquito breeding
        2. Vegetation provides resting places for adult mosquitoes
        3. It correlates with humidity levels that affect mosquito survival
        
        NDVI typically has a direct relationship with malaria risk in many settings (higher vegetation = higher risk).
    """,
    
    "evi": """
        Enhanced Vegetation Index (EVI) is an improved vegetation index that:
        
        1. Is more sensitive than NDVI in areas with dense vegetation
        2. Better accounts for soil background and atmospheric interference
        3. Provides a more robust measure of canopy variations
        
        Like NDVI, EVI generally has a direct relationship with malaria risk but may provide better discrimination in heavily vegetated areas.
    """,
    
    "distance_to_water": """
        Distance to water bodies is a crucial spatial variable because:
        
        1. Permanent water bodies provide reliable mosquito breeding habitats
        2. Proximity to rivers, lakes, or marshes increases exposure to vectors
        3. The effect typically diminishes with distance from the water source
        
        Distance to water has an inverse relationship with malaria risk (greater distance = lower risk).
    """,
    
    "housing_quality": """
        Housing quality affects malaria risk through:
        
        1. Physical barriers to mosquito entry (screens, closed eaves, improved roofing)
        2. Correlation with socioeconomic factors that influence preventive behaviors
        3. Indoor environmental conditions that affect mosquito survival
        
        Housing quality has an inverse relationship with malaria risk (better housing = lower risk) and represents an important modifiable risk factor.
    """,
    
    "population": """
        Population density influences malaria transmission through:
        
        1. Human host availability for mosquitoes
        2. In high-density areas, "dilution effect" may reduce per-person biting rates
        3. Urban areas (high density) often have reduced vector habitats
        
        The relationship is context-dependent but often modeled as inverse in settings where higher density correlates with urbanization and better infrastructure.
    """,
    
    "soil_wetness": """
        Soil moisture or wetness is relevant because:
        
        1. It indicates areas prone to water pooling after rainfall
        2. Persistent soil moisture can support small breeding sites even without visible standing water
        3. It influences local humidity affecting mosquito survival
        
        Soil wetness typically has a direct relationship with malaria risk (wetter soil = higher risk).
    """,
    
    "urbanpercent": """
        Urban percentage or urbanicity is important because:
        
        1. Urban environments typically have fewer suitable vector habitats
        2. Urban infrastructure often includes better drainage and housing
        3. It helps determine appropriate intervention strategies
        
        Urban percentage usually has an inverse relationship with malaria risk (more urban = lower risk), though urban malaria remains significant in many settings.
    """,
    
    "pfpr": """
        Plasmodium falciparum Parasite Rate (PfPR) is a direct measure of malaria burden:
        
        1. It represents the percentage of the population carrying malaria parasites
        2. It's often considered the gold standard for measuring transmission intensity
        3. It captures the reservoir of infection in a community
        
        PfPR has a direct relationship with malaria risk (higher parasite rate = higher transmission risk) and is a key outcome indicator.
    """,
    
    "tpr": """
        Test Positivity Rate (TPR) measures the proportion of positive diagnostic tests:
        
        1. It reflects the prevalence of malaria among symptomatic individuals
        2. It's more readily available than PfPR as it's routinely collected at health facilities
        3. It can be influenced by testing practices and healthcare-seeking behavior
        
        TPR has a direct relationship with malaria risk but may be biased by who gets tested.
    """
}

def get_knowledge(topic, subtopic=None):
    """
    Retrieve knowledge base content for a given topic and optional subtopic.
    
    Args:
        topic (str): Main topic ('methodology', 'variables', or a specific variable name)
        subtopic (str, optional): Subtopic for methodology or variables
        
    Returns:
        str: The knowledge base content or None if not found
    """
    try:
        # Check if this is a variable rationale request
        if topic.lower() in [var.lower() for var in VARIABLE_RATIONALES]:
            # Find the key with case-insensitive match
            for var_key in VARIABLE_RATIONALES:
                if var_key.lower() == topic.lower():
                    return VARIABLE_RATIONALES[var_key].strip()
        
        # Check for methodology explanation
        if topic.lower() == 'methodology' and subtopic:
            subtopic_key = subtopic.lower().replace(' ', '_')
            if subtopic_key in METHODOLOGY_KB["data_cleaning"]:
                return METHODOLOGY_KB["data_cleaning"][subtopic_key].strip()
        
        # Check for general variable category information
        if topic.lower() == 'variables' and subtopic:
            subtopic_key = subtopic.lower()
            if subtopic_key in METHODOLOGY_KB["variables"]:
                return METHODOLOGY_KB["variables"][subtopic_key].strip()
        
        # If we get here, the requested topic/subtopic wasn't found
        logger.warning(f"Knowledge base lookup failed for topic='{topic}', subtopic='{subtopic}'")
        return None
        
    except Exception as e:
        logger.error(f"Error retrieving knowledge: {str(e)}")
        return None