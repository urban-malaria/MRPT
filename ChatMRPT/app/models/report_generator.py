# app/models/report_generator.py
import os
import logging
import datetime
import pandas as pd
import numpy as np # Ensure numpy is imported
from flask import current_app, session as flask_session # Use flask_session to avoid name collision
import openai # Ensure openai is imported
import json # Ensure json is imported

# Set up logging
logger = logging.getLogger(__name__)

# --- OpenAI Client and Call Helper ---
def get_openai_client():
    api_key = current_app.config.get('OPENAI_API_KEY')
    if not api_key:
        logger.error("OpenAI API Key not configured.")
        return None
    try:
        return openai.OpenAI(api_key=api_key)
    except Exception as e:
        logger.error(f"Failed to initialize OpenAI client: {e}")
        return None

def call_llm(prompt_messages, model="gpt-4o", max_tokens=2000): # Increased max_tokens for report sections
    client = get_openai_client()
    if not client:
        return "Error: OpenAI client not available. Please configure the API key."
    try:
        completion = client.chat.completions.create(
            model=model,
            messages=prompt_messages,
            temperature=0.4, # Slightly lower for more factual/consistent report content
            max_tokens=max_tokens
        )
        return completion.choices[0].message.content
    except Exception as e:
        logger.error(f"Error calling LLM: {e}")
        return f"<p><strong>Error generating content from AI assistant:</strong> {str(e)}</p>"

# --- Data Summarization Functions (Modified for AI Prompts) ---

def summarize_data_overview_for_ai(data_handler):
    if data_handler.csv_data is None:
        return "No CSV data was loaded for analysis."
    
    n_rows = len(data_handler.csv_data)
    n_columns = len(data_handler.csv_data.columns)
    numeric_vars_summary = []

    for col in data_handler.csv_data.columns:
        if col.lower() == 'wardname':
            continue
        if pd.api.types.is_numeric_dtype(data_handler.csv_data[col]):
            col_data = data_handler.csv_data[col].dropna()
            if not col_data.empty:
                summary = (f"'{col}': Min={col_data.min():.2f}, Max={col_data.max():.2f}, "
                           f"Mean={col_data.mean():.2f}, Missing={data_handler.csv_data[col].isna().sum()} values")
                numeric_vars_summary.append(summary)
            else:
                numeric_vars_summary.append(f"'{col}': All values missing or non-numeric after dropna, Missing={data_handler.csv_data[col].isna().sum()} values")
        else:
            numeric_vars_summary.append(f"'{col}': Non-numeric, Missing={data_handler.csv_data[col].isna().sum()} values")


    summary_text = (f"The analysis is based on a dataset covering {n_rows} wards. "
                    f"Initially, {n_columns} variables were provided. ")
    if numeric_vars_summary:
        summary_text += "Key variables and their basic statistics (before cleaning/normalization): "
        summary_text += "; ".join(numeric_vars_summary[:7]) # Show more for overview
        if len(numeric_vars_summary) > 7:
            summary_text += "... and others."
    else:
        summary_text += "No numeric variables found for summary or all values were missing."
    return summary_text


def summarize_missing_values_for_ai(data_handler):
    """
    Summarizes missing value handling for the AI prompt, using the
    methods actually applied by the DataHandler.
    """
    # Check if the list of methods used exists and is populated
    if not hasattr(data_handler, 'na_handling_methods') or not data_handler.na_handling_methods:
        # Check original missing columns list as a fallback message source
        if hasattr(data_handler, 'missing_columns') and data_handler.missing_columns:
            cols = data_handler.missing_columns
            return (f"Missing values were detected in {len(cols)} columns ({', '.join(cols)}) "
                    f"but specific handling method details were not recorded in 'na_handling_methods'.")
        else:
            return "No missing values were detected or handling method details were not recorded."

    # Create a lookup map from the list of dictionaries for easier access
    # Format: {'column_name': 'method_used', ...}
    methods_map = {item['column']: item['method'] for item in data_handler.na_handling_methods}

    summary_parts = []
    # Iterate through the original list of columns that HAD missing data initially
    # This ensures we report on all columns that needed cleaning.
    initial_missing_cols = data_handler.missing_columns if hasattr(data_handler, 'missing_columns') else list(methods_map.keys())
    
    if not initial_missing_cols:
         return "No columns required missing value handling."

    summary_parts.append("The following table summarizes the variables with missing entries and the imputation method used to address these gaps:")
    # Prepare data for a simple text-based table structure for the LLM
    table_data = []
    table_data.append("| Variable | Number of Missing Entries | Imputation Method Used |")
    table_data.append("|---|---|---|") # Markdown table separator

    for col in initial_missing_cols:
        missing_count = 'N/A'
        # Get original missing count if possible
        if data_handler.csv_data is not None and col in data_handler.csv_data.columns:
            missing_count = data_handler.csv_data[col].isna().sum()
        
        # Get the method actually used from our map
        method_used = methods_map.get(col, "Unknown/Not Imputed")

        # Make method names more user-friendly for the report
        if 'spatial' in method_used:
            # Be more specific if possible, e.g., based on fallbacks recorded
            if 'fallback_mean' in method_used:
                method_display = "Spatial Neighbor Mean (Fallback: Global Mean)"
            elif 'fallback_zero' in method_used:
                 method_display = "Spatial Neighbor Mean (Fallback: Zero)"
            else:
                 method_display = "Spatial Neighbor Mean"
        elif method_used == 'mean':
            method_display = "Mean Imputation"
        elif method_used == 'mode':
            method_display = "Mode Imputation"
        elif method_used == 'ffill/bfill':
            method_display = "Forward/Backward Fill"
        else:
            method_display = method_used.replace('_', ' ').capitalize()

        table_data.append(f"| {col} | {missing_count} | {method_display} |")

    # Combine table parts and add explanation
    summary_parts.extend(table_data)
    summary_parts.append("\nExplanation of Methods:")
    summary_parts.append("- **Spatial Neighbor Mean:** Missing values were imputed using the average value of adjacent wards (based on geography). If neighbors had no data or the ward had no neighbors, a fallback (like the overall mean or zero) might have been used.")
    summary_parts.append("- **Mean Imputation:** Missing numeric values were replaced with the average value of the entire column.")
    summary_parts.append("- **Mode Imputation:** Missing categorical values were replaced with the most frequent value in the entire column.")
    summary_parts.append("- **Forward/Backward Fill:** Used as a fallback for mode imputation if a clear mode wasn't found.")
    summary_parts.append("\nIt's important to consider that imputation introduces assumptions. The chosen methods aim to preserve data integrity while minimizing bias.")

    # Add instruction for the LLM to format this nicely
    final_instruction = ("\nPlease present the summary table clearly in HTML format (using <table>, <thead>, <tbody>, <tr>, <th>, <td> tags) "
                         "followed by the explanation of methods using appropriate HTML tags like <p> and <ul>/<li>.")

    return "\n".join(summary_parts) + final_instruction


def summarize_variable_relationships_for_ai(data_handler):
    if not hasattr(data_handler, 'variable_relationships') or not data_handler.variable_relationships:
        return "The relationships between variables and malaria risk were not explicitly determined or are currently unavailable."

    relationships_summary = []
    for var, rel in data_handler.variable_relationships.items():
        explanation = "increases, malaria risk also tends to increase" if rel == "direct" else "increases, malaria risk tends to decrease"
        relationships_summary.append(f"The variable '{var}' was determined to have a {rel} relationship with malaria risk (i.e., as '{var}' {explanation}).")
    
    if not relationships_summary:
        return "No variable relationship details are available."
    return "Analysis of variable relationships with malaria risk yielded: " + " ".join(relationships_summary)


def summarize_composite_scores_for_ai(data_handler):
    if not hasattr(data_handler, 'composite_scores') or not data_handler.composite_scores:
        return "Composite risk scores have not been calculated or are unavailable for this report."

    variables_used_in_composite = []
    if hasattr(data_handler, 'composite_variables') and data_handler.composite_variables:
        variables_used_in_composite = [v.replace('normalization_', '') for v in data_handler.composite_variables]
    elif isinstance(data_handler.composite_scores, dict) and data_handler.composite_scores.get('model_formulas'):
        # Try to get from the first model formula as a representative set
        if data_handler.composite_scores['model_formulas']:
            first_model_vars = data_handler.composite_scores['model_formulas'][0].get('variables', [])
            variables_used_in_composite = [v.replace('normalization_', '') for v in first_model_vars]
    
    num_models_generated = 0
    if isinstance(data_handler.composite_scores, dict) and data_handler.composite_scores.get('model_formulas'):
        num_models_generated = len(data_handler.composite_scores.get('model_formulas', []))

    summary = f"Composite malaria risk scores were computed by combining multiple normalized variables. "
    if variables_used_in_composite:
        summary += (f"The primary set of variables considered for these scores included: {', '.join(variables_used_in_composite)}. ")
    if num_models_generated > 0:
        summary += (f"A total of {num_models_generated} different models (combinations of these variables) were generated to assess risk. ")
    summary += "These scores provide a consolidated view of malaria risk across different wards."
    return summary


def summarize_vulnerability_rankings_for_ai(data_handler):
    if not hasattr(data_handler, 'vulnerability_rankings') or data_handler.vulnerability_rankings is None:
        return "Vulnerability rankings for the wards are not available at this time."

    try:
        top_5_wards = data_handler.vulnerability_rankings.head(5)['WardName'].tolist()
        num_ranked_wards = len(data_handler.vulnerability_rankings)
        return (f"Based on the median composite risk scores, all {num_ranked_wards} wards were ranked by vulnerability. "
                f"The top 5 most vulnerable wards identified were: {', '.join(top_5_wards)}. "
                "These rankings help prioritize areas for intervention.")
    except Exception as e:
        logger.error(f"Error summarizing vulnerability rankings: {e}")
        return "Could not summarize vulnerability rankings due to an error."


def summarize_urban_extent_for_ai(data_handler):
    if not hasattr(data_handler, 'urban_extent_results') or not data_handler.urban_extent_results:
        return "The urban extent analysis was not performed or its results are unavailable."

    summary_parts = []
    for threshold, results in data_handler.urban_extent_results.items():
        summary_parts.append(f"At a {threshold}% urbanicity threshold, {results.get('meets_threshold', 0)} wards were classified as predominantly urban (above threshold), while {results.get('below_threshold', 0)} were classified as less urban (below threshold). This classification can influence resource allocation strategies.")
    if not summary_parts:
        return "No specific details from the urban extent analysis are available."
    return "Urban extent analysis was conducted to categorize wards based on urbanicity: " + " ".join(summary_parts)


# --- Main AI Report Generation ---
def generate_ai_report_html_content(data_handler):
    html_parts = []
    session_id = flask_session.get('session_id', 'default') # Corrected to flask_session

    html_parts.append(f"""
    <!DOCTYPE html><html lang="en"><head><meta charset="UTF-8"><title>AI Generated Malaria Risk Report</title>
    <style>
        body {{ font-family: Arial, 'Helvetica Neue', Helvetica, sans-serif; margin: 20px; padding: 15px; background-color: #f4f7f6; color: #333; line-height: 1.6; }}
        h1, h2, h3 {{ color: #2c3e50; border-bottom: 1px solid #bdc3c7; padding-bottom: 5px; }}
        h1 {{ font-size: 2em; border-bottom-width: 2px; border-color: #3498db; }}
        h2 {{ font-size: 1.6em; margin-top: 1.5em; }}
        h3 {{ font-size: 1.3em; margin-top: 1.2em; color: #2980b9; }}
        .section {{ margin-bottom: 35px; padding: 20px; background-color: #ffffff; border: 1px solid #e0e0e0; border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.05); }}
        .viz-container {{ text-align: center; margin: 25px auto; padding: 15px; background-color: #fdfdfd; border: 1px solid #eee; border-radius: 5px; }}
        .viz-container img, .viz-container iframe {{ max-width: 100%; height: auto; border: 1px solid #ccc; border-radius: 4px; box-shadow: 0 1px 3px rgba(0,0,0,0.1); }}
        iframe {{ min-height: 550px; width: 100%; display: block; }}
        p {{ margin-bottom: 0.8em; }}
        ul, ol {{ padding-left: 25px; margin-bottom: 0.8em; }}
        li {{ margin-bottom: 0.4em; }}
        .report-header p {{ text-align: center; font-style: italic; color: #7f8c8d; margin-bottom: 2em; }}
        table {{ width:100%; border-collapse: collapse; margin: 1em 0; }}
        th, td {{ padding: 10px; text-align:left; border: 1px solid #ddd; }}
        th {{ background-color: #e9ecef; color: #495057; font-weight: bold; }}
        tr:nth-child(even) {{ background-color: #f8f9fa; }}
    </style></head><body>
    <div class="report-header">
        <h1>AI Generated Malaria Risk Analysis Report</h1>
        <p><em>Generated on: {datetime.datetime.now().strftime("%B %d, %Y at %H:%M")}</em></p>
    </div>
    """)

    report_sections_config = [
        {"title": "Data Overview & Initial Insights", "summary_func": summarize_data_overview_for_ai, "viz_suggestion": "If available, mention a key variable map that provides a good overview (e.g., population or a primary environmental factor)."},
        {"title": "Data Quality: Missing Value Assessment and Handling", "summary_func": summarize_missing_values_for_ai, "viz_suggestion": "This section is primarily textual unless specific 'before & after imputation' visualizations for a key variable are available and highly relevant."},
        {"title": "Variable Relationship Analysis and Data Normalization", "summary_func": summarize_variable_relationships_for_ai, "viz_suggestion": "Refer to the concept of normalized maps. If a good example exists (e.g., a map of a variable after normalization reflecting its risk contribution), mention it."},
        {"title": "Composite Malaria Risk Score Calculation", "summary_func": summarize_composite_scores_for_ai, "viz_suggestion": "Allude to the composite risk maps that visually represent these combined scores across different models/combinations."},
        {"title": "Ward Vulnerability Ranking and Prioritization", "summary_func": summarize_vulnerability_rankings_for_ai, "viz_suggestion": "Crucially mention the vulnerability ranking plot (box plot) and the overall vulnerability map as key outputs for prioritization."},
        {"title": "Urban Extent Analysis for Resource Allocation", "summary_func": summarize_urban_extent_for_ai, "viz_suggestion": "Describe that urban extent maps are generated for different thresholds to aid in deciding where interventions like bed net distribution are most appropriate."},
    ]

    # --- Visualization Manifest ---
    viz_manifest_parts = ["Manifest of available visualizations (use their ID and iframe src URL if embedding):\n"]
    all_viz_details_for_llm = [] # To store dicts for easier lookup if needed

    upload_folder_path_on_disk = os.path.join(current_app.config['UPLOAD_FOLDER'], session_id)
    if os.path.exists(upload_folder_path_on_disk):
        for i, fname in enumerate(os.listdir(upload_folder_path_on_disk)):
            if fname.endswith(('.html', '.png')): # Assuming PNGs might also be generated
                web_path = f"/serve_viz_file/{session_id}/{fname}"
                viz_type_guess = fname.split('_')[0] if '_' in fname else "general"
                viz_title_guess = fname.replace('.html','').replace('.png','').replace('_', ' ').title()
                
                viz_info = {"id": f"VIZ{i+1}", "filename": fname, "type": viz_type_guess, "title": viz_title_guess, "url": web_path}
                all_viz_details_for_llm.append(viz_info)
                viz_manifest_parts.append(f"- ID {viz_info['id']}: '{viz_info['title']}' (Type: {viz_info['type']}, Filename: {viz_info['filename']}, Embeddable URL: {viz_info['url']})")
    
    viz_manifest_text = "\n".join(viz_manifest_parts) if len(viz_manifest_parts) > 1 else "No specific visualizations were pre-generated for direct embedding reference in this report."


    # --- Generate content for each section ---
    for section_config in report_sections_config:
        title = section_config["title"]
        data_summary_for_section = section_config["summary_func"](data_handler)
        viz_guidance_for_section = section_config["viz_suggestion"]

        html_parts.append(f"<div class='section'><h2>{title}</h2>")

        system_prompt = (
            "You are an expert data analyst and epidemiologist authoring a section for a Malaria Risk Report. "
            "Your tone must be professional, authoritative, clear, and insightful. Write directly in HTML format for this section. "
            "Do NOT include overarching <html>, <head>, or <body> tags, as this will be part of a larger document. "
            "Use <p>, <ul>, <ol>, <li>, <strong>, <em> for text, and <table>, <thead>, <tbody>, <tr>, <th>, <td> for any tabular data summaries if appropriate. "
            "When discussing visualizations, refer to them by their ID and Title from the provided manifest. "
            "If you decide to embed a visualization iframe, use the 'Embeddable URL' directly. "
            "Example of embedding: '<div class=\"viz-container\"><p><strong>Visualization Title (ID VIZx)</strong></p><iframe src=\"/serve_viz_file/your_actual_session_id_here/filename.html\" title=\"Visualization Title\"></iframe></div>'. "
            f"The current session_id for constructing iframe src URLs is: '{session_id}'. Make sure this is used. "
            "Focus on interpreting the provided data summary and explaining its significance in the context of malaria risk. "
            "Choose ONLY ONE OR TWO most relevant visualizations from the manifest to embed or describe in detail for this specific section to avoid clutter."
        )
        user_prompt = (
            f"Develop the content for the report section titled: '{title}'.\n"
            f"Relevant data summary for this section: {data_summary_for_section}\n"
            f"Guidance on visualizations for this section: {viz_guidance_for_section}\n"
            f"Full manifest of available visualizations:\n{viz_manifest_text}\n"
            "Generate the HTML content for this section now."
        )
        
        messages = [{"role": "system", "content": system_prompt}, {"role": "user", "content": user_prompt}]
        
        logger.info(f"Prompting LLM for section: {title}")
        section_content_html = call_llm(messages)
        html_parts.append(section_content_html)
        html_parts.append("</div>")

    # --- Overall Conclusion (AI Generated) ---
    html_parts.append("<div class='section'><h2>Overall Conclusion & Strategic Recommendations</h2>")
    conclusion_system_prompt = "You are an expert epidemiologist providing a concise summary and actionable recommendations based on a malaria risk analysis. Output in HTML format. Do NOT include <html>, <head>, or <body> tags."
    conclusion_user_prompt = (
        "Based on the following key findings from a comprehensive malaria risk analysis, write an overall conclusion and formulate 2-3 strategic, actionable recommendations "
        "for health authorities regarding malaria control efforts, such as bed net distribution or targeted interventions. "
        f"Data Overview Summary: {summarize_data_overview_for_ai(data_handler)}. "
        f"Key Variables Influencing Risk (from composite scores): {summarize_composite_scores_for_ai(data_handler)}. "
        f"Identified Vulnerable Wards: {summarize_vulnerability_rankings_for_ai(data_handler)}. "
        f"Urban vs. Rural Considerations (from urban extent analysis): {summarize_urban_extent_for_ai(data_handler)}."
    )
    
    logger.info("Prompting LLM for Overall Conclusion.")
    overall_conclusion_html = call_llm([
        {"role": "system", "content": conclusion_system_prompt},
        {"role": "user", "content": conclusion_user_prompt}
    ])
    html_parts.append(overall_conclusion_html)
    html_parts.append("</div>")

    html_parts.append("</body></html>")
    return "".join(html_parts)


# --- Main function called by routes.py (generate_report) ---
# This function orchestrates the report generation.
def generate_report(data_handler, format='pdf'): # format is 'pdf', 'html', 'docx'
    try:
        report_id = f"mrpt_report_{datetime.datetime.now().strftime('%Y%m%d_%H%M%S')}"
        session_id = flask_session.get('session_id', 'default') # Corrected to flask_session
        
        # Ensure reports_dir uses app.config['REPORTS_FOLDER'] which points to instance path
        reports_dir = os.path.join(current_app.config['REPORTS_FOLDER'], session_id)
        os.makedirs(reports_dir, exist_ok=True)

        logger.info(f"Attempting to generate AI-driven HTML report content for session {session_id}.")
        html_content = generate_ai_report_html_content(data_handler)
        logger.info(f"AI-driven HTML report content generated for session {session_id}. Length: {len(html_content)} chars.")

        report_file_path = ""
        file_format_saved = format.lower()

        # For PDF/DOCX, we'll save the AI-generated HTML for now.
        # Actual conversion requires robust libraries (e.g., WeasyPrint for PDF, Pandoc/python-docx for DOCX)
        # and careful handling of styles and complex layouts from HTML.
        if file_format_saved == 'pdf':
            # Temp: Save as HTML with .pdf.html extension to indicate intent
            report_file_name = f"{report_id}.pdf.html"
            report_file_path = os.path.join(reports_dir, report_file_name)
            with open(report_file_path, 'w', encoding='utf-8') as f:
                f.write(html_content)
            logger.warning(f"PDF format requested, but saved as HTML ({report_file_name}) for now. Implement actual PDF conversion (e.g., WeasyPrint, xhtml2pdf).")
            # To use xhtml2pdf (ensure it's installed: pip install xhtml2pdf)
            # from xhtml2pdf import pisa
            # pdf_actual_path = os.path.join(reports_dir, f"{report_id}.pdf")
            # with open(pdf_actual_path, "wb") as pdf_file:
            #     pisa_status = pisa.CreatePDF(html_content, dest=pdf_file)
            # if not pisa_status.err:
            #    report_file_path = pdf_actual_path # Update path if PDF conversion succeeds
            #    file_format_saved = 'pdf'
            # else:
            #    logger.error(f"xhtml2pdf conversion failed: {pisa_status.err}")
            #    # Fallback to HTML path if PDF fails
        elif file_format_saved == 'docx':
            report_file_name = f"{report_id}.docx.html" # Temp
            report_file_path = os.path.join(reports_dir, report_file_name)
            with open(report_file_path, 'w', encoding='utf-8') as f:
                f.write(html_content)
            logger.warning(f"DOCX format requested, but saved as HTML ({report_file_name}) for now. Implement actual DOCX conversion (e.g., Pandoc, python-docx).")
        else: # Default to HTML
            report_file_name = f"{report_id}.html"
            report_file_path = os.path.join(reports_dir, report_file_name)
            with open(report_file_path, 'w', encoding='utf-8') as f:
                f.write(html_content)
            file_format_saved = 'html'
            logger.info(f"Successfully saved AI-generated HTML report: {report_file_path}")
            
        web_url = f"/download_report/{os.path.basename(report_file_path)}"

        return {
            'status': 'success',
            'message': f'AI-driven report content generated as {file_format_saved.upper()}. True PDF/DOCX conversion might require additional steps/libraries.',
            'report_url': web_url,
            'format': file_format_saved # This reflects what was actually saved
        }

    except Exception as e:
        logger.error(f"Error generating AI report: {str(e)}", exc_info=True)
        return {
            'status': 'error',
            'message': f'An unexpected error occurred while generating the AI report: {str(e)}'
        }

# If you decide to use xhtml2pdf for direct PDF generation:
# from xhtml2pdf import pisa
# def html_to_pdf_internal(html_content, output_path):
#     with open(output_path, "wb") as pdf_file:
#         pisa_status = pisa.CreatePDF(html_content, dest=pdf_file)
#     if pisa_status.err:
#         logger.error(f"PDF conversion error: {pisa_status.err}")
#         return False
#     return True