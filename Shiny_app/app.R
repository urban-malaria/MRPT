source("functions.R")

ui <- fluidPage(
  theme = shinytheme("sandstone"),
  useShinyjs(),  
  titlePanel("Malaria Risk Mapping Tool"),
  
  tags$head(
    tags$style(HTML("
      .modal-content {
        box-shadow: 0 4px 8px 0 rgba(0,0,0,0.2);
        background-color: #f7f7f7;
        border-radius: 8px;
        font-family: 'Arial', sans-serif;
      }
      .modal-header, .modal-footer {
        background-color: #e9e9e9;
        border-bottom: 1px solid #ddd;
      }
      .modal-title {
        color: #333;
      }
      .modal-body {
        color: #555;
      }
      .fade {
        animation-name: fadeIn;
        -webkit-animation-name: fadeIn; 
        animation-duration: 0.5s; 
        -webkit-animation-duration: 0.5s;
        animation-timing-function: ease-in-out; 
        -webkit-animation-timing-function: ease-in-out;           
      }
      @keyframes fadeIn {
        0% {opacity: 0;}
        100% {opacity: 1;}
      }
      @-webkit-keyframes fadeIn {
        0% {opacity: 0;}
        100% {opacity: 1;}
      }
    "))
  ),
  
  br(),
  
  tabsetPanel(
    tabPanel("Instructions",
             tags$br(),
             h6("
Malaria remains a significant public health challenge globally, necessitating 
optimal and efficient strategies for risk management and intervention. 
Ozodiegwu et-al's study introduces a novel Malaria Risk Mapping Tool (MRMT), designed to
facilitate the microstratification of malaria risk across diverse geographical 
landscapes. The MRMT integrates environmental, socio-economic, 
and health-related data to generate high-resolution risk maps. Key variables, 
including the Enhanced Vegetation Index (EVI), settlement types, test positivity 
rates, and proximity to water bodies, are analyzed to identify areas of high 
transmission potential.


The tool's innovative approach lies in its ability to combine multiple data layers,
offering a comprehensive visualization of risk factors that contribute to malaria spread. 
This enables health authorities and stakeholders to prioritize resources and interventions 
more effectively, focusing on high-risk zones identified through the tool's analytical 
capabilities. Initial deployment and testing in a pilot region demonstrated the MRMT's efficacy 
in revealing previously unrecognized areas of vulnerability, facilitating targeted mosquito control
measures, health education campaigns, and infrastructure improvements (see Ozodiegwu et al).

Furthermore, the MRMT supports dynamic updating and scalability, allowing for the incorporation 
of new data and adaptation to different geographical regions. Its user-friendly interface ensures 
accessibility to a wide range of users, from public health officials to research institutions.

By providing a granular view of malaria risk, the MRMT represents a significant advancement 
in the field of public health, offering a potent tool for the strategic planning of malaria control 
and elimination efforts. Future developments will focus on integrating real-time data feeds and
expanding the tool's application to other vector-borne diseases, further enhancing its utility in 
global health management."),
             
             fluidRow(
               column(6,
                      tags$br(),tags$br(),
                      h3("Instructions to Using the Application"),
                      HTML("
        <ol>
          <li>Ensure you have a dataset with the variables of interest. 
          Your dataset should include column names, please download the example.csv file to edit. 
          Click on the varible name you will be redirected to the sites where you can download 
          the respective raster files. Note: Some countries may not be listed on the provided sites.
            <ul>
              <li>Ward name</li>
              <li>Distance to water bodies</li>
              <li>Population density</li>
              <li>Population size</li>
              <li>Test positivity rate</li>
              <li>Housing quality</li>
              <li>Enhanced Vegetation Index</li>
              <li>Surface temperature and humidity</li>
              <li>Rainfall</li>
              <li>Dump sites</li>
              <li>ITN distribution per capita</li>
              <li>Settlement type</li>
            </ul>
          </li>
          <li>Download the shapefile for your region of interest.</li>
          <li>Save the shapefile in the same folder as the variable dataset and compress it into a .zip format.</li>
          <li>Proceed to the next tab to upload the data file and shapefile and visualize the output.
            <ul>
              <li><b>Visualize Tab:</b> Visualize the distribution of each variable across wards. Hover over a polygon to see the ward name and variable measure.</li>
              <li><b>Normalized Tab:</b> Select variables for the composite score. This tab allows for normalization and visualization of each variable's impact on the composite score.</li>
              <li><b>Composite Score Tab:</b> Shows the effect of each variable within the composite score. Select the variables you want to see in the composite function.</li>
              <li><b>Composite Score Summary Tab:</b> Analyzes ward ranks to produce box/whisker plots, highlighting wards with the lowest risk.</li>
            </ul>
          </li>
        </ol>
      ")
               ),
               column(6,
                      tags$br(),tags$br(),tags$br(),tags$br(),
                      tags$img(src = "digital_abstract.png", height = "600px", width = "auto"),
                      style='text-align: center',
                      tags$h6("The figure above shows a digital abstract first presented in Ozodiegwu et-al (in-press), 
            and it shows how the application was used to develop the malaria risk map in Ilorin")
               )
             )
    ), 
    
    tabPanel("Input variables (data and shapefiles)", 
             fluidRow(
               column(4,
                      h3("Upload the shapefile and analysis data:"),
                      fileInput("file_csv", "Choose CSV File", 
                                accept = c(".csv", ".xlsx", ".xls")),
                      fileInput("file_shp", "Choose a Zipped Shapefile", 
                                accept = c('application/zip', 'application/x-zip-compressed', 
                                           'multipart/x-zip', 'application/x-compress', 
                                           'application/x-compressed', 'application/gzip')),
                      uiOutput("reopen_mismatch_modal"),
                      hr(),
                      h4("Select Variable to Visualize"),
                      uiOutput("variable_select"),
                      actionButton("plot_data", "Plot Maps"),
                      hr(),
                      actionButton("data_cleaning", "Data Cleaning (Click to clean data)")
               ),
               column(8,
                      fluidRow(
                        column(6,
                               
                               girafeOutput("rawDataPlot")
                        ),
                        column(6,
                               
                               girafeOutput("cleanedDataPlot")
                        )
                      ),
                      textOutput("visualizationExplanation")
               )
             ),
             style='text-align: center',
             tags$h6("The plot shows the distribution of the varibles selected for
                     evaluation accross the region of interest before they have been 
                     normalized using the min-max method. Each measure is presented on 
                     a continous scale (for the units one should refer to the rasters 
                     where the data was extarcted). The deeper the intensity of the 
                     colour in a given ward the higher the value of that measure is 
                     in the ward.")
    ), 
    
    tabPanel("Composite Score distribution", 
             tags$br(),tags$br(),
             sidebarLayout(
               sidebarPanel(
                 tags$h4("Select a variable in the dataset to visualise:"),
                 uiOutput("composite_variable_select"),
                 actionButton("plot_button", "calculate")
               ),
               mainPanel(
                 girafeOutput("mapPlot"),
                 tableOutput("dataTable"),
                 style='text-align: center',
                 tags$h6(".")
               )
             )
    ),
    
    tabPanel("box whisker plot", 
             tags$br(),tags$br(),
             plotlyOutput("boxwhiskerPlots", height = "800px"),
             tags$br(),tags$br(),
             tags$br(),tags$br(),
             tags$br(),tags$br(),
             tags$br(),tags$br(),
             tags$br(),tags$br(),
             style='text-align: center',
             tags$h6("The box and whisker plot visualizes the distribution of median 
                     vulnerability scores across wards, ranked from least to most 
                     vulnerable based on outcomes from models tailored with combinations 
                     of variables such as enhanced vegetation index, settlement type, 
                     test positivity rate, and distance to water bodies. Each model, 
                     integrating at least two or all mentioned variables, 
                     contributes to deriving the wards' median scores, showcasing 
                     the variance in vulnerability efficiently.") 
    ),
    
    tabPanel("normalization", 
             tags$br(),tags$br(),
             girafeOutput("normalizationplot"),
             style='text-align: center',
             tags$h6("The plot shows the distribution of the varibles selected for
                     evaluation accross the region of interest after they have been 
                     normalized using the min-max method. The values are now all on 
                     the same scale ranging from 0 to 1. This range of values are put 
                     into 5 classes (see legend). The plot highlights which of the
                     variables if used in the algorithm will have more infuence in the
                     composite score.")
    )
  ),
  
  tags$br(),  
  tags$br(),  
  tags$br(),  
  tags$br(),
  tags$br(),  
  tags$br(),
  
  hr(),
  
  div(
    style='text-align: center',
    'Created by the', 
    shiny::HTML('<a href=\'https://www.urban-malaria.com/\' 
                target=\'_blank\'> Urban Malaria Project Team </a>'),
    '@ Loyola University Chicago, Parkinson School of Public Health, Department of Health Informatics and Data Science'
  ),
  
  br()
)


server <- function(input, output, session) {
  
  # Show initial modal
  showModal(modalDialog(
    title = "Information",
    "Welcome to the de-prioritization web application, a powerful tool for 
    visualizing the distribution of variables associated with malaria risk 
    at a granular level. It uniquely focuses on a finer scale, analyzing data 
    down to the ward level rather than the broader Local Government Area (LGA). 
    This detailed approach, combined with geocoding technology, allows for 
    enhanced de-prioritization, enabling stakeholders to make decisions at the 
    smallest scale of settlement type. This specificity ensures that de-prioritization
    of resources like bed nets happens effectively, targeting areas with the highest
    need and optimizing malaria control efforts."
  ))
  
  # Use JavaScript to close the modal after 60 seconds
  shinyjs::runjs("setTimeout(function() { $('.modal').modal('hide'); }, 60000);")
  
  # Reactive values for storing various data states
  rv <- reactiveValues(
    raw_data = NULL,
    rawdata = NULL,
    mismatched_wards = NULL,
    cleaned_data = NULL,
    normalized_data = NULL,
    normalizeddata = NULL,
    data = NULL,
    shp_data = NULL,
    csv_data = NULL,
    output_data = NULL,
    composite_scores = NULL,
    variable_impacts = NULL,
    na_handling_choice = NULL,
    na_handling_method = NULL,
    variable_relationships = list(),
    csv_uploaded = FALSE,
    shp_uploaded = FALSE
  )
  
  
  # CSV file upload
  observeEvent(input$file_csv, {
    req(input$file_csv)
    
    csv_data <- if (tolower(tools::file_ext(input$file_csv$name)) %in% c("xlsx", "xls")) {
      readxl::read_excel(input$file_csv$datapath)
    } else {
      read.csv(input$file_csv$datapath)
    }
    
    rv$raw_data <- rename_columns(as.data.frame(csv_data))
    rv$na_columns <- check_missing_values(rv$raw_data)
    
    if (length(rv$na_columns) > 0) {
      showNotification(paste("Warning: Missing values (NAs) found in columns:", paste(rv$na_columns, collapse = ", ")), type = "warning")
    }
  })
  
  # Shapefile upload
  observeEvent(input$file_shp, {
    req(input$file_shp)
    
    temp_dir <- tempdir()
    unzip(input$file_shp$datapath, exdir = temp_dir)
    shapefile_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    
    if (length(shapefile_files) > 0) {
      tryCatch({
        shp_data <- st_read(shapefile_files[1], quiet = TRUE)
        rv$shp_data <- shp_data
        showNotification("Shapefile loaded successfully.", type = "message")
        
        if (!is.null(rv$raw_data)) {
          rv$mismatched_wards <- check_wardname_mismatches(rv$raw_data, rv$shp_data)
          if (!is.null(rv$mismatched_wards)) {
            showModal(wardNameMismatchModal())
          }
        }
      }, error = function(e) {
        showNotification(paste("Error loading shapefile:", e$message), type = "error")
      })
    } else {
      showNotification("No .shp file found in the uploaded zip file.", type = "error")
    }
  })
  
  # Add a button to reopen the ward name mismatch modal
  output$reopen_mismatch_modal <- renderUI({
    req(rv$mismatched_wards)
    if (!is.null(rv$mismatched_wards) && nrow(rv$mismatched_wards) > 0) {
      actionButton("reopen_mismatch", "Review Ward Name Mismatches")
    }
  })
  
  # Reopen ward name mismatch modal
  observeEvent(input$reopen_mismatch, {
    showModal(wardNameMismatchModal())
  })
  
  # Ward name mismatch modal
  wardNameMismatchModal <- function() {
    modalDialog(
      title = "Ward Name Mismatches",
      "The following ward names in your CSV file do not match the shapefile:",
      br(),
      tableOutput("mismatch_table"),
      br(),
      "Please correct these ward names in your CSV file to match the Shapefile WardNames and re-upload the data.",
      footer = tagList(
        modalButton("Close")
      ),
      size = "l"  # Make the modal larger
    )
  }
  
  output$mismatch_table <- renderTable({
    req(rv$mismatched_wards)
    rv$mismatched_wards
  }, align = 'l')
  
  
  # Data Cleaning
  observeEvent(input$data_cleaning, {
    req(rv$raw_data, rv$shp_data)
    
    showModal(modalDialog(
      title = "Data Cleaning",
      
      h4("Columns with Missing Values (NAs)"),
      tableOutput("na_columns_table"),
      
      radioButtons("na_handling", "Choose NA handling method:",
                   choices = c("Mean of neighbors" = "spatial_neighbor_mean",
                               "Mean of entire region" = "region_mean",
                               "Mode of entire region" = "region_mode"),
                   selected = rv$na_handling_method),
      
      h4("Specify Variable Relationships with Malaria Risk"),
      uiOutput("variable_relationships"),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("apply_cleaning", "Apply and Continue")
      )
    ))
  })
  
  output$variable_relationships <- renderUI({
    req(rv$raw_data)
    vars <- setdiff(names(rv$raw_data), "WardName")
    
    lapply(vars, function(var) {
      fluidRow(
        column(6, var),
        column(6, 
               radioButtons(paste0("relationship_", var), NULL,
                            choices = c("Direct" = "direct", "Inverse" = "inverse"),
                            selected = rv$variable_relationships[[var]] %||% "direct",  # Use stored value or default to "direct"
                            inline = TRUE)
        )
      )
    })
  })
  
  # Apply cleaning
  observeEvent(input$apply_cleaning, {
    req(rv$raw_data, rv$shp_data)
    
    # Store NA handling method
    rv$na_handling_method <- input$na_handling
    
    # Apply NA handling method
    cleaned_data <- switch(rv$na_handling_method,
                           "spatial_neighbor_mean" = handle_na_neighbor_mean(rv$raw_data, rv$shp_data),
                           "region_mean" = handle_na_region_mean(rv$raw_data),
                           "region_mode" = handle_na_region_mode(rv$raw_data))
    
    # Store and apply variable relationships
    vars <- setdiff(names(cleaned_data), "WardName")
    for (var in vars) {
      relationship <- input[[paste0("relationship_", var)]]
      rv$variable_relationships[[var]] <- relationship
      if (relationship == "inverse") {
        cleaned_data[[var]] <- max(cleaned_data[[var]], na.rm = TRUE) - cleaned_data[[var]]
      }
    }
    
    rv$cleaned_data <- cleaned_data
    removeModal()
    showNotification("Data cleaning applied successfully.", type = "message")
  })
  
  # Raw data variable selection
  output$variable_select <- renderUI({
    req(rv$raw_data)
    selectInput("visualize_var", "Select Variable", 
                choices = setdiff(names(rv$raw_data), "WardName"),
                selected = names(rv$raw_data)[2])
  })
  
  observeEvent(input$plot_data, {
    req(rv$raw_data, rv$shp_data, input$visualize_var)
    
    output$rawDataPlot <- renderGirafe({
      plot_map_00(variable_name = input$visualize_var,
                  shp_data_reactive = rv$shp_data,
                  dataframe_reactive = rv$raw_data,
                  title = "Raw Data")
    })
    
    output$cleanedDataPlot <- renderGirafe({
      if (!is.null(rv$cleaned_data)) {
        plot_map_00(variable_name = input$visualize_var,
                    shp_data_reactive = rv$shp_data,
                    dataframe_reactive = rv$cleaned_data,
                    title = "Cleaned Data")
      } else {
        plot_map_00(variable_name = input$visualize_var,
                    shp_data_reactive = rv$shp_data,
                    dataframe_reactive = rv$raw_data,
                    title = "Data Not Cleaned Yet")
      }
    })
    
    output$visualizationExplanation <- renderText({
      if (is.null(rv$cleaned_data)) {
        "The left plot shows the raw data. The right plot will show cleaned data after you perform data cleaning."
      } else {
        paste("The left plot shows the raw data. The right plot shows the cleaned data, where missing values (NAs) have been handled using the", 
              rv$na_handling_method, "method. Variable relationships have been applied as specified in the data cleaning step.")
      }
    })
  })
  
  
  
  # Composite Score
  output$composite_variable_select <- renderUI({
    req(rv$cleaned_data)
    selectInput("composite_vars", "Select Variables for Composite Score",
                choices = setdiff(names(rv$cleaned_data), "WardName"),
                selected = NULL, multiple = TRUE)
  })
  
  observeEvent(input$plot_button, {
    req(rv$cleaned_data, input$composite_vars)
    
    if (length(input$composite_vars) < 2) {
      showNotification("Please select at least two variables for composite score calculation.", type = "warning")
      return()
    }
    
    tryCatch({
      print("Cleaned data structure:")
      print(str(rv$cleaned_data))
      
      # Get variable impacts
      variable_impacts <- sapply(input$composite_vars, function(var) input[[paste0("impact_", var)]])
      print("Variable impacts:")
      print(variable_impacts)
      
      # Normalize data
      normalized_data <- normalize_data(rv$cleaned_data[, c("WardName", input$composite_vars)], variable_impacts)
      print("Normalized data summary:")
      print(summary(normalized_data))
      
      if (is.null(normalized_data)) {
        showNotification("Error in data normalization. Check the console for details.", type = "error")
        return()
      }
      
      rv$normalized_data <- normalized_data
      
      print("Normalized data structure:")
      print(str(normalized_data))
      
      # Calculate composite scores using only selected variables
      composite_scores <- composite_score_models(normalized_data, selected_vars = input$composite_vars)
      
      if (is.null(composite_scores)) {
        showNotification("Error in composite score calculation. Check the console for details.", type = "error")
        return()
      }
      
      # Process composite scores for plotting
      processed_scores <- process_model_score(composite_scores$final_data)
      
      # Join with shapefile data
      combined_data <- left_join(processed_scores, rv$shp_data, by = "WardName")
      rv$data <- combined_data
      
      # Generate model formulas table
      model_formulae_table <- models_formulas(composite_scores$model_formula)
      rv$output_data <- model_formulae_table
      
      # Update plots and tables
      output$mapPlot <- renderGirafe({
        plot_model_score_map(shp_data = rv$shp_data,
                             processed_csv = rv$data)
      })
      
      output$dataTable <- renderTable({
        rv$output_data
      })
      
      # Normalization plot
      output$normalizationplot <- renderGirafe({
        req(rv$normalized_data, rv$shp_data, input$composite_vars)
        plot_normalized_map(shp_data = rv$shp_data, 
                            processed_csv = rv$normalized_data, 
                            selected_vars = input$composite_vars)
      })
      
      # Box whisker plot
      output$boxwhiskerPlots <- renderPlotly({
        box_plot_function(plottingdata = rv$data)
      })
      
      # Show the number of model combinations generated
      num_models <- nrow(model_formulae_table)
      if (length(input$composite_vars) == 2) {
        showNotification("Generated 1 model combination using the two selected variables.", type = "message")
      } else {
        showNotification(paste("Generated", num_models, "model combinations."), type = "message")
      }
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
}

shinyApp(ui = ui, server = server)