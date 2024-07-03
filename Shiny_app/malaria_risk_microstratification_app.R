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
  
  # actionButton("infoButton", "Open Modal"),
  
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
global health management"),
             
             fluidRow(column(6,
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
             )
             ,
             column( 6,
                     tags$br(),tags$br(),tags$br(),tags$br(),
                     tags$img(src = "digital_abstract.png", height = "600px", width = "auto"
                     ),
                     style='text-align: center',
                     tags$h6("The figure above shows a digital abstract first presented in Ozodiegwu et-al (in-press), 
            and it shows how the application was used to develop the malaria risk map in Ilorin")
                     
             )
             )), 
    
    tabPanel("Input variables (data and shapefiles)", 
             tags$br(),tags$br(),
             sidebarLayout(
               sidebarPanel(
                 tags$h3("Upload the shapefile and analysis data:"),
                 fileInput("file_csv", "Choose CSV File", accept = c(".csv", ".xlsx", ".xls")),
                 actionButton("edit_impacts", "Edit Variable Impacts"),
                 fileInput("file_shp", "Choose a Zipped Shapefile", 
                           accept = c('application/zip', 'application/x-zip-compressed', 
                                      'multipart/x-zip', 'application/x-compress', 
                                      'application/x-compressed', 'application/gzip')),
                 
                 tags$h4("Select a variable in the dataset to visualise:"),
                 uiOutput("variable_select_input"),
                 
                 
                 actionButton("plot_raw_data_button", "Plot Map"),
                 downloadButton("downloadData", "example data")
               ),
               mainPanel(
                 girafeOutput("rawdataPlots")
               )
             ),
             style='text-align: center',
             tags$h6("The ward.")), 
    
    tabPanel("Composite Score distribution", 
             # "Composite Score distribution", 
             tags$br(),tags$br(),
             sidebarLayout(
               sidebarPanel(
                 tags$h4("Select a variable in the dataset to visualise:"),
                 uiOutput("composite_variable_select"),
                 
                 actionButton("plot_button", "calculate")
                 
               ),
               
               mainPanel(
                 
                 girafeOutput("mapPlot"),
                 
                 tableOutput("dataTable") ,
                 style='text-align: center',
                 
                 tags$h6(".")
               )
             )
    ),
    
    # hr(),
    
    tabPanel(" box whisker plot", 
             tags$br(),tags$br(),
             
             plotOutput("boxwhiskerPlots"),
             # rank the plots based of  the mean/median score
             tags$br(),tags$br(), # Adds a single line break (adjust by adding more)
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
    '@ Loyola University, Parkinson School of Public Health, Department of Health Informatics and Data Science'
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
  raw_data_reactive <- reactiveVal()
  rawdata_reactive <- reactiveVal()
  normalized_data_reactive <- reactiveVal()
  normalizeddata_reactive <- reactiveVal()
  data_reactive <- reactiveVal()
  shp_data_reactive <- reactiveVal()
  csv_data_reactive <- reactiveVal()
  output_data_reactive <- reactiveVal()
  composite_scores_reactive <- reactiveVal()
  variable_impacts <- reactiveVal()
  
  
  # Function to show modal for specifying variable impacts
  showImpactModal <- function() {
    req(raw_data_reactive())
    variables <- setdiff(names(raw_data_reactive()), "WardName")
    current_impacts <- variable_impacts()
    
    # Create UI elements for each variable
    impact_ui <- lapply(variables, function(var) {
      selectInput(
        inputId = paste0("impact_", var),
        label = var,
        choices = c("Direct" = "direct", "Inverse" = "inverse"),
        selected = if (!is.null(current_impacts)) current_impacts[var] else "direct"
      )
    })
    
    # Show modal dialog
    showModal(modalDialog(
      title = "Specify Variable Relationship on Malaria Risk (direct or inverse)",
      impact_ui,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("ok", "OK")
      )
    ))
  }
  
  # Handle CSV/Excel file upload
  observeEvent(input$file_csv, {
    req(input$file_csv)
    
    # Read the uploaded file (csv or Excel)
    csv_data <- if (tolower(tools::file_ext(input$file_csv$name)) %in% c("xlsx", "xls")) {
      readxl::read_excel(input$file_csv$datapath)
    } else {
      read.csv(input$file_csv$datapath)
    }
    
    # Process numeric columns
    numeric_columns <- sapply(csv_data, is.numeric)
    csv_data[numeric_columns] <- lapply(csv_data[numeric_columns], function(x) {
      ifelse(is.na(x), mean(x, na.rm = TRUE), x)
    })
    
    # Convert to data frame and rename columns
    raw_dataframe <- rename_columns(as.data.frame(csv_data))
    raw_data_reactive(raw_dataframe)
    
    # Show modal dialog only if variable impacts haven't been set
    if (is.null(variable_impacts())) {
      showImpactModal()
    }
  })
  
  # Handle edit impacts button click
  observeEvent(input$edit_impacts, {
    showImpactModal()
  })
  
  # Handle OK button click in impact modal
  observeEvent(input$ok, {
    req(raw_data_reactive())
    variables <- setdiff(names(raw_data_reactive()), "WardName")
    impacts <- sapply(variables, function(var) input[[paste0("impact_", var)]])
    variable_impacts(impacts)
    removeModal()
    showNotification("Variable impacts updated successfully.", type = "message")
  })
  
  
  # Handle shapefile upload
  observeEvent(input$file_shp, {
    req(input$file_shp)
    
    # Unzip and read shapefile
    temp_dir <- tempdir()
    unzip(input$file_shp$datapath, exdir = temp_dir)
    shapefile_files <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    
    if (length(shapefile_files) > 0) {
      tryCatch({
        shp_data <- st_read(shapefile_files[1], quiet = TRUE)
        
        # Print shapefile info for debugging
        print(paste("Shapefile loaded. CRS:", st_crs(shp_data)$input))
        print(paste("Number of features:", nrow(shp_data)))
        print(paste("Column names:", paste(names(shp_data), collapse = ", ")))
        
        # Ensure the shapefile has a valid CRS
        if (is.na(st_crs(shp_data))) {
          showNotification("Warning: Shapefile has no CRS. Assuming WGS84.", type = "warning")
          shp_data <- st_set_crs(shp_data, 4326)
        }
        
        # Check for 'WardName' column and handle if missing
        if (!"WardName" %in% names(shp_data)) {
          potential_ward_cols <- grep("ward|name|district|HealthDistrict|NOM", names(shp_data), ignore.case = TRUE, value = TRUE)
          if (length(potential_ward_cols) > 0) {
            shp_data$WardName <- shp_data[[potential_ward_cols[1]]]
            showNotification(paste("Using", potential_ward_cols[1], "as WardName"), type = "warning")
          } else {
            showNotification("No 'WardName' column found in shapefile. Please ensure your shapefile has a column for ward names.", type = "error")
            return(NULL)
          }
        }
        
        shp_data_reactive(shp_data)
        showNotification("Shapefile loaded successfully.", type = "message")
      }, error = function(e) {
        showNotification(paste("Error loading shapefile:", e$message), type = "error")
        print(paste("Detailed error:", e$message))
      })
    } else {
      showNotification("No .shp file found in the uploaded zip file.", type = "error")
    }
  })
  
  # Update variable selection UI
  output$variable_select_input <- renderUI({
    req(raw_data_reactive())
    selectInput("visualize", "Select Variable", 
                choices = setdiff(names(raw_data_reactive()), "WardName"),
                selected = names(raw_data_reactive())[2])
  })
  
  
  # Update composite variable selection UI
  output$composite_variable_select <- renderUI({
    req(raw_data_reactive())
    selectInput("composite_vars", "Select Variables for Composite Score",
                choices = setdiff(names(raw_data_reactive()), "WardName"),
                selected = NULL, multiple = TRUE)
  })
  
  
  # Handle raw data plot button click
  observeEvent(input$plot_raw_data_button, {
    req(raw_data_reactive(), shp_data_reactive(), input$visualize)
    
    raw_data <- inner_join(raw_data_reactive(), shp_data_reactive())
    rawdata_reactive(raw_data)
    
    output$rawdataPlots <- renderGirafe({
      plot_map_00(variable_name = input$visualize,
                  shp_data_reactive = shp_data_reactive(),
                  raw_dataframe_reactive = rawdata_reactive())
    })
  })
  
  
  # Handle main plot button click
  observeEvent(input$plot_button, {
    req(raw_data_reactive(), input$composite_vars, variable_impacts())
    
    if (length(input$composite_vars) < 2) {
      showNotification("Please select at least two variables for composite score calculation.", type = "warning")
      return()
    }
    
    tryCatch({
      # Normalize data
      normalized_data <- normalize_data(raw_data_reactive(), variable_impacts())
      normalized_data_reactive(normalized_data)
      
      # Calculate composite scores using only selected variables
      composite_scores <- composite_score_models(normalized_data, selected_vars = input$composite_vars)
      
      # Process composite scores for plotting
      processed_scores <- process_model_score(composite_scores$final_data)
      
      # Join with shapefile data
      combined_data <- left_join(processed_scores, shp_data_reactive(), by = "WardName")
      data_reactive(combined_data)
      
      # Generate model formulas table
      model_formulae_table <- models_formulas(composite_scores$model_formula)
      output_data_reactive(model_formulae_table)
      
      # Update plots and tables
      output$mapPlot <- renderGirafe({
        plot_model_score_map(shp_data = shp_data_reactive(),
                             processed_csv = data_reactive())
      })
      
      output$dataTable <- renderTable({
        output_data_reactive()
      })
      
      # normalization plot section
      output$normalizationplot <- renderGirafe({
        req(normalized_data_reactive(), shp_data_reactive(), input$composite_vars)
        
        normalized_data <- normalized_data_reactive()
        shp_data <- shp_data_reactive()
        
        selected_norm_cols <- paste0("normalization_", tolower(input$composite_vars))
        
        combined_data <- left_join(normalized_data, shp_data, by = "WardName")
        
        if (!inherits(combined_data, "sf")) {
          combined_data <- st_as_sf(combined_data)
        }
        
        plot_data <- combined_data %>%
          select(WardName, geometry, all_of(selected_norm_cols)) %>%
          pivot_longer(cols = all_of(selected_norm_cols), 
                       names_to = "variable", values_to = "value") %>%
          mutate(class = cut(value, seq(0, 1, 0.2), include.lowest = TRUE))
        
        plot_normalized_map(shp_data = shp_data, processed_csv = plot_data)
      })
      
      output$boxwhiskerPlots <- renderPlot({
        box_plot_function(plottingdata = data_reactive())
      }, width = 800, height = 600)
      
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
  
  # Download example data
  output$downloadData <- downloadHandler(
    filename = function() {
      "example.csv"
    },
    content = function(file) {
      file.copy("example.csv", file)
    }
  )
}

shinyApp(ui = ui, server = server) 