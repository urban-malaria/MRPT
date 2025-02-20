source("functions.R")
#shiny::runApp("Shiny_app")
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  useShinyjs(),  
  titlePanel("Malaria Reprioritization Tool"),
  
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
      
      .shiny-progress-container {
      top: 0px;
    }
    .progress-bar {
      background-color: #337ab7;
    }
    "))
  ),
  
  br(),
  
  tabsetPanel(
    tabPanel("Instructions",
             tags$br(),
             fluidRow(
               column(6,
                      h3("Instructions for Using the Malaria Reprioritization Tool"),
                      p("Follow these steps to use the tool effectively:"),
                      h4("1. Preparing Your Data"),
                      tags$ul(
                        tags$li("Ensure you have a CSV file with variable data and the corresponding shapefile for your region of interest."),
                        tags$li("The CSV file should include columns for WardName and various risk factors (e.g., settlement type, enhanced vegetation index, rainfall, etc.)."),
                        tags$li("Compress the shapefile components into a zip file.")
                      ),
                      h4("2. Input Variables (Data and Shapefiles) Tab"),
                      tags$ul( 
                        tags$li("Upload your CSV file and zipped shapefile using the respective upload buttons."),
                        tags$li("Select a variable to visualize from the dropdown menu and click 'Plot Maps' to see its distribution."),
                        tags$li("Use the 'Click to fill in missing values' button if your data has missing values.")
                      ),
                      h4("3. Normalization Tab"),
                      tags$ul( 
                        tags$li("Define how each variable relates to malaria risk (direct or inverse)."),
                        tags$li("Visualize normalized variables to understand their distribution.")
                      ),
                      h4("4. Composite Score Distribution Tab"),
                      tags$ul(
                        tags$li("Select variables to include in your composite risk score."),
                        tags$li("Generate and examine composite score maps for different variable combinations.")
                      ),
                      h4("5. Box and Whisker Plot Tab"),
                      tags$ul( 
                        tags$li("Analyze the distribution of vulnerability scores across wards."),
                        tags$li("Toggle between box plot and map views to understand risk distribution.")
                      ),
                      h4("6. Decision Tree Tab"),
                      tags$ul(
                        tags$li("Explore a visual representation of the decision-making process in creating the malaria risk maps."),
                        tags$li("Understand how variable selections impact the final risk assessment.")
                      ),
                      h4("7. Manual Labelling Tab"),
                      tags$ul(
                        tags$li("Visualize detailed maps of individual wards based on rankings from previous analyses."),
                        tags$li("Examine settlement types and other factors within specific wards.")
                      ),
                      h4("Data Privacy"),
                      p("Please ensure that you have the necessary permissions to use and upload your data. Do not upload sensitive or personal data without proper authorization."),
                      p(
                        "For issues or technical support with the application, email ",
                        tags$a(href="mailto:lmhlanga@luc.edu", "lmhlanga@luc.edu"),
                        " or ",
                        tags$a(href="mailto:bboateng1@luc.edu", "bboateng1@luc.edu")
                      ),
                      h4("Funding"),
                      p("This work and the authors are supported by the Bill and Melinda Gates Foundation (Investment ID: INV-036449).")
               ),
               column(6, 
                      tags$br(), tags$br(), tags$br(), tags$br(), 
                      tags$img(src = "digital_abstract.png", height = "600px", width = "auto"),
                      style = 'text-align: center',
                      tags$h6("The figure above shows a digital abstract first presented in Ozodiegwu et-al (in-press), 
                              and it shows how the application was used to develop the malaria risk map in Ilorin")
               )
             ),
    ),  
    
    tabPanel("Input variables (data and shapefiles)", 
             fluidRow(
               column(3,
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
                      actionButton("data_cleaning", "Click to fill in missing values")
               ),
               column(9,
                      fluidRow(
                        uiOutput("plotLayout")
                      ),
                      htmlOutput("visualizationExplanation"),
                      tags$p(style = "font-style: italic; margin-top: 10px;", 
                             "Hover over the maps to see detailed information for each ward.")
               )
             )
    ), 
    
    tabPanel("Normalization", 
             sidebarLayout(
               sidebarPanel(
                 actionButton("specify_relationships", "SPECIFY VARIABLE RELATIONSHIPS"),
                 tags$br(), 
                 tags$br(),
                 uiOutput("normalized_variable_select"),
                 tags$br(),
                 actionButton("plot_normalized", "PLOT NORMALIZED MAP")
               ),
               mainPanel(
                 conditionalPanel(
                   condition = "input.plot_normalized > 0", 
                   girafeOutput("normalizationplot")
                 )
               )
             ),
             conditionalPanel(
               condition = "input.plot_normalized > 0",
               tags$br(),
               tags$p(style = "font-style: italic; margin-top: 10px;",
                      "Hover over the map to see detailed information for each ward."),
               tags$div(
                 style = "margin-top: 20px; padding: 10px; background-color: #f0f0f0; border-radius: 5px;",
                 tags$h4("Understanding the Normalized Plot", style = "text-align: center;"),
                 tags$p("The plot shows the distribution of the variables selected for
                            evaluation across the region of interest after they have been
                            normalized using the min-max method. The values are now all on
                            the same scale ranging from 0 to 1. This range of values are put
                            into 5 classes (see legend). The plot highlights which of the
                            variables if used in the algorithm will have more influence in the
                            composite score.", style = "text-align: justify;")
               )
             )
    ),
    
    tabPanel("Composite Score distribution", 
             tags$br(),tags$br(),
             sidebarLayout(
               sidebarPanel(
                 tags$h4("Select a variable in the dataset to visualise:"),
                 tags$p("Select at least two variables:", style = "color: black;"),
                 uiOutput("composite_variable_select"),
                 div(id = "progress-bar-container", style = "margin-top: 10px;"),
                 actionButton("plot_button", "Calculate")
               ),
               mainPanel(
                 uiOutput("mapPlot"),
                 conditionalPanel(
                   condition = "input.plot_button > 0",
                   tags$br(),
                   tags$div(
                     style = "text-align: justify; font-size: 12px; color: #666;",
                     "The maps above show the distribution of malaria risk scores across different wards, calculated using various combinations of variables. Each map represents a different model, with the variables used listed in the title. The color scale ranges from yellow (very low risk) to dark red (very high risk). This visualization helps identify areas of high concern and compare how different combinations of factors affect the risk assessment."
                   ),
                   tags$br(),
                   tableOutput("flagged_models_table"),
                   tags$p(style = "font-style: italic; margin-top: 10px; font-size: 12px;",
                          "Hover over the maps to see detailed information for each ward.")
                 )
               )
             )
    ),
    
    tabPanel("Box and Whisker Plot", 
             fluidRow(
               column(12,
                      h3("Ward Vulnerability Distribution"),
                      p("This visualization shows the distribution of vulnerability scores across wards, helping to identify areas of high and low risk."),
                      br(),
                      checkboxInput("show_map", "Show Map View", value = FALSE),
                      br()
               )
             ),
             fluidRow(
               column(8,
                      conditionalPanel(
                        condition = "input.show_map == false",
                        plotlyOutput("boxwhiskerPlots", height = "500px")
                      ),
                      conditionalPanel(
                        condition = "input.show_map == true",
                        girafeOutput("vulnerabilityMap", height = "500px")
                      )
               ),
               column(4,
                      conditionalPanel(
                        condition = "input.show_map == false",
                        wellPanel(
                          style = "background-color: #f5f5f5; border: 1px solid #e3e3e3; border-radius: 4px; padding: 15px; height: 600px; overflow-y: auto;",
                          h4("Understanding the Box and Whisker Plot"),
                          p("This plot visualizes the distribution of median vulnerability scores across wards, ranked from most vulnerable (top) to least vulnerable (bottom)."),
                          p("Each horizontal bar represents a ward:"),
                          tags$ul(
                            tags$li("The box shows the interquartile range (IQR) of vulnerability scores."),
                            tags$li("The vertical line inside the box represents the median score."),
                            tags$li("The whiskers extend to show the full range of scores, excluding outliers."),
                            tags$li("Any points beyond the whiskers are considered outliers.")
                          ),
                          p("This visualization helps identify which areas may need more attention or resources in malaria prevention efforts.")
                        )
                      ),
                      conditionalPanel(
                        condition = "input.show_map == true",
                        wellPanel(
                          style = "background-color: #f5f5f5; border: 1px solid #e3e3e3; border-radius: 4px; padding: 15px; height: 500px; overflow-y: auto;",
                          h4("Understanding the Vulnerability Map"),
                          p("This map displays the overall vulnerability rank for each ward, providing a geographic perspective on malaria risk."),
                          p("Key features:"),
                          tags$ul(
                            tags$li("Color intensity represents the vulnerability rank, with darker colors indicating higher vulnerability (higher rank) and lighter colors indicating lower vulnerability (lower rank)."),
                            tags$li("The color scale ranges from light yellow (least vulnerable, lowest rank, 1) to dark purple (most vulnerable, highest rank)."),
                            tags$li("Hover over each ward to see its name and exact rank."),
                            tags$li("The map allows for easy identification of high-risk areas and spatial patterns in vulnerability.")
                          ),
                          p("Interpretation:"),
                          tags$ul(
                            tags$li("Dark purple areas represent wards with the highest ranks indicating the highest vulnerability and risk of malaria."),
                            tags$li("Light yellow areas represent wards with the lowest ranks (1, 2, 3, etc.), indicating the lowest vulnerability and risk of malaria."),
                            tags$li("The gradients between these colors represent varying levels of vulnerability.")
                          ),
                          p("Use this map to identify priority areas for intervention. Wards with higher ranks (darker colors) may require more immediate attention and resources in malaria prevention efforts.")
                        )
                      )
               )
             )
    ),
    
    # Add these to your UI
    tabPanel("Decision Tree",
             fluidRow(
               column(12,
                      div(
                        style = "padding: 20px; background-color: #FFFFFF; border-radius: 8px; margin-bottom: 20px;",
                        
                        # Download buttons
                        div(
                          style = "margin-bottom: 20px;",
                          tags$head(
                            tags$script("
                              $(document).on('shiny:error', function(event) {
                                if(event.name === 'downloadPNG' || event.name === 'downloadPDF') {
                                  alert('Error during download. Please try again.');
                                }
                              });
                            ")
                          ),
                          downloadButton("downloadPNG", "Download PNG", 
                                         style = "margin-right: 10px; background-color: #5D4E6D; color: white;"),
                          downloadButton("downloadPDF", "Download PDF",
                                         style = "background-color: #5D4E6D; color: white;")
                        ),
                        
                        # Main visualization container
                        div(
                          style = "background-color: white; padding: 20px; border-radius: 8px;",
                          grVizOutput("decisionTree", width = "100%", height = "600px")
                        ),
                        
                        # Help text
                        div(
                          style = "margin-top: 20px; padding: 15px; background-color: #F8F9FA; border-radius: 8px;",
                          p("Click the download buttons above to save the decision tree in your preferred format.",
                            style = "color: #666; font-style: italic;"),
                          p("Note: The download may take a few seconds to complete.",
                            style = "color: #666; font-style: italic;")
                        )
                      )
               )
             )
    ),
    
    tabPanel("Manual Labelling",
             sidebarLayout(
               sidebarPanel(
                 uiOutput("ward_select"),
                 actionButton("plot_ward_map", "Plot Map")
               ),
               mainPanel(
                 leafletOutput("ward_map", height = 600)
               )
             )
    ),
  ),
  
  
  tags$br(),  
  tags$br(),
  tags$br(),
  tags$br(),
  tags$br(),
  tags$br(),
  
  hr(),
  
  div(
    style='text-align: center; margin-top: 20px;',  # Add margin-top for slight spacing
    'Created by the', 
    shiny::HTML('<a href=\'https://www.urban-malaria.com/\' 
                target=\'_blank\'> Urban Malaria Lab </a>'),
    '@ Loyola University Chicago, Parkinson School of Public Health, Department of Health Informatics and Data Science'
  ),
  
  br(),
  
  div(
    style='text-align: center; font-style: italic; font-size: 1.1em; margin-bottom: 20px;',  # Add margin-bottom for slight spacing
    tags$strong('If you use this tool in your work, please cite:'),
    br(),
    'Mhlanga, L.,* Boateng, B.O.,* Jamiu, Y., Bamgboye, E.A., Adeniji, H., Ademu, C., Okoronkwo, C., Enang, G. & Ozodiegwu, I.D. (2024).',
    br(),
    'Malaria Reprioritization Tool. Urban Malaria Lab, Loyola University Chicago.',
    br(),
    'Available at: ',
    tags$a(href="https://urbanmalaria.shinyapps.io/mrpt/", 
           target="_blank", 
           "https://urbanmalaria.shinyapps.io/mrpt/")
  ),
  br()
)


server <- function(input, output, session) {
  
  # Show initial modal
  showModal(modalDialog(
    title = "Information",
    HTML("Welcome to the reprioritization web application, a powerful tool for 
    visualizing the distribution of variables associated with malaria risk 
    at a granular level. It uniquely focuses on a finer scale, analyzing data 
    down to the ward level rather than the broader Local Government Area (LGA).
    <br><br>
    This detailed approach, combined with geocoding technology, allows for 
    enhanced reprioritization, enabling stakeholders to make decisions at the 
    smallest scale of settlement type. This specificity ensures that 
    \"reprioritization of resources like bed nets happens effectively, targeting 
    areas with the highest need and optimizing malaria control efforts.\"")
  ))
  
  # Use JavaScript to close the modal after 60 seconds
  shinyjs::runjs("setTimeout(function() { $('.modal').modal('hide'); }, 300000);")
  
  # Reactive values for storing various data states
  rv <- reactiveValues(
    raw_data = NULL,
    cleaned_data = NULL,
    normalized_data = reactiveVal(NULL),
    shp_data = NULL,
    mismatched_wards = NULL,
    na_handling_methods = list(),
    variable_relationships = list(),
    composite_scores = NULL,
    data = NULL,
    output_data = NULL
  )
  
  rv$decision_tree_progress <- reactiveVal(list(
    data_loaded = FALSE,
    variables_selected = FALSE,
    normalization_done = FALSE,
    composite_scores_calculated = FALSE
  ))
  
  # new reactive value to track whether cleaning was performed
  rv$cleaning_performed <- reactiveVal(FALSE)
  
  rv$flagged_models <- reactiveVal(NULL)
  
  # CSV file upload
  observeEvent(input$file_csv, {
    req(input$file_csv)
    
    csv_data <- if (tolower(tools::file_ext(input$file_csv$name)) %in% c("xlsx", "xls")) {
      readxl::read_excel(input$file_csv$datapath)
    } else {
      read.csv(input$file_csv$datapath)
    }
    
    rv$raw_data <- rename_columns(as.data.frame(csv_data))
    missing_data <- check_missing_values(rv$raw_data)
    rv$na_columns <- missing_data$columns
    rv$raw_data <- missing_data$data
    
    # Set cleaned_data to raw_data initially
    rv$cleaned_data <- rv$raw_data
    
    # Get columns after WardName
    columns_after_wardname <- get_columns_after_wardname(rv$cleaned_data)
    
    # Populate variable relationships for columns after WardName
    rv$variable_relationships <- setNames(rep("direct", length(columns_after_wardname)), columns_after_wardname)
    
    if (rv$needs_cleaning()) {
      if (length(rv$na_columns) > 0) {
        showNotification(paste("Warning: Missing values (NAs) found in columns:", paste(rv$na_columns, collapse = ", ")), type = "warning")
      }
      if (!is.null(rv$mismatched_wards) && nrow(rv$mismatched_wards) > 0) {
        showModal(wardNameMismatchModal(rv$mismatched_wards))
      }
    } else {
      showNotification("Data is already clean. No cleaning necessary.", type = "message")
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
        rv$shp_data <- shp_data %>% 
          mutate(Urban = as.character(Urban))  # Ensure Urban is character type
        print(paste("Number of rows in shp_data:", nrow(rv$shp_data)))
        showNotification("Shapefile loaded successfully.", type = "message")
        
        if (!is.null(rv$raw_data)) {
          rv$mismatched_wards <- check_wardname_mismatches(rv$raw_data, rv$shp_data)
          if (!is.null(rv$mismatched_wards) && nrow(rv$mismatched_wards) > 0) {
            showModal(wardNameMismatchModal(rv$mismatched_wards))
          }
        }
      }, error = function(e) {
        showNotification(paste("Error loading shapefile:", e$message), type = "error")
      })
    } else {
      showNotification("No .shp file found in the uploaded zip file.", type = "error")
    }
  })
  
  
  # add this reactive value
  rv$corrected_wardnames <- reactiveVal(data.frame(original = character(), corrected = character(), stringsAsFactors = FALSE))
  
  # Modify the observeEvent for apply_corrections
  observeEvent(input$apply_corrections, {
    req(rv$mismatched_wards, rv$raw_data)
    
    corrected_names <- data.frame(
      original = rv$raw_data$WardName,
      corrected = rv$raw_data$WardName,
      stringsAsFactors = FALSE
    )
    
    for (i in 1:nrow(rv$mismatched_wards)) {
      old_name <- rv$mismatched_wards$CSV_WardName[i]
      new_name <- input[[paste0("mismatch_", i)]]
      
      if (new_name != "None") {
        corrected_names$corrected[corrected_names$original == old_name] <- new_name
      }
    }
    
    rv$corrected_wardnames(corrected_names)
    rv$raw_data$WardName <- corrected_names$corrected
    
    # Check for remaining mismatches
    remaining_mismatches <- check_wardname_mismatches(rv$raw_data, rv$shp_data)
    
    if (is.null(remaining_mismatches)) {
      removeModal()
      showNotification("All ward name mismatches have been corrected.", type = "message")
    } else {
      rv$mismatched_wards <- remaining_mismatches
      showModal(wardNameMismatchModal(remaining_mismatches))
    }
  })
  
  # Modify the wardNameMismatchModal function
  wardNameMismatchModal <- function(mismatches) {
    req(mismatches)
    corrected_names <- rv$corrected_wardnames()
    
    # Sort mismatches alphabetically by CSV_WardName
    mismatches <- mismatches[order(mismatches$CSV_WardName), ]
    
    modalDialog(
      title = "Data Cleaning",
      
      tags$div(
        tags$h4("Warning: Wardname Mismatches Detected"),
        tags$p("The following ward names in the scoring data do not match with the shapefile:"),
        tags$ul(
          lapply(mismatches$CSV_WardName, function(ward) {
            tags$li(ward)
          })
        ),
        tags$p("Please correct these mismatches before proceeding.")
      ),
      
      lapply(1:nrow(mismatches), function(i) {
        ward <- mismatches$CSV_WardName[i]
        options <- c("None", sort(mismatches$Shapefile_Options[[i]]))  # Sort options alphabetically
        
        selected_value <- if (!is.null(corrected_names) && nrow(corrected_names) > 0) {
          corrected <- corrected_names$corrected[corrected_names$original == ward]
          if (length(corrected) > 0) corrected else "None"
        } else {
          "None"
        }
        
        tags$div(
          tags$h4(ward),
          selectInput(paste0("mismatch_", i), label = NULL, choices = options, selected = selected_value)
        )
      }),
      
      footer = tagList(
        actionButton("apply_corrections", "Apply Corrections"),
        modalButton("Cancel")
      ),
      size = "l"
    )
  }
  
  
  output$reopen_mismatch_modal <- renderUI({
    req(rv$mismatched_wards)
    if (!is.null(rv$mismatched_wards) && nrow(rv$mismatched_wards) > 0) {
      actionButton("reopen_mismatch", "Review Ward Name Mismatches", 
                   style = "margin-top: 10px; margin-bottom: 10px;")
    }
  })
  
  observeEvent(input$reopen_mismatch, {
    req(rv$mismatched_wards)
    showModal(wardNameMismatchModal(rv$mismatched_wards))
  })
  
  
  
  output$variable_select <- renderUI({
    req(rv$raw_data)
    columns_after_wardname <- get_columns_after_wardname(rv$raw_data)
    selectInput("visualize_var", "Select Variable to Visualize", 
                choices = columns_after_wardname,
                selected = columns_after_wardname[1])
  })
  
  # Reactive expression for needs_cleaning
  rv$needs_cleaning <- reactive({
    req(rv$raw_data, rv$shp_data) # Ensure both files are loaded
    length(rv$na_columns) > 0 || (!is.null(rv$mismatched_wards) && nrow(rv$mismatched_wards) > 0)
  })
  
  
  
  # Data Cleaning
  observeEvent(input$data_cleaning, {
    req(rv$raw_data, rv$shp_data)
    
    showModal(modalDialog(
      title = "Data Cleaning",
      
      h4("Columns with Missing Values (NAs)"),
      tags$ul(
        tags$li(strong("Spatial neighbor mean:"), "Replaces missing values with the mean of neighboring wards."),
        tags$li(strong("Region mean:"), "Replaces missing values with the mean of the entire region."),
        tags$li(strong("Region mode:"), "Replaces missing values with the most common value in the region.")
      ),
      lapply(rv$na_columns, function(col) {
        fluidRow(
          column(6, h5(col)),
          column(6, 
                 selectInput(paste0("na_handling_", col), NULL,
                             choices = c("Spatial neighbor mean" = "spatial neighbor mean",
                                         "Region mean" = "region mean",
                                         "Region mode" = "region mode"),
                             selected = rv$na_handling_methods[[col]] %||% "spatial neighbor mean")
          )
        )
      }),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("apply_na_handling", "Apply")
      )
    ))
  })
  
  
  
  observeEvent(input$na_handling, {
    rv$na_handling_method <- input$na_handling
    print("Updated NA handling method:")
    print(rv$na_handling_method)
  })
  
  
  
  # Apply cleaning
  observeEvent(input$apply_na_handling, {
    req(rv$raw_data, rv$na_columns)
    
    rv$cleaned_data <- rv$raw_data
    
    for (col in rv$na_columns) {
      method <- input[[paste0("na_handling_", col)]]
      rv$na_handling_methods[[col]] <- method
      rv$cleaned_data <- switch(method,
                                "spatial neighbor mean" = handle_na_neighbor_mean(rv$cleaned_data, rv$shp_data, col),
                                "region mean" = handle_na_region_mean(rv$cleaned_data, col),
                                "region mode" = handle_na_region_mode(rv$cleaned_data, col)
      )
    }
    
    rv$cleaning_performed(TRUE)
    removeModal()
    showNotification("NA handling methods applied and stored.", type = "message")
    
    # Trigger plot update by updating the reactive value that the plot depends on 
    rv$cleaned_data <- rv$cleaned_data # This line triggers the update
    
    # Update variable selection (optional, to refresh choices if needed)
    updateSelectInput(session, "visualize_var", selected = input$visualize_var)
  })
  
  
  output$data_cleaning_button <- renderUI({
    if (rv$needs_cleaning()) {
      actionButton("data_cleaning", "Click to fill in missing values")
    }
  })
  
  
  
  # Modify the normalization process
  observeEvent(input$specify_relationships, {
    req(rv$raw_data)
    
    data_to_use <- if(rv$cleaning_performed()) rv$cleaned_data else rv$raw_data
    
    showModal(modalDialog(
      title = "Specify Variable Relationships with Malaria Risk",
      tags$div(
        tags$h4("Understanding Variable Relationships"),
        tags$p("Specifying variable relationships helps determine how each factor contributes to malaria risk:"),
        tags$ul(
          tags$li(strong("Direct relationship:"), "As the variable increases, malaria risk increases."),
          tags$li(strong("Inverse relationship:"), "As the variable increases, malaria risk decreases.")
        ),
        tags$p("This information is crucial for accurately calculating the composite risk score."),
        
        uiOutput("variable_relationships")
      ),
      
      footer = tagList(
        modalButton("Cancel"),
        actionButton("apply_relationships", "Apply Relationships")
      )
    ))
  })
  
  output$variable_relationships <- renderUI({
    req(rv$cleaned_data)
    columns_after_wardname <- get_columns_after_wardname(rv$cleaned_data)
    
    lapply(columns_after_wardname, function(var) {
      fluidRow(
        column(6, var),
        column(6, 
               radioButtons(paste0("relationship_", var), NULL,
                            choices = c("Direct" = "direct", "Inverse" = "inverse"),
                            selected = rv$variable_relationships[[var]] %||% "direct",
                            inline = TRUE)
        )
      )
    })
  })
  
  output$normalized_variable_select <- renderUI({
    req(rv$normalized_data)
    norm_vars <- grep("^normalization_", names(rv$normalized_data()), value = TRUE)
    selectInput("visualize_normalized_var", "Select Variable to Visualize", 
                choices = norm_vars,
                selected = norm_vars[1])
  })
  
  
  output$vulnerabilityMap <- renderGirafe({
    req(rv$data, rv$ward_rankings, rv$shp_data)
    
    map_data <- left_join(rv$shp_data, rv$ward_rankings, by = "WardName")
    
    plot <- ggplot() +
      geom_sf_interactive(data = map_data, aes(fill = overall_rank, 
                                               tooltip = paste(WardName, "\nRank:", overall_rank))) +
      scale_fill_viridis_c(option = "plasma", direction = -1, 
                           name = "Vulnerability Rank",
                           guide = guide_colorbar(
                             title.position = "top",
                             title.hjust = 0.5,
                             label.theme = element_text(size = 12), # Increase legend labels size
                             barwidth = 15, # Increase legend width 
                             barheight = 1 # Increase legend height
                           )) +
      theme_void() +
      labs(title = "Ward Vulnerability Map") +
      theme(legend.position = "bottom",
            legend.box = "vertical",
            legend.margin = margin(t = 10, b = 10),
            plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) 
    
    # Improved Annotations for Clarity:
    plot <- plot +
      annotate("text", x = -Inf, y = -Inf, label = "Low Vulnerability (Rank 1)", # Clearer label
               hjust = 0, vjust = -1, size = 4, color = "darkred", fontface = "bold") +
      annotate("text", x = Inf, y = -Inf, label = "High Vulnerability (Highest Rank)", # Clearer label
               hjust = 1, vjust = -1, size = 4, color = "darkblue", fontface = "bold")
    
    girafe(ggobj = plot, width_svg = 10, height_svg = 8) 
  })
  
  
  # Update the normalize_data function call
  observeEvent(input$apply_relationships, {
    req(rv$raw_data)
    
    data_to_use <- if(rv$cleaning_performed()) rv$cleaned_data else rv$raw_data
    
    columns_after_wardname <- get_columns_after_wardname(data_to_use)
    
    # Store variable relationships
    for (var in columns_after_wardname) {
      relationship <- input[[paste0("relationship_", var)]]
      rv$variable_relationships[[var]] <- relationship
    }
    
    # Normalize the data
    rv$normalized_data <- reactive({
      normalize_data(data_to_use[, c("WardName", columns_after_wardname)], rv$variable_relationships) 
    })
    
    removeModal()
    showNotification("Variable relationships applied and data normalized successfully.", type = "message")
    
    
    output$normalized_variable_select <- renderUI({
      norm_vars <- grep("^normalization_", names(rv$normalized_data()), value = TRUE)
      selectInput("visualize_normalized_var", "Select Normalized Variables to Visualize", 
                  choices = norm_vars,
                  selected = norm_vars[1])
    })
    
    # Update the normalization plot
    output$normalizationplot <- renderGirafe({
      req(rv$normalized_data(), input$visualize_normalized_var)
      plot_normalized_map(shp_data = rv$shp_data, 
                          processed_csv = rv$normalized_data(), 
                          selected_vars = input$visualize_normalized_var)
    })
  })
  
  
  observeEvent(input$plot_normalized, {
    req(rv$normalized_data, input$visualize_normalized_var, rv$shp_data)
    
    output$normalizationplot <- renderGirafe({
      plot_normalized_map(shp_data = rv$shp_data, 
                          processed_csv = rv$normalized_data(), 
                          selected_vars = input$visualize_normalized_var)
    })
  })
  
  
  # Raw data variable selection
  observeEvent(c(input$plot_data, rv$cleaned_data), { # Observe both button and data change
    req(rv$raw_data, rv$shp_data, input$visualize_var)
    
    
    if (!rv$cleaning_performed()) {
      # Before cleaning: One centered plot
      output$plotLayout <- renderUI({
        column(12, align = "center", girafeOutput("singlePlot", height = "500px"))
      })
      
      output$singlePlot <- renderGirafe({
        plot_map_00(variable_name = input$visualize_var,
                    shp_data_reactive = rv$shp_data,
                    dataframe_reactive = rv$raw_data,
                    title = "Original Data",
                    na_handling_method = NULL)
      })
    } else {
      # After cleaning: Two side-by-side plots
      output$plotLayout <- renderUI({
        fluidRow(
          column(6, girafeOutput("rawDataPlot", height = "500px")),
          column(6, girafeOutput("cleanedDataPlot", height = "500px"))
        )
      })
      
      output$rawDataPlot <- renderGirafe({
        plot_map_00(variable_name = input$visualize_var,
                    shp_data_reactive = rv$shp_data,
                    dataframe_reactive = rv$raw_data,
                    title = "Raw Data",
                    na_handling_method = NULL)
      })
      
      output$cleanedDataPlot <- renderGirafe({
        plot_map_00(variable_name = input$visualize_var,
                    shp_data_reactive = rv$shp_data,
                    dataframe_reactive = rv$cleaned_data,
                    title = "Cleaned Data",
                    na_handling_method = rv$na_handling_methods[[input$visualize_var]] %||% "None")
      })
    }
    
    # Update the explanation text
    output$visualizationExplanation <- renderText({
      if (!rv$cleaning_performed()) {
        "The map below shows the geographic distribution of the selected variable across various wards. If there are missing values in the plot, please click the (Fill in Missing Values) button to handle missing values. If no values are missing, proceed to the next tab."
      } else {
        paste("These maps showcase the effect of data cleaning on the selected variable's distribution. The left map represents the original, raw data. In contrast, the right map reveals the data after addressing missing values using the '",
              rv$na_handling_methods[[input$visualize_var]] %||% "None",
              "' method. The color gradients reflect the variable's magnitude, providing a clear visual comparison between the raw and cleaned datasets."
        )
      }
    })
  })
  
  
  
  # Composite Score
  output$composite_variable_select <- renderUI({
    req(rv$cleaned_data)
    columns_after_wardname <- get_columns_after_wardname(rv$cleaned_data)
    selectInput("composite_vars", "Select Variables for Composite Score",
                choices = columns_after_wardname,
                selected = NULL, multiple = TRUE)
  })
  
  observeEvent(input$plot_button, {
    req(rv$cleaned_data, input$composite_vars)
    
    if (length(input$composite_vars) < 2) {
      showNotification("Please select at least two variables for composite score calculation.", type = "warning")
      return()
    }
    
    withProgress(message = 'Calculating composite scores', value = 0, {
      tryCatch({
        # Step 1: Get variable impacts
        incProgress(0.1, detail = "Getting variable impacts...")
        variable_impacts <- sapply(input$composite_vars, function(var) rv$variable_relationships[[var]])
        
        # Step 2: Normalize data
        incProgress(0.2, detail = "Normalizing data...")
        normalized_data <- normalize_data(rv$cleaned_data[, c("WardName", input$composite_vars)], variable_impacts)
        
        if (is.null(normalized_data)) {
          showNotification("Error in data normalization. Check the console for details.", type = "error")
          return()
        }
        
        # Step 3: Calculate composite scores
        incProgress(0.3, detail = "Calculating composite scores...")
        composite_scores <- composite_score_models(normalized_data, selected_vars = input$composite_vars, shp_data = rv$shp_data)
        
        if (is.null(composite_scores)) {
          showNotification("Error in composite score calculation. Check the console for details.", type = "error")
          return()
        }
        
        # Step 4: Process composite scores for plotting
        incProgress(0.4, detail = "Processing scores...")
        processed_scores <- process_model_score(composite_scores$final_data)
        
        rv$flagged_models(processed_scores %>% 
                            filter(flag_not_ideal) %>% 
                            group_by(variable) %>% 
                            summarise(flagged_wards = paste(WardName, collapse = ", ")))
        
        # Step 5: Join with shapefile data
        incProgress(0.5, detail = "Joining with shapefile data...")
        combined_data <- left_join(processed_scores, rv$shp_data, by = "WardName")
        rv$data <- combined_data
        
        # Step 6: Generate model formulas table
        incProgress(0.6, detail = "Generating model formulas...")
        model_formulae_table <- models_formulas(composite_scores$model_formula)
        rv$output_data <- model_formulae_table
        
        # Step 7: Update plots and tables
        incProgress(0.7, detail = "Updating plots and tables...")
        
        # Update mapPlot
        output$mapPlot <- renderUI({
          req(rv$data, rv$output_data)
          
          plots <- plot_model_score_map(shp_data = rv$shp_data,
                                        processed_csv = rv$data,
                                        model_formulas = rv$output_data,
                                        maps_per_page = 4)
          
          do.call(tabsetPanel, lapply(seq_along(plots), function(i) {
            tabPanel(paste("Page", i), girafeOutput(paste0("mapPlot_", i)))
          }))
        })
        
        # Render individual plots
        observe({
          req(rv$data, rv$output_data)
          plots <- plot_model_score_map(shp_data = rv$shp_data,
                                        processed_csv = rv$data,
                                        model_formulas = rv$output_data,
                                        maps_per_page = 4)
          
          for (i in seq_along(plots)) {
            local({
              local_i <- i
              output[[paste0("mapPlot_", local_i)]] <- renderGirafe({
                plots[[local_i]]
              })
            })
          }
        })
        
        
        # Update normalizationplot
        output$normalizationplot <- renderGirafe({
          req(rv$normalized_data(), input$visualize_normalized_var)
          plot_normalized_map(shp_data = rv$shp_data, 
                              processed_csv = rv$normalized_data(), 
                              selected_vars = input$visualize_normalized_var)
        })
        
        # Update boxwhiskerPlots
        incProgress(0.8, detail = "Generating box plots...")
        output$boxwhiskerPlots <- renderPlotly({
          box_plot_results <- box_plot_function(rv$data)
          rv$ward_rankings <- box_plot_results$ward_rankings
          box_plot_results$plot %>% 
            layout(xaxis = list(fixedrange = TRUE), 
                   yaxis = list(fixedrange = TRUE))
        })
        
        # Step 8: Show completion notification
        incProgress(1, detail = "Completed!")
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
  })
  
  
  
  # Decision Tree
  
  
  
  # Replace the existing download handlers with these improved versions
  output$downloadPNG <- downloadHandler(
    filename = function() {
      paste0("decision_tree_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
    },
    content = function(file) {
      tryCatch({
        # Generate the diagram
        graph <- decision_tree_function(
          all_variables = get_columns_after_wardname(rv$cleaned_data),
          selected_variables = input$composite_vars,
          excluded_variables = setdiff(get_columns_after_wardname(rv$cleaned_data), 
                                       input$composite_vars),
          progress = rv$decision_tree_progress(),
          top_5_wards = if (!is.null(rv$ward_rankings)) {
            rv$ward_rankings %>%
              arrange(overall_rank) %>%
              slice_head(n = 5) %>%
              pull(WardName)
          } else {
            character(0)
          }
        )
        
        # Convert to SVG first
        svg_content <- DiagrammeRsvg::export_svg(graph)
        
        # Write to a temporary SVG file
        temp_svg <- tempfile(fileext = ".svg")
        writeLines(svg_content, temp_svg)
        
        # Convert SVG to PNG using rsvg
        rsvg::rsvg_png(temp_svg, file, width = 1200)
        
      }, error = function(e) {
        # Log the error
        message("Error in PNG export: ", e$message)
        stop("Failed to create PNG file. Error: ", e$message)
      }, finally = {
        # Cleanup
        if (exists("temp_svg") && file.exists(temp_svg)) {
          unlink(temp_svg)
        }
      })
    }
  )
  
  output$downloadPDF <- downloadHandler(
    filename = function() {
      paste0("decision_tree_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
    },
    content = function(file) {
      tryCatch({
        # Generate the diagram
        graph <- decision_tree_function(
          all_variables = get_columns_after_wardname(rv$cleaned_data),
          selected_variables = input$composite_vars,
          excluded_variables = setdiff(get_columns_after_wardname(rv$cleaned_data), 
                                       input$composite_vars),
          progress = rv$decision_tree_progress(),
          top_5_wards = if (!is.null(rv$ward_rankings)) {
            rv$ward_rankings %>%
              arrange(overall_rank) %>%
              slice_head(n = 5) %>%
              pull(WardName)
          } else {
            character(0)
          }
        )
        
        # Convert to SVG first
        svg_content <- DiagrammeRsvg::export_svg(graph)
        
        # Write to a temporary SVG file
        temp_svg <- tempfile(fileext = ".svg")
        writeLines(svg_content, temp_svg)
        
        # Convert SVG to PDF using rsvg
        rsvg::rsvg_pdf(temp_svg, file)
        
      }, error = function(e) {
        # Log the error
        message("Error in PDF export: ", e$message)
        stop("Failed to create PDF file. Error: ", e$message)
      }, finally = {
        # Cleanup
        if (exists("temp_svg") && file.exists(temp_svg)) {
          unlink(temp_svg)
        }
      })
    }
  )
  
  observe({
    rv$decision_tree_progress(modifyList(rv$decision_tree_progress(), list(
      data_loaded = TRUE,
      variables_selected = TRUE,
      normalization_done = TRUE,
      composite_scores_calculated = TRUE
    )))
  })
  
  
  wrap_text <- function(text, width = 20) {
    paste(strwrap(text, width = width), collapse = "\n")
  }
  

  decision_tree_function <- function(all_variables, selected_variables, excluded_variables, progress, top_5_wards = character(0)) {
    # Format variables lists with bullets
    format_var_list <- function(vars) {
      if (length(vars) == 0) return("None")
      paste(sapply(vars, function(x) paste0("• ", x)), collapse = "\\n")
    }
    
    # Format top 5 wards
    formatted_top_5 <- if (length(top_5_wards) > 0) {
      paste("1.", top_5_wards[1], "\\n2.", top_5_wards[2], "\\n3.", 
            top_5_wards[3], "\\n4.", top_5_wards[4], "\\n5.", top_5_wards[5])
    } else {
      "No wards available"
    }
    
    # Define precise professional color scheme
    colors <- list(
      navy = "#1B2631",       # Darker navy for headers
      orange = "#E67E22",     # Bright orange for diamond
      teal = "#16A596",       # Bright teal for included
      gray = "#7F8C8D",       # Medium gray for excluded
      green = "#27AE60",      # Bright green for process
      blue = "#2980B9",       # Bright blue for maps
      purple = "#8E44AD",     # Bright purple for final nodes
      arrow = "#34495E"       # Dark arrow color
    )
    
    graph_string <- sprintf('
  digraph G {
    graph [rankdir=LR,
           nodesep=0.6,
           ranksep=0.8,
           splines=ortho,
           pad=0.5,
           compound=true]
    
    node [shape=rect,
          style="filled,rounded",
          fontname="Arial",
          fontsize=11,
          margin="0.2,0.2",
          penwidth=1.2]
    
    edge [color="%s",
          penwidth=1.2,
          arrowsize=0.9,
          arrowhead=vee]
    // Initial nodes
    start [label="Malaria Risk Assessment\\nVariable Selection"
           fillcolor="%s"
           fontcolor="white"]
    vars [label="Variables:\\n%s"
          fillcolor="%s"
          fontcolor="white"]
    // Evaluation diamond
    eval [label="Variable\\nEvaluation"
          shape=diamond
          fillcolor="%s"
          fontcolor="white"
          width=1.5
          height=1.5]
    // Variable groups
    included [label="Included Variables:\\n%s"
             fillcolor="%s"
             fontcolor="white"]
    excluded [label="Excluded Variables:\\n%s"
             fillcolor="%s"
             fontcolor="white"]
    // Processing and results
    process [label="Data Normalization &\\nComposite Score Calculation"
            fillcolor="%s"
            fontcolor="white"]
    maps [label="Generated Risk Maps\\nfor All Combinations"
          fillcolor="%s"
          fontcolor="white"]
    recommended [label="Recommended Risk Map\\nby the Box and Whisker Plot"
                fillcolor="%s"
                fontcolor="white"]
    priority [label="Top 5 Wards\\nfor Re-Prioritization:\\n%s"
             fillcolor="%s"
             fontcolor="white"]
    // Edge definitions with improved spacing
    start -> vars [weight=2]
    vars -> eval [weight=2]
    eval -> included [label=" Include"]
    eval -> excluded [label=" Exclude"]
    included -> process [weight=2]
    process -> maps [weight=2]
    maps -> recommended [weight=2]
    recommended -> priority [weight=2]
    // Rank definitions for better alignment
    {rank=same; start; vars}
    {rank=same; included; excluded}
    {rank=same; process; maps}
    {rank=same; recommended; priority}
    // Add invisible edges for better spacing
    included -> excluded [style=invis]
    maps -> recommended [style=invis]
  }',
                            colors$arrow,
                            colors$navy, format_var_list(all_variables), colors$navy,
                            colors$orange,
                            format_var_list(selected_variables), colors$teal,
                            format_var_list(excluded_variables), colors$gray,
                            colors$green, colors$blue,
                            colors$purple, formatted_top_5, colors$purple
    )
    
    grViz(graph_string)
  }
  
  
  
  # Dynamic UI for variable selection
  output$decision_tree_variables <- renderUI({
    req(rv$cleaned_data)
    all_vars <- get_columns_after_wardname(rv$cleaned_data)
    checkboxGroupInput("selected_vars", "Select Variables:",
                       choices = all_vars,
                       selected = all_vars)
  })
  
  # Initialize the decision tree
  output$decisionTree <- renderGrViz({
    req(rv$cleaned_data)
    all_vars <- get_columns_after_wardname(rv$cleaned_data)
    selected_vars <- input$composite_vars
    excluded_vars <- setdiff(all_vars, selected_vars)
    
    top_5_wards <- if (!is.null(rv$ward_rankings)) {
      rv$ward_rankings %>%
        arrange(desc(overall_rank)) %>%
        slice_head(n = 5) %>%
        pull(WardName)
    } else {
      character(0)
    }
    
    decision_tree_function(all_vars, selected_vars, excluded_vars, rv$decision_tree_progress(), top_5_wards)
  })
  
  # In the observe block
  observe({
    req(rv$decision_tree_progress())
    output$decisionTree <- renderGrViz({
      req(rv$cleaned_data)
      all_vars <- get_columns_after_wardname(rv$cleaned_data)
      selected_vars <- input$composite_vars
      excluded_vars <- setdiff(all_vars, selected_vars)
      
      top_5_wards <- if (!is.null(rv$ward_rankings)) {
        rv$ward_rankings %>%
          arrange(overall_rank) %>%
          slice_head(n = 5) %>%
          pull(WardName)
      } else {
        character(0)
      }
      
      decision_tree_function(all_vars, selected_vars, excluded_vars, rv$decision_tree_progress(), top_5_wards)
    })
  })
  
  
  # Manual Labelling Tab
  
  # Set the directories for the shapefiles and CSV files
  shapefile_dir <- "www/data/shapefiles"
  csv_dir <- "www/data/csv"
  
  # Function to process and view shapefile and CSV for a given ward
  process_and_view_shapefile_and_csv <- function(ward_name, shapefile_dir, csv_dir) {
    # Get the full path to the directories
    shapefile_dir <- file.path(getwd(), shapefile_dir)
    csv_dir <- file.path(getwd(), csv_dir)
    # Split the selected ward name into parts
    ward_parts <- unlist(strsplit(tolower(ward_name), " "))
    
    # Find matching shapefile ZIP file using partial matching
    shapefile_zip_files <- list.files(shapefile_dir, pattern = ".zip$", full.names = TRUE)
    matching_shapefile <- shapefile_zip_files[Reduce(`&`, lapply(ward_parts, function(x) grepl(x, tolower(shapefile_zip_files))))]
    
    # Check if a single matching shapefile is found
    if (length(matching_shapefile) != 1) {
      stop(paste("Found", length(matching_shapefile), "matching shapefiles for ward:", ward_name))
    }
    
    shapefile_zip_path <- matching_shapefile
    cat("Extracting shapefile from:", shapefile_zip_path, "\n")
    
    # Extract the shapefile from the ZIP archive
    temp_dir <- tempdir()
    unzip(shapefile_zip_path, exdir = temp_dir)
    
    # Find the shapefile path within the extracted files
    shapefile_path <- list.files(temp_dir, pattern = "\\.shp$", full.names = TRUE)
    
    if (length(shapefile_path) == 0) {
      stop("No shapefile (.shp) found in the extracted archive.")
    } else if (length(shapefile_path) > 1) {
      # If multiple shapefiles are found, use the one that matches the ward name most closely
      shapefile_path <- shapefile_path[which.max(sapply(shapefile_path, function(x) sum(grepl(ward_parts, tolower(x)))))]
    }
    
    cat("Shapefile path:", shapefile_path, "\n")
    
    # Read the shapefile
    shapefile <- st_read(shapefile_path)
    
    # Find the CSV file path
    csv_files <- list.files(csv_dir, pattern = ".csv$", full.names = TRUE)
    matching_csv <- csv_files[Reduce(`&`, lapply(ward_parts, function(x) grepl(x, tolower(csv_files))))]
    
    # Check if a single matching CSV is found
    if (length(matching_csv) != 1) {
      stop(paste("Found", length(matching_csv), "matching CSV files for ward:", ward_name))
    }
    
    csv_path <- matching_csv
    cat("CSV file path:", csv_path, "\n")
    
    # Read the CSV file
    data_points <- read.csv(csv_path, stringsAsFactors = FALSE)
    
    # Print available columns for debugging
    print(paste("Available columns:", paste(names(data_points), collapse = ", ")))
    
    # Function to detect coordinate system
    detect_coordinate_system <- function(data) {
      lat_col <- grep("latitude", names(data), value = TRUE, ignore.case = TRUE)
      lon_col <- grep("longitude", names(data), value = TRUE, ignore.case = TRUE)
      
      if (length(lat_col) == 0 || length(lon_col) == 0) {
        stop("The CSV file must contain 'latitude' and 'longitude' columns in their names.")
      }
      
      # Check if coordinates are likely to be in decimal degrees
      if (max(abs(data[[lat_col]])) <= 90 && max(abs(data[[lon_col]])) <= 180) {
        return(list(type = "WGS84", lat = lat_col, lon = lon_col))
      } else {
        return(list(type = "projected", x = lon_col, y = lat_col))
      }
    }
    
    # Detect coordinate system
    coord_system <- detect_coordinate_system(data_points)
    
    print(paste("Detected coordinate system:", coord_system$type))
    
    # Create an sf object from the data points
    if (coord_system$type == "WGS84") {
      data_points_sf <- st_as_sf(data_points, coords = c(coord_system$lon, coord_system$lat), crs = 4326)
    } else {
      # Assuming the projected coordinate system is the same as the shapefile
      data_points_sf <- st_as_sf(data_points, coords = c(coord_system$x, coord_system$y), crs = st_crs(shapefile))
    }
    
    # Transform shapefile and data points to WGS84
    shapefile_wgs84 <- st_transform(shapefile, crs = 4326)
    df_wgs84 <- st_transform(data_points_sf, crs = 4326)
    
    # Filter by settlement type
    formal_points <- df_wgs84[df_wgs84$Impression == "Formal", ]
    informal_points <- df_wgs84[df_wgs84$Impression == "Informal", ]
    slum_points <- df_wgs84[df_wgs84$Impression == "Slum", ]
    nonresidential_points <- df_wgs84[df_wgs84$Impression == "Non residential", ]
    
    # Create a Leaflet map
    map <- leaflet() %>%
      addTiles() %>%
      fitBounds(lng1 = st_bbox(shapefile_wgs84)[[1]],
                lat1 = st_bbox(shapefile_wgs84)[[2]],
                lng2 = st_bbox(shapefile_wgs84)[[3]],
                lat2 = st_bbox(shapefile_wgs84)[[4]])
    
    # Add shapefile to the map
    map <- map %>%
      addPolygons(data = shapefile_wgs84,
                  color = "black",
                  weight = 1,
                  smoothFactor = 0.5,
                  opacity = 1.0,
                  fillOpacity = 0.0,
                  highlightOptions = highlightOptions(
                    weight = 2,
                    color = "#666666",
                    fillOpacity = 0.0,
                    bringToFront = TRUE),
                  label = shapefile_wgs84$WardName,
                  popup = shapefile_wgs84$WardName)
    
    # Function to add points to the map
    add_points <- function(map, points, color) {
      if (nrow(points) > 0) {
        print(paste("Adding points for:", color))
        print(head(points))
        print(st_coordinates(points))
        map <- map %>%
          addCircleMarkers(data = points,
                           lng = ~st_coordinates(geometry)[,1],
                           lat = ~st_coordinates(geometry)[,2],
                           radius = 5,
                           color = color,
                           stroke = TRUE,
                           weight = 2,
                           fillOpacity = 0.7,
                           popup = ~Impression)
      }
      return(map)
    }
    
    # Add points to the map
    map <- add_points(map, formal_points, "#0074D9")
    map <- add_points(map, informal_points, "#FF4136")
    map <- add_points(map, slum_points, "#FFDC00")
    map <- add_points(map, nonresidential_points, "#2ECC40")
    
    # Add legend to the map
    map <- map %>%
      addLegend(
        position = "bottomright",
        colors = c("#0074D9", "#FF4136", "#FFDC00", "#2ECC40"),
        labels = c("Formal", "Informal", "Slum", "Nonresidential"),
        title = "Settlement Type",
        opacity = 0.7
      )
    
    return(map)
  }
  
  # Get top wards for the dropdown
  top_wards <- reactive({
    req(rv$ward_rankings)
    rv$ward_rankings %>%
      arrange(overall_rank) %>%
      slice_head(n = 5) %>%
      pull(WardName)
  })
  
  # Ward selection dropdown for Manual Labelling tab
  output$ward_select <- renderUI({
    req(top_wards())
    selectInput("selected_ward", "Select Ward:", choices = top_wards())
  })
  
  observeEvent(input$plot_ward_map, {
    req(input$selected_ward)
    selected_ward <- input$selected_ward
    
    # Render the Leaflet map
    output$ward_map <- renderLeaflet({
      tryCatch({
        process_and_view_shapefile_and_csv(selected_ward, shapefile_dir, csv_dir)
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        cat("Error details:", conditionMessage(e), "\n")
        return(NULL)
      })
    })
  })
  
  
}

shinyApp(ui = ui, server = server)