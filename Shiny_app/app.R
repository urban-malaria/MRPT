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
             # Use a fluidRow to create a two-column layout
             fluidRow(
               # Column for text instructions 
               column(6,
                      h3("Instructions for Using the Malaria Risk Mapping Tool (Testing Phase)"),
                      p("Welcome to the testing phase of our Malaria Risk Mapping Tool. Please follow these steps to explore the application's features:"),
                      h4("1. Preparing Your Data"),
                      tags$ul(
                        tags$li(a("Download the test dataset from this Google Drive link:", 
                                  href="https://drive.google.com/drive/u/0/folders/1iB103WqBbeIEhhMZ1axHTRaC3zZVnE-S", 
                                  target="_blank")),
                        tags$li("Ensure you have both the CSV file with variable data and the corresponding shapefile for your region of interest."),
                        tags$li("The CSV file should include columns for WardName and various risk factors (e.g., settlement quality, rainfall, etc.)."),
                        tags$li("Compress the shapefile components into a zip file.")
                      ),
                      h4("2. Input Variables (Data and Shapefiles) Tab"),
                      tags$ul( 
                        tags$li("Upload your CSV file and zipped shapefile using the respective upload buttons."),
                        tags$li("If there are mismatches between ward names in your CSV and shapefile, you'll be prompted to resolve them."),
                        tags$li("Select a variable to visualize from the dropdown menu and click 'Plot Maps' to see its distribution."),
                        tags$li("If your data has missing values, use the 'Click to fill in missing values' button to handle them.")
                      ),
                      h4("3. Normalization Tab"),
                      tags$ul( 
                        tags$li("Click 'SPECIFY VARIABLE RELATIONSHIPS' to define how each variable relates to malaria risk (direct or inverse)."),
                        tags$li("Select a normalized variable to visualize from the dropdown menu."),
                        tags$li("Click 'PLOT NORMALIZED MAP' to see the distribution of the normalized variable.")
                      ),
                      h4("4. Composite Score Distribution Tab"),
                      tags$ul(
                        tags$li("Select at least two variables to include in your composite risk score."),
                        tags$li("Click 'Calculate' to generate composite score maps for different combinations of your selected variables."),
                        tags$li("Examine the resulting maps to see how different variable combinations affect the risk distribution.")
                      ),
                      h4("5. Box and Whisker Plot Tab"),
                      tags$ul( 
                        tags$li("This tab shows the distribution of vulnerability scores across wards."),
                        tags$li("Toggle between the box plot view and map view using the 'Show Map View' checkbox."),
                        tags$li("In the box plot view, examine the distribution of ranks for each ward."),
                        tags$li("In the map view, see the geographic distribution of overall vulnerability ranks.")
                      ),
                      h4("Feedback"),
                      p("As you explore each feature, please note any issues, inconsistencies, or suggestions for improvement. Your feedback is crucial for refining this tool. Report any bugs or share your thoughts with the development team."),
                      h4("Note on Data Privacy"),
                      p("Remember, this is a testing phase. Please use only the provided test data or non-sensitive data. Do not upload any real, sensitive, or personal data to this application during testing."),
                      p("Thank you for your participation in testing the Malaria Risk Mapping Tool!")
               ),
               # Column for the image
               column(6, 
                      tags$br(), tags$br(), tags$br(), tags$br(), 
                      tags$img(src = "digital_abstract.png", height = "600px", width = "auto"),
                      style = 'text-align: center',
                      tags$h6("The figure above shows a digital abstract first presented in Ozodiegwu et-al (in-press), 
                              and it shows how the application was used to develop the malaria risk map in Ilorin")
               )
             ), # Close fluidRow
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
             ),

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
                 # Make the plot conditional on button click:
                 conditionalPanel(
                   condition = "input.plot_normalized > 0", 
                   girafeOutput("normalizationplot")
                 )
               )
             ),
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
    ),
    
    
    tabPanel("Composite Score distribution", 
             tags$br(),tags$br(),
             sidebarLayout(
               sidebarPanel(
                 tags$h4("Select a variable in the dataset to visualise:"),
                 tags$p("Select at least two variables:", style = "color: black;"),
                 uiOutput("composite_variable_select"),
                 actionButton("plot_button", "Calculate")
               ),
               mainPanel(
                 uiOutput("mapPlot"),
                 tags$br(),
                 tags$div(
                   style = "text-align: justify; font-size: 12px; color: #666;",
                   "The maps above show the distribution of malaria risk scores across different wards, calculated using various combinations of variables. Each map represents a different model, with the variables used listed in the title. The color scale ranges from yellow (very low risk) to dark red (very high risk). This visualization helps identify areas of high concern and compare how different combinations of factors affect the risk assessment."
                 ),
                 tags$br(),
                 tableOutput("dataTable"),
                 tags$p(style = "font-style: italic; margin-top: 10px; font-size: 12px;", 
                        "Hover over the maps to see detailed information for each ward.")
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
                        plotlyOutput("boxwhiskerPlots", height = "600px")
                      ),
                      conditionalPanel(
                        condition = "input.show_map == true",
                        girafeOutput("vulnerabilityMap", height = "600px")
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
                          style = "background-color: #f5f5f5; border: 1px solid #e3e3e3; border-radius: 4px; padding: 15px; height: 600px; overflow-y: auto;",
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
    
    
#    tabPanel("Decision Tree",
#             fluidRow(
#               column(12,
#                      h3("Decision Tree Visualization"),
#                      p("This decision tree illustrates the process of variable selection and risk mapping."),
#                      br(),
#                      DiagrammeROutput("decisionTreePlot", height = "600px")
#               )
#             )
#    ),
    
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
  
  # new reactive value to track whether cleaning was performed
  rv$cleaning_performed <- reactiveVal(FALSE)
  
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
        rv$shp_data <- shp_data
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
        options <- c("None", mismatches$Shapefile_Options[[i]])
        
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
                             choices = c("Spatial neighbor mean" = "spatial_neighbor_mean",
                                         "Region mean" = "region_mean",
                                         "Region mode" = "region_mode"),
                             selected = rv$na_handling_methods[[col]] %||% "spatial_neighbor_mean")
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
                                "spatial_neighbor_mean" = handle_na_neighbor_mean(rv$cleaned_data, rv$shp_data, col),
                                "region_mean" = handle_na_region_mean(rv$cleaned_data, col),
                                "region_mode" = handle_na_region_mode(rv$cleaned_data, col)
      )
    }
    
    rv$cleaning_performed(TRUE)
    removeModal()
    showNotification("NA handling methods applied and stored.", type = "message")
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
  observeEvent(input$plot_data, {
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
        "The map below shows the geographic distribution of the selected variable across various wards. If there are missing values in the plot, please click the (Fill in Missing Values) button. If no values are missing, proceed to the next tab."
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
    #print(paste("Number of rows in cleaned_data:", nrow(rv$cleaned_data)))
    #print(paste("Number of rows in shp_data:", nrow(rv$shp_data)))
    
    
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
      # Inside the server function, replace the existing mapPlot output with this:
      output$mapPlot <- renderUI({
        req(rv$data, rv$output_data)
        
        plots <- plot_model_score_map(shp_data = rv$shp_data,
                                      processed_csv = rv$data,
                                      model_formulas = rv$output_data,
                                      maps_per_page = 4)  # Adjust this number as needed
        
        # Create a tabset panel for pagination
        do.call(tabsetPanel, lapply(seq_along(plots), function(i) {
          tabPanel(paste("Page", i), girafeOutput(paste0("mapPlot_", i)))
        }))
      })
      
      # Add these observers to render each plot
      observe({
        req(rv$data, rv$output_data)
        plots <- plot_model_score_map(shp_data = rv$shp_data,
                                      processed_csv = rv$data,
                                      model_formulas = rv$output_data,
                                      maps_per_page = 4)  # Adjust this number as needed
        
        for (i in seq_along(plots)) {
          local({
            local_i <- i
            output[[paste0("mapPlot_", local_i)]] <- renderGirafe({
              plots[[local_i]]
            })
          })
        }
      })
      
      output$dataTable <- renderTable({
        rv$output_data
      })
      
      # Normalization plot
      output$normalizationplot <- renderGirafe({
        req(rv$normalized_data(), input$visualize_normalized_var)
        plot_normalized_map(shp_data = rv$shp_data, 
                            processed_csv = rv$normalized_data(), 
                            selected_vars = input$visualize_normalized_var)
      })
      
      # Box whisker plot
      output$boxwhiskerPlots <- renderPlotly({
        box_plot_results <- box_plot_function(rv$data)
        rv$ward_rankings <- box_plot_results$ward_rankings
        box_plot_results$plot
        
        # disable zoom for x and y axes
        box_plot_results$plot %>% 
          layout(xaxis = list(fixedrange = TRUE), 
                 yaxis = list(fixedrange = TRUE)) 
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
 
  
  # # Text wrapping function
  # wrap_text <- function(text, width = 30) {
  #   paste(strwrap(text, width = width), collapse = "\n")
  # }
  # 
  # decision_tree_function <- function(all_variables, selected_variables, excluded_variables) {
  #   # Create nodes DataFrame
  #   nodes <- create_node_df(
  #     n = 7,
  #     label = c(
  #       wrap_text(paste(("The dataset had the following variables:"), 
  #                       paste(all_variables, collapse = ", "))),
  #       "Check the map plot to determine if it depicts the variable under consideration", 
  #       wrap_text(paste("Variables included in the composite score:", 
  #                       paste(selected_variables, collapse = ", "))),
  #       wrap_text(paste("Variables excluded from the composite score:", 
  #                       paste(excluded_variables, collapse = ", "))), 
  #       "Normalization and composite score calculation",
  #       "Malaria risk maps generated from various combinations of all included variables", 
  #       "Malaria risk map recommended by the box and whisker plot"
  #     ),
  #     shape = c("box", "diamond", "ellipse", "ellipse", "box", "ellipse", "ellipse")
  #   )
  #   
  #   # Create edges DataFrame
  #   edges <- create_edge_df(
  #     from = c(1, 2, 2, 3, 5, 5),
  #     to = c(2, 3, 4, 5, 6, 7),
  #     label = c("", "yes", "no", "", "all variables", "recommended")
  #   )
  #   
  #   # Create graph
  #   graph <- create_graph(nodes_df = nodes, edges_df = edges)
  #   
  #   # Render the graph
  #   render_graph(graph)
  # }
  # 
  # 
  # # Define all_variables as a reactive expression
  # all_variables <- reactive({
  #   req(rv$cleaned_data)
  #   setdiff(names(rv$cleaned_data), "WardName")
  # })
  # 
  # excluded_variables <- reactive({
  #   req(all_variables(), input$composite_vars)
  #   setdiff(all_variables(), input$composite_vars)
  # })
  # 
  # output$decisionTreePlot <- renderDiagrammeR({
  #   req(all_variables(), input$composite_vars)
  #   
  #   selected_variables <- input$composite_vars
  #   excluded_vars <- excluded_variables()
  #   
  #   tryCatch({
  #     decision_tree_function(all_variables(), selected_variables, excluded_vars)
  #   }, error = function(e) {
  #     message("Error in decision tree function: ", e$message)
  #     return(NULL)
  #   })
  # })
  
}

shinyApp(ui = ui, server = server)