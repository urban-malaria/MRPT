library(shiny)
library(dplyr)

# Define the function to correct ward names
correct_ward_names <- function(raw_data, mismatched_wards_shapefile, input) {
  
  replacements <- sapply(seq_along(mismatched_wards_shapefile), function(i) {
    corrected_name <- mismatched_wards_shapefile[i]
    original_name <- input[[corrected_name]]
    list(original_name = original_name, corrected_name = corrected_name)
  }, simplify = FALSE)
  
  for (replacement in replacements) {
    raw_data <- raw_data %>%
      mutate(WardName = ifelse(WardName == replacement$original_name,
                               replacement$corrected_name,
                               WardName))
  }
  
  return(raw_data)
}

# Example Shiny app
ui <- fluidPage(
  titlePanel("Correct Ward Names in Shapefile"),
  sidebarLayout(
    sidebarPanel(
      actionButton("save", "Save Changes")
    ),
    mainPanel(
      tableOutput("dataTable")
    )
  )
)

server <- function(input, output, session) {
  # Example reactive values
  rv <- reactiveValues(
    raw_data = data.frame(
      ID = 1:10,
      WardName = c("Ward1", "gawuna", "gwagwarwa", "Ward4", "Ward5", "Ward6", "Goron Dut", "Ward8", "Ward9", "Ward10"),
      Value = c(28, 34, 23, 45, 32, 36, 29, 40, 31, 27),
      stringsAsFactors = FALSE
    ),
    mismatched_wards_shapefile = c("Ward2", "Ward3", "Ward7")
  )
  
  # Example input values (simulating user input from Shiny)
  input_values <- reactive({
    list(
      "Ward2" = "gawuna",
      "Ward3" = "gwagwarwa",
      "Ward7" = "Goron Dut"
    )
  })
  
  observeEvent(input$save, {
    rv$raw_data <- correct_ward_names(rv$raw_data, rv$mismatched_wards_shapefile, input_values())
    showModal(modalDialog(
      title = "Success",
      "Ward names have been corrected.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  output$dataTable <- renderTable({
    rv$raw_data
  })
}

shinyApp(ui = ui, server = server)
