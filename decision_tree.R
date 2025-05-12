library(shiny)
library(glue)
library(DiagrammeR)
library(dplyr)

# Define the wrap_text function
wrap_text <- function(text, width = 30) {
  paste(strwrap(text, width = width), collapse = "\n")
}

# Define the decision tree function
decision_tree_function <- function(all_variables, selected_variables, excluded_variables) {
  # Function to plot the decision tree
  node_data <- data.frame(
    Name = c("Node1", "Node2", "Node3", "Node4", "Node5", "Node6", "Node7"),
    Label = c(
      wrap_text(paste(("The dataset had the following variables:"), paste(all_variables, collapse = ", "))),
      wrap_text("Check the map plot to determine if it depicts the variable under consideration"), 
      wrap_text(paste("Variables included in the composite score:", paste(selected_variables, collapse = ", "))),
      wrap_text(paste("Variables excluded from the composite score:", paste(excluded_variables, collapse = ", "))), 
      wrap_text("Normalization and composite score calculation"),
      wrap_text("Malaria risk maps generated from various combinations of all included variables"), 
      wrap_text("Malaria risk map recommended by the box and whisker plot")
    ),
    Shape = c("box", "diamond", "ellipse", "ellipse", "box", "ellipse", "ellipse"),
    stringsAsFactors = FALSE
  )
  
  # Create nodes dynamically using R
  nodes <- paste0(
    node_data$Name, " [label = '", node_data$Label, "', shape = ", node_data$Shape, "]",
    collapse = "\n  "
  )
  
  # Create edges dynamically using R with labels
  edges <- "Node1 -> Node2\n  Node2 -> Node3 [label = 'yes']\n  Node2 -> Node4 [label = 'no']\n Node3 -> Node5\n Node5 -> Node6 [label = 'all variables']\n Node5 -> Node7 [label = 'recommended']"
  
  # Construct the graph string
  graph_string <- glue("
    digraph flowchart {{
      rankdir=TB
      node [style = filled, fillcolor = lightblue, fontname = Helvetica, fontsize = 12]
      {nodes}
      {{
        rank = same; Node3; Node4;
      }}
      {edges}
    }}
  ")
  
  # Render the graph
  grViz(graph_string)
}

# Define the UI
ui <- fluidPage(
  titlePanel("Interactive Decision Tree"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selected_vars", "Select Variables:", 
                         choices = c("enhanced vegetation index", "settlement type", "distance to water bodies",
                                     "test positivity rate", "dumpsites", "rainfall"),
                         selected = c("enhanced vegetation index", "settlement type", "distance to water bodies",
                                      "test positivity rate")),
      actionButton("update", "Update Decision Tree")
    ),
    mainPanel(
      grVizOutput("decisionTree")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  all_variables <- c("enhanced vegetation index", "settlement type", "distance to water bodies",
                     "test positivity rate", "dumpsites", "rainfall")
  
  observeEvent(input$update, {
    selected_variables <- input$selected_vars
    excluded_variables <- all_variables[!all_variables %in% selected_variables]
    
    output$decisionTree <- renderGrViz({
      decision_tree_function(all_variables, selected_variables, excluded_variables)
    })
  })
  
  # Initialize the decision tree
  output$decisionTree <- renderGrViz({
    selected_variables <- c("enhanced vegetation index", "settlement type", "distance to water bodies",
                            "test positivity rate")
    excluded_variables <- all_variables[!all_variables %in% selected_variables]
    decision_tree_function(all_variables, selected_variables, excluded_variables)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
