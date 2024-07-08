# Load required libraries
library(sf)
library(vroom)
library(stringr)
library(dplyr)
library(tidyr)
library(reshape2)
library(ggplot2)
library(ggiraph)
library(RColorBrewer)
library(gridExtra)
library(shinythemes)
library(viridis)
library(shinyjs)
library(readxl)
library(shinydashboard)
library(shinyBS)
library(rlang)
library(shiny)
library(bslib)
library(plotly)

# Function to generate a list of column name patterns
generate_pattern_list <- function(df) {
  sapply(names(df), function(col) {
    pattern <- tolower(gsub("\\s+", "_", col))
    c(col, pattern)
  }, simplify = FALSE, USE.NAMES = TRUE)
}

# Function to rename columns based on generated patterns
rename_columns <- function(df) {
  pattern_list <- generate_pattern_list(df)
  for (pattern in names(pattern_list)) {
    df <- df %>% rename_with(~ pattern, all_of(intersect(names(df), pattern_list[[pattern]])))
  }
  df
}

# Function to set a custom theme for maps
map_theme <- function(){
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        rect = element_blank(),
        plot.background = element_rect(fill = "white", colour = NA), 
        plot.title = element_text(hjust = 0.5),
        legend.title = element_text(hjust = 0.5, size = 8, colour = 'black'),
        legend.text = element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}


theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 22, color = "black"), 
          axis.text.y = element_text(size = 22, color = "black"),
          axis.title.x = element_text(size = 22),
          axis.title.y = element_text(size =22),
          legend.title=element_text(size=22, colour = 'black'),
          legend.text =element_text(size = 22, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}



# Function to plot an interactive map
plot_map_00 <- function(variable_name, shp_data_reactive, raw_dataframe_reactive) {
  plot <- ggplot(data = shp_data_reactive) +
    geom_sf_interactive(color = "black", fill = "white") +
    geom_sf_interactive(data = raw_dataframe_reactive,
                        aes(geometry = geometry,
                            fill = !!sym(variable_name),  
                            tooltip = paste(WardName, "(", round(as.numeric(!!sym(variable_name)), 3), ")"))) +
    scale_fill_viridis_c(name = "") +
    labs(title = variable_name, subtitle = '', fill = "", x = NULL, y = NULL) +
    map_theme()
  
  girafe(ggobj = plot)
}



# Function to check for missing values:

check_missing_values <- function(data) {
  missing_cols <- sapply(data, function(x) any(is.na(x)))
  cols_with_missing <- names(missing_cols[missing_cols])
  return(cols_with_missing)
}

# Function to check for wardname mismatches:
check_wardname_mismatches <- function(csv_data, shp_data) {
  csv_wardnames <- csv_data$WardName
  shp_wardnames <- shp_data$WardName
  mismatched_wards <- setdiff(csv_wardnames, shp_wardnames)
  return(mismatched_wards)
}


# Normalization function
normalize_data <- function(uploaded_data, variable_impacts) {
  numeric_cols <- sapply(uploaded_data, is.numeric)
  
  scoring_data <- uploaded_data %>%
    mutate(across(where(is.numeric), 
                  ~{
                    normalized <- (. - min(., na.rm = TRUE)) / (max(., na.rm = TRUE) - min(., na.rm = TRUE))
                    if (variable_impacts[cur_column()] == "inverse") {
                      1 - normalized
                    } else {
                      normalized
                    }
                  },
                  .names = "normalization_{tolower(.col)}"))
  
  if (!"WardName" %in% names(scoring_data)) {
    stop("WardName column is missing in the data.")
  }
  
  return(scoring_data)
}


# Function to plot normalized map
plot_normalized_map <- function(shp_data, processed_csv) {
  palette_func <- brewer.pal(5, "YlOrRd")
  
  plot <- ggplot(data = shp_data) +
    geom_sf_interactive(color = "black", fill = "white") + 
    geom_sf_interactive(data = processed_csv,
                        aes(geometry = geometry, fill = class, 
                            tooltip = paste(WardName, variable, 
                                            "\nValue:", round(value, 3)))) +
    facet_wrap(~variable, ncol = 2) +
    scale_fill_discrete(drop=FALSE, name="score", type = palette_func) +
    labs(subtitle='', title='', fill = "score") +
    theme_void() +
    theme(panel.background = element_blank(),
          strip.text = element_text(size = 12),
          legend.position = "right",
          legend.key.size = unit(0.8, 'cm'),
          legend.text = element_text(size = 10))
  
  girafe(ggobj = plot, width_svg = 10, height_svg = 8)
}


# Function to calculate composite scores for different models

composite_score_models <- function(normalized_data, selected_vars) {
  # Get normalized column names for selected variables only
  norm_cols <- paste0("normalization_", tolower(selected_vars))
  
  if (length(norm_cols) < 2) {
    stop("Error: At least two variables are required for composite score calculation.")
  }
  
  # Generate combinations
  model_combinations <- list()
  if (length(norm_cols) == 2) {
    # If only two variables are selected, create just one model
    model_combinations <- list(norm_cols)
  } else {
    # For more than two variables, generate all combinations
    for (i in 2:length(norm_cols)) {
      model_combinations <- c(model_combinations, combn(norm_cols, i, simplify = FALSE))
    }
    # We don't need to add the full model separately as it's already included in the loop above
  }
  
  # Calculate composite scores
  for (i in seq_along(model_combinations)) {
    model_name <- paste0("model_", i)
    formula_expr <- parse_expr(paste(model_combinations[[i]], collapse = "+"))
    normalized_data <- normalized_data %>% 
      mutate(!!sym(model_name) := (!!formula_expr) / length(model_combinations[[i]]))
  }
  
  # Prepare output
  final_data <- normalized_data %>% 
    select(WardName, starts_with("model_"))
  
  list(model_formula = model_combinations, 
       final_data = final_data)
}

# Function to generate model formulas
models_formulas <- function(model_data) {
  model_formulas_data <- data.frame(model = character(), 
                                    variables = character(),
                                    stringsAsFactors = FALSE)
  
  for (index in seq_along(model_data)) {
    model_formula <- data.frame(model = paste0("model_", index), 
                                variables = paste(gsub("normalization_", "", model_data[[index]]), collapse = " + "),
                                stringsAsFactors = FALSE)
    
    model_formulas_data <- rbind(model_formulas_data, model_formula)
  }
  
  return(model_formulas_data)
}


# Function to process model scores
process_model_score <- function(data_to_process){
  plottingdata <- data_to_process %>% 
    reshape2::melt(id.vars = c("WardName")) %>% 
    group_by(variable) %>% 
    mutate(new_value = (value - min(value))/(max(value) - min(value)),
           class = cut(new_value, seq(0, 1, 0.2), include.lowest = T)) %>%
    arrange(value) %>% 
    mutate(rank = 1:n(), 
           wardname_rank = paste(WardName, "(",rank,")"))
  
  plottingdata
}


# Function to plot model score map
plot_model_score_map <- function(shp_data, processed_csv) {
  palette_func <- brewer.pal(5, "YlOrRd")
  
  plot <- ggplot(data = shp_data) +
    geom_sf_interactive(color = "black", fill = "white") +
    geom_sf_interactive(data = processed_csv, 
                        aes(geometry = geometry, fill = class, tooltip = wardname_rank)) +
    facet_wrap(~variable, ncol = 3) +
    scale_fill_discrete(drop=FALSE, name="score", type = palette_func)+
    labs(subtitle='', title='', fill = "score")+
    theme(panel.background = element_blank(), size = 20)+
    theme_void()
  
  girafe(ggobj = plot)
}

box_plot_function <- function(plottingdata, max_wards = 20) {
  library(plotly)
  
  df_long <- plottingdata %>%
    select(WardName, variable, rank)
  
  medians <- df_long %>%
    group_by(WardName) %>%
    summarize(median_value = median(rank)) %>%
    arrange(desc(median_value)) %>%
    .$WardName
  
  # Limit the number of wards shown
  if (length(medians) > max_wards) {
    medians <- medians[1:max_wards]
    df_long <- df_long %>% filter(WardName %in% medians)
  }
  
  df_long$WardName <- factor(df_long$WardName, levels = medians)
  
  # Create the base ggplot
  p <- ggplot(df_long, aes(x = WardName, y = rank)) +
    geom_boxplot(fill = "lightblue", color = "darkblue", alpha = 0.7) +
    coord_flip() +  # Flip coordinates for horizontal layout
    labs(title = "Ward Rankings Distribution", x = "", y = "Rank") +
    scale_y_continuous(limits = c(0, max(df_long$rank)), 
                       breaks = seq(0, max(df_long$rank), by = 5)) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      plot.title = element_text(size = 14, hjust = 0.5),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Convert to plotly for interactivity
  ggplotly(p, tooltip = c("y", "x")) %>%
    layout(hoverlabel = list(bgcolor = "white"),
           plot_bgcolor = "rgba(0,0,0,0)",  # Transparent plot background
           paper_bgcolor = "rgba(0,0,0,0)") %>%  # Transparent paper background
    config(scrollZoom = TRUE, displayModeBar = FALSE) %>%
    style(hoverlabel = list(bgcolor = "white"))
}