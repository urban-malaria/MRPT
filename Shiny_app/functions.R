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


plot_map <- function(variable_names, shp_data_reactive, raw_dataframe_reactive) {
  plots <- list()
  
  for (variable_name in variable_names) {
    plot <- ggplot(data = shp_data_reactive) +
      geom_sf(color = "black", fill = "white") +
      geom_sf(data = raw_dataframe_reactive,
              aes(geometry = geometry,
                  fill = !!sym(variable_name))
      ) +
      scale_fill_viridis_c(name = "") +
      labs(title = variable_name, subtitle = '', fill = "", x = NULL, y = NULL) +
      map_theme()
    
    plots[[variable_name]] <- plot
  }
  
  combined_plot <- do.call(grid.arrange, c(plots, ncol = 2))
  
  return(combined_plot)
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


# Normalization function
normalize_data <- function(uploaded_data, variable_impacts) {
  numeric_cols <- sapply(uploaded_data, is.numeric)
  
  # Replace NAs with column means for numeric columns
  uploaded_data[numeric_cols] <- lapply(uploaded_data[numeric_cols], function(x) {
    ifelse(is.na(x), mean(x, na.rm = TRUE), x)
  })
  
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
  
  # Ensure WardName is present
  if (!"WardName" %in% names(scoring_data)) {
    stop("WardName column is missing in the data.")
  }
  
  return(scoring_data)
}



# Function to process data for plotting
process_data <- function(data, selected_vars) {
  selected_vars <- unique(c("WardName", selected_vars))
  
  if (length(selected_vars) < 3) {
    stop("Error: Enter at least three variables along with WardName.")
  }
  
  plotting_scoring_data <- data %>%
    select(all_of(c("WardName", selected_vars))) %>%
    reshape2::melt(id.vars = "WardName") %>%
    mutate(class = cut(value, seq(0, 1, length.out = 6), include.lowest = TRUE)) 
  
  plotting_scoring_data
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

# Function to create a box plot of ranks
box_plot_function <- function(plottingdata){
  df_long <- plottingdata %>%
    select(WardName, variable, rank)
  
  medians <- df_long %>%
    group_by(WardName) %>%
    summarize(median_value = median(rank)) %>%
    arrange(median_value) %>%
    .$WardName
  
  df_long$WardName <- factor(df_long$WardName, levels = medians)
  
  ggplot(df_long, aes(x = rank, y = WardName)) +
    geom_boxplot() +
    labs(title = "", x = "Rank", y = "Ward") +
    scale_x_continuous(limits = c(0, 36),
                       breaks = seq(0, 36, 3)) +
    theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 16, color = "black"), 
          axis.text.y = element_text(size = 16, color = "black"),
          axis.title.x = element_text(size = 16),
          axis.title.y = element_text(size =16),
          legend.title=element_text(size=16, colour = 'black'),
          legend.text =element_text(size = 16, colour = 'black')
    )
}