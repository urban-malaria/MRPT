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
library(spdep)
library(stringdist)
library(DiagrammeR)
library(glue)


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
    df <- df %>% rename_with(~ gsub("\\.", " ", pattern), all_of(intersect(names(df), pattern_list[[pattern]])))
  }
  df
}


# Function to filter the columns
filter_columns <- function(data) {
  ward_name_index <- which(names(data) == "WardName")
  
  if (length(ward_name_index) == 0) {
    warning("WardName column not found. Using all columns.")
    return(data)
  }
  
  selected_columns <- c("WardName", names(data)[(ward_name_index + 1):ncol(data)])
  return(data[, selected_columns])
}


get_columns_after_wardname <- function(data, specific_columns = NULL) {
  ward_name_index <- which(names(data) == "WardName")
  if (length(ward_name_index) == 0) {
    warning("WardName column not found. Returning all columns.")
    return(names(data))
  }
  
  columns_after_wardname <- names(data)[(ward_name_index + 1):ncol(data)]
  
  if (!is.null(specific_columns)) {
    columns_after_wardname <- intersect(columns_after_wardname, specific_columns)
  }
  
  return(columns_after_wardname)
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



# Function to handle NA values using spatial weights
handle_na_neighbor_mean <- function(data, shp_data, col = NULL) {
  if (is.null(col)) {
    cols_to_process <- names(data)[sapply(data, function(x) any(is.na(x)))]
  } else {
    cols_to_process <- col
  }
  
  # Create neighbor structure using shapefile data
  w <- spdep::poly2nb(shp_data, queen = TRUE)
  
  for (current_col in cols_to_process) {
    col_data <- data[[current_col]]
    missing_indices <- which(is.na(col_data))
    
    print(paste("Processing column:", current_col))
    print(paste("Number of NAs:", length(missing_indices)))
    
    for (index in missing_indices) {
      neighbor_indices <- w[[index]]
      neighbor_values <- col_data[neighbor_indices]
      imputed_value <- mean(neighbor_values, na.rm = TRUE)
      
      if (is.na(imputed_value)) {
        imputed_value <- mean(col_data, na.rm = TRUE)
      }
      
      col_data[index] <- imputed_value
      print(paste("Imputed value for index", index, ":", imputed_value))
    }
    
    # Only update the NAs in the original data
    data[[current_col]][missing_indices] <- col_data[missing_indices]
  }
  
  return(data)
}

# Function to handle NA values using mean of entire region
handle_na_region_mean <- function(data, col = NULL) {
  if (is.null(col)) {
    cols_to_process <- names(data)[sapply(data, function(x) any(is.na(x)))]
  } else {
    cols_to_process <- col
  }
  
  for (current_col in cols_to_process) {
    data[[current_col]][is.na(data[[current_col]])] <- mean(data[[current_col]], na.rm = TRUE)
  }
  
  return(data)
}

# Function to handle NA values using mode of entire region
handle_na_region_mode <- function(data, col = NULL) {
  get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  if (is.null(col)) {
    cols_to_process <- names(data)[sapply(data, function(x) any(is.na(x)))]
  } else {
    cols_to_process <- col
  }
  
  for (current_col in cols_to_process) {
    data[[current_col]][is.na(data[[current_col]])] <- get_mode(data[[current_col]][!is.na(data[[current_col]])])
  }
  
  return(data)
}




# Modified plot_map_00 function to work with both raw and cleaned data
plot_map_00 <- function(variable_name, 
                        shp_data_reactive, 
                        dataframe_reactive, 
                        title, 
                        na_handling_method = NULL) {
  # Ensure the dataframe has a geometry column
  if (!"geometry" %in% names(dataframe_reactive)) {
    dataframe_reactive <- left_join(shp_data_reactive, dataframe_reactive, by = "WardName")
  }
  
  na_method_text <- if (!is.null(na_handling_method)) {
    paste("\nNA Handling: ", na_handling_method)
  } else {
    ""
  }
  
  plot <- ggplot() +
    geom_sf_interactive(data = shp_data_reactive, color = "black", fill = "white") +
    geom_sf_interactive(data = dataframe_reactive,
                        aes(fill = !!sym(variable_name),  
                            tooltip = paste(WardName, "(", round(as.numeric(!!sym(variable_name)), 3), ")"))) +
    scale_fill_gradientn(colors = brewer.pal(9, "Blues"), name = "") +
    labs(title = paste(title, na_method_text), subtitle = variable_name, fill = "", x = NULL, y = NULL) +
    theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 14),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
  
  girafe(ggobj = plot, width_svg = 10, height_svg = 8)
}




# Function to check for missing values:

check_missing_values <- function(data) {
  missing_cols <- sapply(data, function(x) any(is.na(x)))
  cols_with_missing <- names(missing_cols[missing_cols])
  return(list(columns = cols_with_missing, data = data))
}

# Function to check for wardname mismatches:
check_wardname_mismatches <- function(csv_data, shp_data) {
  csv_wardnames <- csv_data$WardName
  shp_wardnames <- shp_data$WardName
  
  mismatched_wards <- setdiff(csv_wardnames, shp_wardnames)
  
  if (length(mismatched_wards) > 0) {
    mismatches <- data.frame(
      CSV_WardName = mismatched_wards,
      Shapefile_Options = I(replicate(length(mismatched_wards), list(shp_wardnames))),
      stringsAsFactors = FALSE
    )
    return(mismatches)
  } else {
    return(NULL)
  }
}


# Normalization function
# Modify normalize_data 
normalize_data <- function(cleaned_data, variable_relationships) { 
  tryCatch({
    print("Input data structure (cleaned data):")
    print(str(cleaned_data))
    print("Variable relationships:")
    print(variable_relationships) 
    
    # Identify numeric columns for normalization
    numeric_cols <- names(cleaned_data)[sapply(cleaned_data, is.numeric)]
    numeric_cols <- intersect(numeric_cols, names(variable_relationships))
    
    print("Numeric columns to be normalized:")
    print(numeric_cols)
    
    if (length(numeric_cols) == 0) {
      stop("No numeric columns found for normalization!")
    }
    
    # Apply normalization to numeric columns based on relationships
    scoring_data <- cleaned_data %>% 
      mutate(across(all_of(numeric_cols), 
                    ~{
                      col_name <- cur_column()
                      if (variable_relationships[col_name] == "inverse") {
                        inverted <- 1 / (. + 1) 
                        (inverted - min(inverted, na.rm = TRUE)) / 
                          (max(inverted, na.rm = TRUE) - min(inverted, na.rm = TRUE))
                      } else { 
                        (. - min(., na.rm = TRUE)) / 
                          (max(., na.rm = TRUE) - min(., na.rm = TRUE)) 
                      }
                    },
                    .names = "normalization_{tolower(.col)}"))
    
    print("Normalized data summary:")
    print(summary(scoring_data))
    
    return(scoring_data)
    
  }, error = function(e) {
    print(paste("Error in normalize_data:", e$message))
    print(traceback()) 
    return(NULL)
  })
} 



# Function to plot normalized map
plot_normalized_map <- function(shp_data, processed_csv, selected_vars) {
  palette_func <- brewer.pal(5, "YlOrRd")
  
  selected_cols <- c("WardName", selected_vars)
  filtered_data <- processed_csv %>%  
    select(all_of(selected_cols)) %>% 
    pivot_longer(cols = -WardName, names_to = "variable", values_to = "value") 
  
  combined_data <- left_join(filtered_data, shp_data, by = "WardName") 
  
  plot <- ggplot(data = shp_data) +
    geom_sf_interactive(color = "black", fill = "white") + 
    geom_sf_interactive(data = combined_data,
                        aes(geometry = geometry, fill = value, 
                            tooltip = paste(WardName, variable, 
                                            "\nValue:", round(value, 3)))) +
    scale_fill_gradientn(colours = palette_func, name = "Normalized Value") +
    labs(title = paste('Normalized Variable:', gsub("normalization_", "", selected_vars))) +
    theme_void() +
    theme(panel.background = element_blank(),
          plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          legend.position = "right",
          legend.key.size = unit(0.8, 'cm'),
          legend.text = element_text(size = 10),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.line = element_blank())  # This line removes the axis lines
  
  girafe(ggobj = plot, width_svg = 10, height_svg = 8)
}


# Function to calculate composite scores for different models

composite_score_models <- function(normalized_data, selected_vars) {
  print("Entering composite_score_models function")
  print("Normalized data structure:")
  print(str(normalized_data))
  print("Selected variables:")
  print(selected_vars)
  
  # Get normalized column names for selected variables only
  norm_cols <- paste0("normalization_", tolower(selected_vars))
  norm_cols <- intersect(norm_cols, names(normalized_data))
  
  print("Normalized columns to be used:")
  print(norm_cols)
  
  if (length(norm_cols) < 2) {
    print("Error: At least two valid variables are required for composite score calculation.")
    return(NULL)
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
  }
  
  # Calculate composite scores
  final_data <- normalized_data %>% select(WardName)
  
  for (i in seq_along(model_combinations)) {
    model_name <- paste0("model_", i)
    vars <- model_combinations[[i]]
    
    print(paste("Processing", model_name))
    print("Variables used:")
    print(vars)
    
    tryCatch({
      final_data <- final_data %>% 
        mutate(!!sym(model_name) := {
          result <- rowSums(select(normalized_data, all_of(vars))) / length(vars)
          
          print("Result summary:")
          print(summary(result))
          
          if (model_name == "model_4") {
            print("Detailed result for model_4:")
            print(head(result, 10))
          }
          
          result
        })
    }, error = function(e) {
      print(paste("Error in composite_score_models for", model_name, ":", e$message))
      print("Data causing the error:")
      print(str(normalized_data))
      print("Variables causing the error:")
      print(vars)
    })
  }
  
  # Prepare output
  if (ncol(final_data) <= 1) {
    print("Error: No valid models could be created.")
    return(NULL)
  }
  
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
    mutate(
      new_value = (value - min(value)) / (max(value) - min(value)),
      class = cut(new_value, seq(0, 1, 0.2), include.lowest = TRUE)
    ) %>%
    arrange(value) %>% 
    mutate(
      rank = row_number(),
      wardname_rank = paste(WardName, "(",rank,")")
    )
  
  # Add debugging output
  print("Plotting data summary:")
  print(summary(plottingdata))
  
  plottingdata
}


# Function to plot model score map
plot_model_score_map <- function(shp_data, processed_csv, model_formulas, maps_per_page = 4) {
  palette_func <- brewer.pal(5, "YlOrRd")
  
  # Create a named vector for facet labels
  facet_labels <- setNames(
    paste(gsub("_", " ", model_formulas$model), "\n", model_formulas$variables),
    model_formulas$model
  )
  
  # Split the data into pages
  total_models <- nrow(model_formulas)
  pages <- ceiling(total_models / maps_per_page)
  
  plot_list <- list()
  
  for (page in 1:pages) {
    start_index <- (page - 1) * maps_per_page + 1
    end_index <- min(page * maps_per_page, total_models)
    
    current_models <- model_formulas$model[start_index:end_index]
    current_data <- processed_csv %>% filter(variable %in% current_models)
    
    plot <- ggplot(data = shp_data) +
      geom_sf_interactive(color = "black", fill = "white") +
      geom_sf_interactive(data = current_data, 
                          aes(geometry = geometry, fill = class, tooltip = wardname_rank)) +
      facet_wrap(~variable, ncol = 2, labeller = labeller(variable = facet_labels)) +
      scale_fill_discrete(drop=FALSE, name="Malaria Risk Score", type = palette_func,
                          labels = c("Very Low", "Low", "Medium", "High", "Very High")) +
      labs(subtitle=paste("Page", page, "of", pages), 
           title='Composite Score Distribution by Model', 
           fill = "Malaria Risk Score") +
      theme_void() +
      theme(
        strip.text = element_text(size = 14, face = "bold", lineheight = 0.9),
        strip.background = element_blank(),  # This removes the box around the heading
        legend.position = "bottom",
        legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5)
      )
    
    plot_list[[page]] <- plot
  }
  
  # Return a list of girafe objects
  return(lapply(plot_list, function(p) girafe(ggobj = p, width_svg = 12, height_svg = 10)))
}



#Function for boxplots
box_plot_function <- function(plottingdata) {
  df_long <- plottingdata %>%
    select(WardName, variable, rank)
  
  ward_rankings <- df_long %>%
    group_by(WardName) %>%
    summarize(median_rank = median(rank)) %>%
    arrange(median_rank) %>%
    mutate(overall_rank = row_number())
  
  df_long <- df_long %>%
    left_join(ward_rankings, by = "WardName")
  
  df_long$WardName <- factor(df_long$WardName, levels = ward_rankings$WardName)
  
  p <- ggplot(df_long, aes(x = WardName, y = rank)) +
    geom_boxplot(fill = "#69b3a2", color = "#3c5e8b", alpha = 0.7) +
    coord_flip() +
    labs(title = "Ward Rankings Distribution", x = "", y = "Rank") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
    )
  
  plot <- ggplotly(p, height = 750) %>%
    layout(
      yaxis = list(fixedrange = FALSE),
      xaxis = list(fixedrange = TRUE)
    ) %>%
    config(scrollZoom = TRUE)
  
  return(list(plot = plot, ward_rankings = ward_rankings))
}