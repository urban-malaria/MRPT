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
library(spdep)

handle_na_neighbor_mean <- function(data, shp_data) {
  # Merge data with shapefile
  spatial_data <- left_join(shp_data, data, by = "WardName")
  
  # Create neighbor list
  w <- spdep::poly2nb(spatial_data, queen = TRUE)
  w_listw <- spdep::nb2listw(w, style = "W")
  
  for (col in names(data)) {
    if (is.numeric(data[[col]]) && any(is.na(data[[col]]))) {
      # Get the column data
      col_data <- spatial_data[[col]]
      
      # Find indices of missing values
      missing_indices <- which(is.na(col_data))
      
      # Impute missing values with the mean of neighboring values
      for (index in missing_indices) {
        neighbor_indices <- w[[index]]
        neighbor_values <- col_data[neighbor_indices]
        imputed_value <- mean(neighbor_values, na.rm = TRUE)
        
        # If all neighbors are NA, use the mean of the entire region
        if (is.na(imputed_value)) {
          imputed_value <- mean(col_data, na.rm = TRUE)
        }
        
        col_data[index] <- imputed_value
      }
      
      # Update the original data
      data[[col]] <- col_data
    }
  }
  
  return(data)
}


# Function to handle NA values using mean of entire region
handle_na_region_mean <- function(data) {
  for (col in names(data)) {
    if (is.numeric(data[[col]]) && any(is.na(data[[col]]))) {
      data[[col]][is.na(data[[col]])] <- mean(data[[col]], na.rm = TRUE)
    }
  }
  return(data)
}

# Function to handle NA values using mode of entire region
handle_na_region_mode <- function(data) {
  get_mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  for (col in names(data)) {
    if (any(is.na(data[[col]]))) {
      if (is.numeric(data[[col]])) {
        data[[col]][is.na(data[[col]])] <- get_mode(data[[col]][!is.na(data[[col]])])
      } else if (is.factor(data[[col]]) || is.character(data[[col]])) {
        data[[col]][is.na(data[[col]])] <- get_mode(data[[col]][!is.na(data[[col]])])
      }
    }
  }
  return(data)
}

# Modified plot_map_00 function to work with both raw and cleaned data
plot_map_00 <- function(variable_name, shp_data_reactive, dataframe_reactive, title) {
  # Ensure the dataframe has a geometry column
  if (!"geometry" %in% names(dataframe_reactive)) {
    dataframe_reactive <- left_join(shp_data_reactive, dataframe_reactive, by = "WardName")
  }
  
  plot <- ggplot() +
    geom_sf_interactive(data = shp_data_reactive, color = "black", fill = "white") +
    geom_sf_interactive(data = dataframe_reactive,
                        aes(fill = !!sym(variable_name),  
                            tooltip = paste(WardName, "(", round(as.numeric(!!sym(variable_name)), 3), ")"))) +
    scale_fill_gradientn(colors = brewer.pal(9, "Blues"), name = "") +
    labs(title = title, subtitle = variable_name, fill = "", x = NULL, y = NULL) +
    theme_minimal() +
    theme(legend.position = "right",
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5, size = 12),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())
  
  girafe(ggobj = plot, width_svg = 10, height_svg = 8)
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
  
  if (length(mismatched_wards) > 0) {
    mismatches <- data.frame(
      CSV_WardNames = mismatched_wards,
      Shapefile_WardNames = character(length(mismatched_wards)),
      stringsAsFactors = FALSE
    )
    
    for (i in seq_along(mismatched_wards)) {
      distances <- stringdist(mismatched_wards[i], shp_wardnames, method = "lv")
      closest_match <- shp_wardnames[which.min(distances)]
      mismatches$Shapefile_WardNames[i] <- closest_match
    }
    
    return(mismatches)
  } else {
    return(NULL)
  }
}


# Normalization function
normalize_data <- function(uploaded_data, variable_impacts) {
  tryCatch({
    print("Input data structure:")
    print(str(uploaded_data))
    print("Variable impacts:")
    print(variable_impacts)
    
    scoring_data <- uploaded_data %>%
      mutate(across(where(is.numeric), 
                    ~{
                      col_name <- cur_column()
                      print(paste("Processing column:", col_name))
                      if (variable_impacts[col_name] == "inverse") {
                        print("Applying inverse transformation")
                        # Use a different inverse transformation
                        inverted <- 1 / (. + 1)  # Adding 1 to avoid division by zero and extreme values
                        normalized <- (inverted - min(inverted, na.rm = TRUE)) / 
                          (max(inverted, na.rm = TRUE) - min(inverted, na.rm = TRUE))
                      } else {
                        print("Applying direct normalization")
                        normalized <- (. - min(., na.rm = TRUE)) / 
                          (max(., na.rm = TRUE) - min(., na.rm = TRUE))
                      }
                      print(paste("Normalization complete for", col_name))
                      normalized
                    },
                    .names = "normalization_{tolower(.col)}"))
    
    print("Normalized data summary:")
    print(summary(scoring_data))
    
    return(scoring_data)
  }, error = function(e) {
    print(paste("Error in normalize_data:", e$message))
    return(NULL)
  })
}


# Function to plot normalized map
plot_normalized_map <- function(shp_data, processed_csv, selected_vars) {
  palette_func <- brewer.pal(5, "YlOrRd")
  
  # Filter the processed_csv to include only the selected variables
  selected_cols <- c("WardName", paste0("normalization_", tolower(selected_vars)))
  filtered_data <- processed_csv %>% 
    select(all_of(selected_cols)) %>%
    pivot_longer(cols = -WardName, names_to = "variable", values_to = "value")
  
  # Join with shapefile data
  combined_data <- left_join(filtered_data, shp_data, by = "WardName")
  
  plot <- ggplot(data = shp_data) +
    geom_sf_interactive(color = "black", fill = "white") + 
    geom_sf_interactive(data = combined_data,
                        aes(geometry = geometry, fill = value, 
                            tooltip = paste(WardName, variable, 
                                            "\nValue:", round(value, 3)))) +
    facet_wrap(~variable, ncol = 2) +
    scale_fill_gradientn(colours = palette_func, name = "Normalized Value") +
    labs(subtitle='', title='Normalized Variables') +
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

box_plot_function <- function(plottingdata) {
  
  df_long <- plottingdata %>%
    select(WardName, variable, rank)
  
  medians <- df_long %>%
    group_by(WardName) %>%
    summarize(median_value = median(rank)) %>%
    arrange(desc(median_value)) %>%
    .$WardName
  
  df_long$WardName <- factor(df_long$WardName, levels = medians)
  
  # Create the base ggplot
  p <- ggplot(df_long, aes(x = WardName, y = rank)) +
    geom_boxplot(fill = "#69b3a2", color = "#3c5e8b", alpha = 0.7) +
    coord_flip() +
    labs(title = "Ward Rankings Distribution", x = "", y = "Rank") +
    scale_y_continuous(limits = c(0, max(df_long$rank)), 
                       breaks = seq(0, max(df_long$rank), by = 5)) +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 8),
      axis.text.x = element_text(size = 10),
      axis.title.x = element_text(size = 12, margin = margin(t = 10)),
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.background = element_rect(fill = "white"),
      plot.margin = margin(10, 10, 10, 10)
    )
  
  # Convert to plotly for interactivity
  ggplotly(p, tooltip = c("y", "x")) %>%
    layout(
      hoverlabel = list(bgcolor = "white"),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = "rgba(0,0,0,0)",
      height = 800  # Increase plot height
    ) %>%
    config(scrollZoom = TRUE, displayModeBar = TRUE) %>%
    style(hoverlabel = list(bgcolor = "white"))
}