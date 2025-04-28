
#directory path to dropbox
Drive <- Sys.getenv("USERPROFILE")

Drive <- gsub("\\\\", "/", Drive)

DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria") 


dhsDir <- file.path(DriveDir, "data")
dropbox <- file.path(dhsDir, "/nigeria/urban_microstratification/shiny_app_data")

results <- file.path(DriveDir, "projects/Manuscripts/ongoing/ShinyAppDevelopment/images/R") 


# packges to use 
list_of_packages <- c("stringr","ggplot2", "dplyr", "purrr", "haven", "tidyverse", 
                      "readxl", "patchwork", "tidyr", "factoextra", "MASS", "broom", 
                      "glm2", "viridis", "ggwordcloud", "GGally", "reshape2", "wordcloud",
                      "dplyr", "ggplot2", "fmsb", "ROCR", "text2vec", "spacyr", "rvest", 
                      "lubridate", "stringr", "spacyr", "tidytext", "plotly", "widyr", 
                      "igraph", "ggraph","showtext", "ggtext", "kableExtra", "data.table",
                      "tidytext", "wordcloud2", "wordcloud", "reshape2", "GGally", 
                      "syuzhet", "corrplot")



read_install_pacakges <- function(packages = list_of_packages
){
  new_packages <- packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new_packages)
  return(sapply(list_of_packages, require, character.only = TRUE))
}

read_install_pacakges()

min_max = function(x){
  
  return((x -min(x))/(max(x)-min(x)) *5)
}

map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=18, colour = 'black'),
        legend.text =element_text(size = 18, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}


theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size =12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}




get_multiple_sentiments <- function(text) {
  # Function to get sentiment scores from both packages
  # Get sentimentr score
  # Get syuzhet score 
  sentimentr_score <- sentiment(text)$sentiment %>%
    mean()
  
  syuzhet_score <- get_sentiment(text, method="syuzhet") %>% 
    mean()
  
  return(c(sentimentr=sentimentr_score, 
           syuzhet=syuzhet_score))
}