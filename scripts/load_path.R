# This command clears the R workspace, removing all objects from memory to ensure a clean slate.
rm(list=ls())

## This section defines various directory paths used throughout the project, making it easier to reference files stored in these locations.

#directories
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
Drive <- file.path(gsub("[//]", "/", Drive))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
dhsDir <- file.path(DriveDir, "data")
oneDrive <-  file.path(DriveDir,"data","nigeria")
IlorinDir <- file.path(oneDrive, "Ilorin")
rasterfiles <- file.path(oneDrive, "Raster_files")
shapefileDir <- file.path(oneDrive, "shapefiles")
dhsdhs <- file.path(dhsDir, "DHS","Downloads")
projectpath <- file.path(DriveDir, "projects", "Manuscripts", "ongoing", "Ilorin_manuscript", "plots", "R-plots")
result_plots <- file.path(projectpath, "urban_microstratification", 
                          "Kwara-microstratification","ilorin_laurette", "final")
input_drive <-  file.path(DriveDir,"projects/mathematical_model/simulation_output")
presentDir <- file.path(DriveDir, "presentations" )
ifyDir <- file.path(presentDir, "team member archive_Ifeoma/2023/230816_Kano_microstratification/raw_pictures")


# This function checks for the required R packages and installs any that are missing. It then loads all the specified packages into the R environment. 

list_of_packages <- c("RColorBrewer", "readr", "haven", "data.table",
                      "ggplot2", "labelled", "tidyverse", "janitor",
                      "readxl", "mapsf", "survey","srvyr",
                      "broom", "ggthemes", "ggrepel", "sjlabelled",
                      "ggplot2", "dplyr", "ggpubr", "sf", "tidytext",
                      "tidyverse", "wordcloud", "ggwordcloud", "readx1", "bslib", "plotly")


read_install_pacakges <- function(packages = list_of_packages
){
  new_packages <- packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new_packages)
  return(sapply(list_of_packages, require, character.only = TRUE))
}

read_install_pacakges()

# These lines read in shapefiles using the sf package and mutate the data to include a more descriptive LGA (Local Government Area) name based on a code.
ilorin_shapefile <- sf::read_sf(file.path(IlorinDir, "Ilorin_shape_files", "ward", "Ilorin_3LGA.shp"))%>%
  mutate(LGA = ifelse(LGACode == 24006,"Ilorin East",
                      ifelse(LGACode == 24007, "Ilorin South",
                             "Ilorin West")))

ilorin_LGA <- sf::read_sf(file.path(IlorinDir, "Ilorin_shape_files", "LGA", "Illorin_metro_LGA.shp"))

# These functions define custom themes for ggplot2 to ensure consistent styling across plots. map_theme is used for maps, and theme_manuscript is tailored for manuscript-quality plots.
map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=8, colour = 'black'), 
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 22, color = "black"), 
          axis.text.y = element_text(size = 22, color = "black"),
          axis.title.x = element_text(size = 22),
          axis.title.y = element_text(size =22),
          legend.title=element_text(size=22, colour = 'black'),
          legend.text =element_text(size = 22, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}

# This function creates bar plots with customized aesthetics. It takes several parameters to define the data and styling of the plot.
bar_fun = function(df, x, y, fill, scale_fill, 
                   size_x_text, size_y_text, 
                   size_title_x, size_title_y, xlab)
{
  ggplot(df, aes(x=!!x, y =!!y, fill=!!fill))+
    geom_bar(stat = "identity")+
    scale_fill_manual(values = scale_fill)+
    theme_classic()+
    theme(legend.position="none",
          axis.text.x = ggplot2::element_text(size =size_x_text),
          axis.text.y = ggplot2::element_text(size = size_y_text),
          axis.title.x = element_text(size = size_title_x),
          axis.title.y = element_text(size =size_title_y))+
    xlab(xlab)+
    ylab('')
}


# This function calculates the mode (the most frequent value) of a given vector.
calculate_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# This function reads files from a specified directory that match a general and specific pattern. It then applies a given function (fun) to these files.
read.files <- function(path, general_pattern, specific_pattern, fun) {
  files <- list.files(path = path , pattern = general_pattern, full.names = TRUE, recursive = TRUE)
  files<- files[(grep(specific_pattern, files))]
  sapply(files, fun, simplify = F)
}
