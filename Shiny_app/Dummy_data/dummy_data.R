### Generate dummy data

rm(list=ls())

library(raster)
library(sf)
library(terra)
library(dplyr)
library(ggplot2)
library(ggrepel)

ShpfilesDir <- file.path("C:/Users/USER/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/shapefiles/NMEP Net Distribution States Shapefiles for ADM3 - 13 States")

kano_shp <- st_read(file.path(ShpfilesDir, "Kano", "Kano_Wards.shp"))
kano_metro_shp <- st_read(file.path("C:/Users/USER/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan/kano_ibadan_shape_files/Kano_metro_ward_sixLGAs/Kano_metro_ward_sixLGAs.shp"))

kano_variables <- read.csv(file.path("C:/Users/USER/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Kano data/combined_data.csv"))


kano_variables[3, "WardName"] <- "Bakin-Ruwa"
kano_variables[7, "WardName"] <- "Don Agunda"
kano_variables[16, "WardName"] <- "Fagg c"
kano_variables[22, "WardName"] <- "Gawunab"
kano_variables[25, "WardName"] <- "Goron"
kano_variables[36, "WardName"] <- "Kavuga"
kano_variables[45, "WardName"] <- "Madigswa"
kano_variables[52, "WardName"] <- "Sharada1"
kano_variables[58, "WardName"] <- "TUNDUNWAZURUCHI" 
kano_variables[65, "WardName"] <- "zaitawai"


write.csv(kano_variables, file.path("C:/Users/USER/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Kano data/dummy_data.csv") )
