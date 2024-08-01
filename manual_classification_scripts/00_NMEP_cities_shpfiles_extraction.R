##NMEP Cities exploration

rm(list=ls())

library(sf)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(grid)
library(patchwork)
library(readxl)
library(openxlsx)
library(officer)
library(magrittr)
library(raster)

map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
        plot.title = element_text(hjust = 0.5),
        legend.title=element_text(size=8, colour = 'black', hjust = 0.5), 
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

#file paths

Drive <- gsub("Documents", "", Sys.getenv("HOME"))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria") 

ShpfilesDir <- file.path(DriveDir, "data/nigeria/shapefiles/NMEP Net Distribution States Shapefiles for ADM3 - 13 States")
CitiesshpDir <- file.path(DriveDir, "data/nigeria/shapefiles/ShinyApp_shapefiles")
plots_dir <- file.path(DriveDir, "projects/urban_microstratification/Shiny App/Plots")


nigeria_shp<- st_read(file.path(ShpfilesDir, "Nigeria", "Nigeria_Wards.shp"))

#generate individual city shape files from nigeria_shp
abeokuta_shp <- nigeria_shp %>% 
  filter(LGACode == "28001" | LGACode == "28002") %>%  #Abeokuta-North, Abeokuta-South
  mutate(LGAName = ifelse(LGACode == "28001", "Abeokuta-North",
                          ifelse(LGACode == "28002", "Abeokuta-South", NA)))
st_write(abeokuta_shp, file.path(CitiesshpDir, "Abeokuta", "Abeokuta.shp" ))

katsina_shp <- nigeria_shp %>% 
  filter(LGACode == "421" | LGACode == "402" | LGACode == "426" |LGACode == "427" | LGACode == "430") %>% #Katsina, Batagarawa, Mani, Mashi, Rimi
  mutate(LGAName = ifelse(LGACode == "421", "Katsina",
                          ifelse(LGACode == "402", "Batagawara",
                                 ifelse(LGACode == "426", "Mani",
                                        ifelse(LGACode == "427", "Mashi",
                                               ifelse(LGACode == "430", "Rimi", NA))))))
st_write(katsina_shp, file.path(CitiesshpDir, "Katsina", "Katsina.shp" ))

dutse_shp <- nigeria_shp %>% 
  filter(LGACode == "18012") %>% #Dutse
  mutate(LGAName = "Dutse")
st_write(dutse_shp, file.path(CitiesshpDir, "Dutse", "Dutse.shp" ))


damaturu_shp <- nigeria_shp %>% 
  filter(LGACode == "703") %>% 
  mutate(LGAName = "Damaturu")
st_write(damaturu_shp, file.path(CitiesshpDir, "Damaturu", "Damaturu.shp" ))


gombe_shp <- nigeria_shp %>% 
  filter(LGACode == "16006") %>% 
  mutate(LGAName = "Gombe")
st_write(gombe_shp, file.path(CitiesshpDir, "Gombe", "Gombe.shp" ))

jalingo_shp <- nigeria_shp %>% 
  filter(LGACode == "35007") %>% 
  mutate(LGAName = "Jalingo")
st_write(jalingo_shp, file.path(CitiesshpDir, "Jalingo", "Jalingo.shp" ))


asaba_shp <- nigeria_shp %>% 
  filter(LGACode == "10015") %>% 
  mutate(LGAName = "Oshimili-South")
st_write(asaba_shp, file.path(CitiesshpDir, "Asaba", "Asaba.shp" ))


# adamawa_city nigeria_shp %>% 
#   filter(LGACode == "" |)
# st_write(adamawa_city, file.path(CitiesshpDir, "", "" ))
# 

#Plot wards in each city
cities <- c("Abeokuta", "Asaba", "Damaturu", "Dutse", "Gombe", "Ilorin", "Jalingo", "Kano", "Katsina", 
            "Minna", "Osogbo", "Warri", "Zaria")

for (city in cities) {
  
  city_shp <- st_read(file.path(CitiesshpDir, city, paste0(city, ".shp")))
  
  city_map <- ggplot()+
    geom_sf(data = city_shp, aes(geometry = geometry, fill = LGAName))+
    ggrepel::geom_text_repel(data = city_shp, aes(label = WardName, geometry = geometry),
                             color = "black", stat = "sf_coordinates",
                             min.segment.length = 0, size = 4, force = 1)+
    labs(title = paste("Wards in", city),
         x = "",
         y = "",
         fill = "LGA")+
    map_theme()
  
ggsave(filename = file.path(plots_dir, "cities", paste0(city, ".pdf")), plot = city_map, 
       width = 8, height = 6)
}


