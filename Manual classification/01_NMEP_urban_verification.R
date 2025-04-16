#Building urban wards and building density

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

Drive <- gsub("Documents", "", Sys.getenv("HOME"))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria") 

ShpfilesDir <- file.path(DriveDir, "data/nigeria/shapefiles/NMEP Net Distribution States Shapefiles for ADM3 - 13 States")
CitiesshpDir <- file.path(DriveDir, "data/nigeria/shapefiles/ShinyApp_shapefiles")
plots_dir <- file.path(DriveDir, "projects/urban_microstratification/Shiny App/Plots")


BuildingrastersDir <- file.path("C:/Users/USER/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/Raster_files/NGA_buildings_v1_1")
building_density <- raster(file.path(BuildingrastersDir, "NGA_buildings_v1_1_density.tif"))
building_count <- raster(file.path(BuildingrastersDir, "NGA_buildings_v1_1_count.tif"))
building_urban <- raster(file.path(BuildingrastersDir, "NGA_buildings_v1_1_urban.tif"))
lc_urban_wards <- read.csv(file.path(CitiesshpDir, "lc_urban_wards.csv"))
lc_urban_wards_sf <-  st_as_sf(lc_urban_wards, coords = c("Longitude", "Latitude"), crs = 4326)


# plot map of urban extent using shapefile

cities <- c("Abeokuta", "Asaba", "Damaturu", "Dutse", "Gombe", "Ilorin", "Jalingo", "Kano", "Katsina", 
            "Minna", "Osogbo", "Warri", "Zaria")

plots_list <- list()

for (city in cities) {
  
  city_shp <- st_read(file.path(CitiesshpDir, city, paste0(city, ".shp")))
  
  # urban_map <- ggplot()+
  #   geom_sf(data = city_shp, aes(geometry = geometry, fill = Urban))+
  #   labs(title = paste("Urban Extent in", city),
  #        x = "",
  #        y = "",
  #        fill = "Urban")+
  #   map_theme()
  
  
  #labels only urban wards
  
  urban_map2 <- ggplot()+
    geom_sf(data = city_shp, aes(geometry = geometry, fill = Urban))+
    geom_text_repel(data = city_shp %>% 
                      filter(Urban == "Yes"), 
                    aes(geometry = geometry, label = WardName), color = "black", stat = "sf_coordinates",
                    min.segment.length = 0, size = 3.5, force = 1)+
    labs(title = paste("Urban Extent in", city),
         x = "",
         y = "",
         fill = "Urban")+
    map_theme()
  
  
  plots_list[[city]] <- urban_map2
  
}

ppt <- read_pptx()

for (city in names(plots_list)) {
  ppt <- ppt %>% 
    add_slide(layout = "Blank", master = "Office Theme") %>% 
    ph_with(value = plots_list[[city]], location = ph_location_fullsize())
}
print (ppt, target = file.path(plots_dir, "Urban_Extent_plots2.pptx"))



#generate list of urban wards
for (city in cities) {
  
  city_shp <- st_read(file.path(CitiesshpDir, city, paste0(city, ".shp")))
  
  city_urban <- city_shp %>% 
    st_drop_geometry() %>% 
    dplyr::select(StateCode, WardCode, WardName, Urban) %>% 
    filter(Urban == "Yes")
  
  urban_wards <- read.csv(file.path(CitiesshpDir, "urban_wards.csv"))
  urban_wards <- rbind(urban_wards, city_urban)
  write.csv(urban_wards, file.path(CitiesshpDir, "urban_wards.csv"), row.names = FALSE)
  
}

## generate list of all wards
for (city in cities) {
  
  city_shp <- st_read(file.path(CitiesshpDir, city, paste0(city, ".shp")))
  
  city_wards <- city_shp %>% 
    st_drop_geometry() %>% 
    dplyr::select(StateCode, WardCode, WardName, Urban) 
  
  all_wards <- read.csv(file.path(CitiesshpDir, "all_wards.csv"))
  all_wards <- rbind(all_wards, city_wards)
  write.csv(all_wards, file.path(CitiesshpDir, "all_wards.csv"), row.names = FALSE)
  
}

#Urban extent in Warri and Asaba

warri_shp <- st_read(file.path(CitiesshpDir, "Warri", "Warri.shp"))
asaba_shp <- st_read(file.path(CitiesshpDir, "Asaba", "Asaba.shp"))

warri_density <- raster::extract(building_density, warri_shp,
                                 buffer = 0, fun = mean, df = TRUE, sp = TRUE)  
warri_count <- raster::extract(building_count, warri_shp,
                                 buffer = 0, fun = mean, df = TRUE, sp = TRUE)  
warri_urban <- raster::extract(building_urban, warri_shp,
                               buffer = 0,  fun = mean, df = TRUE, sp = TRUE)

ggplot()+
  geom_sf(data = st_as_sf(warri_density), aes(geometry = geometry, fill = NGA_buildings_v1_1_density))+
  # geom_text_repel(data = st_as_sf(warri_density), aes(geometry = geometry, label = NGA_buildings_v1_1_density),
  #                 color = "black", stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  geom_text_repel(data = st_as_sf(warri_density) %>% 
                    filter(!is.na(NGA_buildings_v1_1_density)), 
                  aes(geometry = geometry, label = WardName), color = "black", stat = "sf_coordinates",
                  min.segment.length = 0, size = 3.5, force = 1)+
  scale_fill_continuous(na.value = "white")+
  labs(title = "Building density in Warri",
       x = "",
       y = "",
       fill = "Building density")+
  map_theme()


ggplot()+
  geom_sf(data = st_as_sf(warri_count), aes(geometry = geometry, color = Urban, fill = NGA_buildings_v1_1_count))+
  map_theme()


ggplot()+
  geom_sf(data = st_as_sf(warri_urban), aes(geometry = geometry, color = Urban, fill = NGA_buildings_v1_1_urban))+
  map_theme()

ggplot()+
  geom_sf(data = warri_shp, aes(geometry = geometry, fill = Urban))+
  geom_sf(data = lc_urban_wards_sf, aes(geometry = geometry), size = 0.5)+
  labs(title = "Urban Wards in Warri",
       caption = "Black points represent Urban Wards based on Landcover data: Bowen, Esisi, Avenue") +
  map_theme()


## Asaba
asaba_density <- raster::extract(building_density, asaba_shp,
                                 buffer = 0, fun = mean, df = TRUE, sp = TRUE)  
asaba_count <- raster::extract(building_count, asaba_shp,
                               buffer = 0, fun = mean, df = TRUE, sp = TRUE)  
asaba_urban <- raster::extract(building_urban, asaba_shp,
                               buffer = 0,  fun = mean, df = TRUE, sp = TRUE)

ggplot()+
  geom_sf(data = st_as_sf(asaba_density), aes(geometry = geometry, fill = Urban))+
  geom_text_repel(data = st_as_sf(asaba_density), aes(geometry = geometry, label = ),
                  color = "black", stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(title = "Building density in Asaba",
       x = "",
       y = "")+
  map_theme()



ggplot()+
  geom_sf(data = st_as_sf(asaba_count), aes(geometry = geometry, color = Urban, fill = NGA_buildings_v1_1_count))+
  map_theme()


ggplot()+
  geom_sf(data = st_as_sf(asaba_urban), aes(geometry = geometry, color = Urban, fill = NGA_buildings_v1_1_urban))+
  map_theme()
  

### cross check building density in all cities

cities <- c("Abeokuta", "Damaturu", "Dutse", "Gombe", "Ilorin", "Jalingo", "Kano", "Katsina", 
            "Minna", "Osogbo", "Zaria")
for (city in cities) {
  
  city_shp <- st_read(file.path(CitiesshpDir, city, paste0(city, ".shp")))
  
  city_density <- raster::extract(building_density, city_shp,
                                     buffer = 0, fun = mean, df = TRUE, sp = TRUE)  
  
  map <- ggplot()+
    geom_sf(data = st_as_sf(city_density), aes(geometry = geometry, fill = NGA_buildings_v1_1_density))+
    ggrepel::geom_text_repel(data = st_as_sf(city_density) %>% 
                               filter(!is.na(NGA_buildings_v1_1_density)), 
                             aes(geometry = geometry, label = WardName), color = "black", stat = "sf_coordinates",
                             min.segment.length = 0, size = 3.5, force = 1)+
    scale_fill_continuous(na.value = "white")+
    labs(title = paste("Building density in", city),
         x = "",
         y = "",
         fill = "Building Density")+
    map_theme()
  
  print(map)
   
  # ggsave(filename = file.path(plots_dir, "cities", paste0(city, ".pdf")), plot = map, 
  #         width = 8, height = 6)
}

ilorin_shp <- st_read(file.path(CitiesshpDir, "Ilorin", "Ilorin.shp"))

ilorin_density <- raster::extract(building_density, ilorin_shp,
                                buffer = 0, fun = mean, df = TRUE, sp = TRUE) 
ggplot()+
  geom_sf(data = st_as_sf(ilorin_density), aes(geometry = geometry, fill = NGA_buildings_v1_1_density))+
  ggrepel::geom_text_repel(data = st_as_sf(ilorin_density) %>% 
                             filter(!is.na(NGA_buildings_v1_1_density)), 
                           aes(geometry = geometry, label = WardCode), color = "black", stat = "sf_coordinates",
                           min.segment.length = 1, size = 3.5, force = 1)+
  scale_fill_continuous(na.value = "white")+
  labs(title = paste("Building density in Ilorin"),
       x = "",
       y = "",
       fill = "Building Density")+
  map_theme()

