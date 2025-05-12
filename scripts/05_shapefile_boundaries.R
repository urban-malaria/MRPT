# Load the necessary libraries
source("load_path.R")
library(sf)
library(ggplot2)


#read in the shape files
state_ward <- read.csv(file.path(shapefileDir, "/ShinyApp_shapefiles/urban_wards_extent.csv"))
shapefiles <- file.path(DriveDir, "/data/nigeria/shapefiles/ShinyApp_shapefiles")


cities <- unique(state_ward$City)
wards <- unique(state_ward$Ward)

all_shapefile <- lapply(seq_along(cities), function(x) 
  sf::st_read(paste0(shapefiles, "/", cities[x],"/", cities[x] ,".shp")))


allwards <- lapply(seq_along(all_shapefile), function(x)  all_shapefile[[x]] %>% 
                     filter(WardName  %in% wards)) 

polygon_allwards <- lapply(seq_along(allwards), 
                           function(x)  split(allwards[[x]], allwards[[x]]$WardName)) 


# Convert to an sf object
flattened_polygon_list <- do.call(c, polygon_allwards)

polygon_sf <-lapply(seq_along(flattened_polygon_list), 
                    function(x) sf::st_as_sfc(flattened_polygon_list[[x]], crs = 4326))




subdivide_polygon <- function(polygon, rows, cols) {
  
  # Function to subdivide the polygon into a grid
  
  bounds <- st_bbox(polygon)
  x_range <- seq(bounds["xmin"], bounds["xmax"], length.out = cols + 1)
  y_range <- seq(bounds["ymin"], bounds["ymax"], length.out = rows + 1)
  
  sub_polygons <- list()
  
  for (i in 1:cols) {
    
    for (j in 1:rows) {
      sub_poly <- st_polygon(list(rbind(c(x_range[i], y_range[j]), 
                                        c(x_range[i + 1], y_range[j]), 
                                        c(x_range[i + 1], y_range[j + 1]), 
                                        c(x_range[i], y_range[j + 1]), 
                                        c(x_range[i], y_range[j]))))
      sub_polygons <- append(sub_polygons, list(sub_poly))
    }
  }
  
  st_sfc(sub_polygons, crs = st_crs(polygon))
}



for (index in seq_along(flattened_polygon_list)){ 
  
  
  polygon <- flattened_polygon_list[[index]]
  
  ward <- gsub(" ", "", polygon$WardName)
  
  # Define the number of rows and columns for subdivision
  rows <- 10
  cols <- 10
  
  # Subdivide the polygon
  subdivided_polygons <- subdivide_polygon(polygon_sf[[index]], rows, cols)
  
  # st_sfc(subdivided_polygons, crs = 4)
  # st_sfc(polygon_sf[[index]], crs = 4326)
  
  
  newshape <- st_intersection(polygon_sf[[index]], subdivided_polygons)
  

  
  #save the grids 
  
  # Define the path to the directory
  dir_path <- file.path(shapefileDir, paste0("/ShinyApp_shapefiles/gridded/", ward, "00"))
  
  # Check if the directory exists and create it if it doesn't
  if (!dir.exists(dir_path)) {
    
    dir.create(dir_path, showWarnings = FALSE, recursive = TRUE)
    
    cat("Directory created:", dir_path, "\n")
    
  } else {
    
    cat("Directory already exists:", dir_path, "\n")
  }
  
  
 tryCatch(st_write(newshape, file.path(paste0(dir_path, "/", ward, ".shp"))), error = function(e) {
   cat("An error occurred:\n")
   print(e)})
  
  }



# Plot the original and subdivided polygons using ggplot2
ggplot() +
  # geom_sf(data = st_as_sf(polygon_sf), fill = "red", alpha = 0.5) +
  geom_sf(data = st_as_sf(newshape), fill = "blue", alpha = 0.3) +
  theme_minimal() 


newshape <- st_make_valid(newshape)



# 
