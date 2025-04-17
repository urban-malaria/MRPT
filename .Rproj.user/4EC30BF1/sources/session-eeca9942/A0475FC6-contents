### PRIORITZED AREAS
source("load_path.R", echo=F)
source("pop_estimate_function.R", echo=F)

pathLGA <-  file.path(DriveDir, "/data/nigeria/nigeria_shapefiles/Nigeria LGAs shapefile (260216)")


StateShpDir <- file.path(ShpfilesDir, "all_reprioritization_nmep_states/STATES")

Yobe_shp <- sf::st_read(file.path(StateShpDir ,"Yobe", "Yobe_State.shp"))


Yobe_lGAshp <- sf::st_read(file.path(pathLGA, "NGA_LGAs.shp"))
sf::sf_use_s2(FALSE)


lga_ward <- sf::st_intersection(Yobe_shp, Yobe_lGAshp)


Yobe_variables <- read.csv(file.path(OutputsDir, "Final Extractions", "Yobe_plus.csv")) %>% 
  distinct(WardCode, .keep_all = TRUE) %>%
  dplyr::select(X, WardName, urbanPercentage, WardCode) %>% 
  mutate(
    # classification_20 = ifelse(urbanPercentage > 20, "Urban", "Rural"),
    # classification_30 = ifelse(urbanPercentage > 30, "Urban", "Rural"),
    classification_50 = ifelse(urbanPercentage > 50, "Urban", "Rural"),
    classification_75 = ifelse(urbanPercentage > 75, "Urban", "Rural")
  )  %>% 
  mutate(num = 1:n())


Yobe_ranks <- read.csv(file.path(OutputsDir, "rankings", "Yobe_rankings.csv")) 
Yobe_ranks$WardName <- stringr::str_trim(Yobe_ranks$WardName)
# Yobe_ranks$ranks <- stringr::str_trim(Yobe_ranks$ranks)

Yobe_ranks <- Yobe_ranks %>% 
  mutate(num = 1:n())


ITNDir <- file.path(DataDir, "nigeria/ITN_distribution")

Yobe_itn_data <- readxl::read_excel(
  file.path(ITNDir, "pbi_distribution_Yobe.xlsx"), 
  sheet = 1) 




Yobe_itn_clean <- Yobe_itn_data %>% 
  rename(population = N_FamilyMembers,
         Ward = AdminLevel3) %>% 
  dplyr::select(population, Ward) %>% 
  group_by(Ward) %>% 
  summarise(Population = sum(population, na.rm =T)) %>% 
  ungroup() %>% 
  mutate(num = 1:n())



combined_wards <- left_join(Yobe_variables, Yobe_ranks , by = c("WardCode" = "WardCode"))

combined_wards2 <- left_join(combined_wards, Yobe_itn_clean, by = c("WardName.x" = "Ward")) 



colums <- c("classification_20", "classification_30", "classification_50", "classification_75")


plots <- list()
prioritized_Yobe <- list()

for (ii in seq_along(colums)){
  
  
  prioritized_Yobe[[ii]] <- prioritize_wards(data = combined_wards2, 
                                               population_col = "Population", 
                                               rank_col = "ranks", 
                                               class_col = colums[ii], 
                                               ward_col = "WardName.x", 
                                               target_percentage = 30) 
  
  write.csv(prioritized_Yobe[[ii]], file.path(OutputsDir, "NMEP Presentation Reprioritization Tables", 
                                              "Yobe", paste0("yobe_scenario_", ii, ".csv")))
  
  combined_plot2 <- Yobe_shp %>% 
    left_join(prioritized_Yobe[[ii]], by = c("WardName" = "SelectedWards")) %>% 
    mutate(status = ifelse(is.na(WardPopulation), "Not Reprioritized", "Reprioritized"))
  
  
  plots[[ii]] <- ggplot(data = combined_plot2) +
    geom_sf(aes(geometry = geometry, fill = status)) +
    scale_fill_manual(values = c("Not Reprioritized" = "red", "Reprioritized" = "green"),
                      name = "Status") +
    labs(title = "Reprioritization Scenario 1",
         caption = "Suggested wards are low the lowest-ranking wards under these conditions:
       1. Using composite scores from EVI, u5_tpr, distance to water bodies, settlement type, and flood intensity
       2. Have at least 20% urban area")+
    map_theme()
  
  
  
}


# p_all <- plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]], 
#                    ncol = 2,   # Arrange in 2 columns
#                    align = "hv",  # Align horizontally and vertically
#                    labels = "AUTO") 
# 
# FigDir <- file.path(DriveDir,"/projects/urban_microstratification/Shiny App/Plots/deprioritization_scenarios")
# 
# ggsave(paste0(FigDir,"/", Sys.Date(),"_Yobe_reprioritization_scenarios.pdf"), p_all, width = 13, height = 14)
