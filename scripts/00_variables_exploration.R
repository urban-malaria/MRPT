source("load_path.R", echo=FALSE)


##############################################################################################################################################################
# EVI Analysis (Enhanced Vegetation Index)
###############################################################################################################################################################


files_names  = list.files( file.path(rasterfiles, "EVI/EVI_nigeria_2020"), 
                           pattern = ".tif", full.names = TRUE)


raster_data = lapply(seq_along(files_names), 
                     function(x) raster::raster(files_names[[x]]))

EVI_data = raster_data %>%
  purrr::map(~raster::extract(., ilorin_shapefile,
                              buffer = buffer,
                              fun = mean, df =TRUE)) %>%
  purrr::reduce(left_join, by = c("ID"))

ilorin_shapefile$meanEVI <- rowMeans(EVI_data[ , c(2:13)], na.rm=TRUE)

evi_plottingdata <- ilorin_shapefile %>%
  sf::st_as_sf() %>%
  mutate(class = cut(meanEVI, c(0, 0.05, 0.1, 0.15, 0.2,
                                0.25, 0.3, 0.4), include.lowest = T ))



p1=ggplot() +
  geom_sf(data = evi_plottingdata, aes(geometry = geometry, fill = meanEVI), color = "white") +
  scale_fill_continuous(name="", low = "#F6E0b3", high = "#607c3c") +
  labs(subtitle = '', fill = "", x = NULL, y = NULL) +
  map_theme() 

p2 = ggplot()+
  geom_freqpoly(data = evi_plottingdata, aes(x = meanEVI), binwidth = 0.02, color = "#607c3c", size = 1)+
  theme_manuscript()+
  labs(title = "Mean EVI in 2020")+
  theme(panel.border = element_blank())+
  scale_x_continuous(breaks = seq(0, 1, 0.25))

p3 = ggplot(data = evi_plottingdata, aes(x = meanEVI))+
  stat_ecdf(geom = "step", color = "#607c3c", size = 1)+
  labs(title = "Mean EVI in 2020")+
  theme_manuscript() +
  theme(panel.border = element_blank())


all = ggarrange(p3, p1, heights = c(0.5, 1), ncol = 1)
ggsave(paste0(projectpath, "/", Sys.Date(), '_EVI.pdf'), all, width =5, height =4)

###############################################################################################################################################################
# Settlement Type Analysis
###############################################################################################################################################################

setlement_tpye = sf::st_read(file.path(rasterfiles,"nigeria_settlement_classification","blocks_V1.1", "Nigeria_Blocks_V1.shp")) %>% 
  filter(state == 'Kwara', landuse =='Residential')


settlement_data = sf::st_join(ilorin_shapefile, 
                              setlement_tpye, 
                              join = sf::st_overlaps)


ko_bar_dat = settlement_data

sf::st_geometry(ko_bar_dat) <- NULL

ko_bar_overall = ko_bar_dat %>% 
  dplyr::select(type) %>%  
  group_by(type) %>%  
  summarise(number=n())




#prepare settlement data (convert settlement data to proportion)
settlement_type_all = settlement_data %>% 
  dplyr::select(WardName, settle_type=type)%>% 
  group_by(WardName, settle_type)%>% 
  summarise(number=n())

sf::st_geometry(settlement_type_all) <- NULL 

settlement_types <- settlement_type_all %>% 
  mutate(number = ifelse(is.na(number), 0,  number)) %>% 
  pivot_wider(names_from = settle_type, values_from = number)

plotting = ilorin_shapefile %>% 
  inner_join(settlement_type_all) %>% 
  group_by(WardName) %>% 
  mutate(total_settlement = sum(number),
         proportion_settlement_type = number/total_settlement) %>% 
  ungroup() %>% 
  mutate(class_stmnt_number = cut(number, c(0,2,4,6,8,10,15,20,30,50, 78), include.lowest = T),
         class_stmnt_proportion = cut(proportion_settlement_type, seq(0,1,0.2)))


plotting_v2 = plotting %>% 
  mutate(grp =ifelse(settle_type == "A"|settle_type == "B"|settle_type == "M",  "Poor", "Good")) %>%
  group_by(WardName, grp, total_settlement) %>% 
  summarise(grp_number = sum(number)) %>% 
  mutate(proportion_settlement_type_grp = grp_number/total_settlement,
         grp_proportion = cut(proportion_settlement_type_grp, seq(0,1,0.2), include.lowest = T)) 


plotting_00 <- plotting_v2 %>% 
  filter(grp == "Poor") %>% 
  select(WardName, settlement_type = proportion_settlement_type_grp) %>% 
  st_drop_geometry()


sf::st_crs(plotting) <- sf::st_crs(ilorin_shapefile)
sf::st_crs(plotting_v2) <- sf::st_crs(ilorin_shapefile)

plotting <- sf::st_transform(plotting, 
                             crs = sf::st_crs(ilorin_shapefile))

plotting_v2 <- sf::st_transform(plotting_v2, 
                                crs = sf::st_crs(ilorin_shapefile))


p1=ggplot()+
  geom_sf(data = plotting_v2 %>% filter(grp == "Poor"), 
          aes(geometry = geometry, fill = proportion_settlement_type_grp), color = "white")+
  scale_fill_continuous( name="settlement type", low = "#F6E0b3", high = "#999abe")+
  labs(subtitle='', fill = "", y = "", x = "")+
  map_theme()



p3 = ggplot(data =  plotting_v2 %>% filter(grp == "Poor"), aes(x = proportion_settlement_type_grp))+
  stat_ecdf(geom = "step", color = "#999abe", size = 1)+
  labs(title = "Proportion of poor settlement type", x = NULL)+
  theme_manuscript() +
  theme(panel.border = element_blank())



all = ggarrange(p3, p1, heights = c(0.5, 1), ncol = 1)
ggsave(paste0(projectpath, "/", Sys.Date(), '_settle_poor.pdf'), all, width =5, height =4)

###############################################################################################################################################################
# Distance to Water Bodies Analysis
###############################################################################################################################################################

ilorin_raster_wb <- raster::raster(file.path(rasterfiles,"distance_to_water_bodies", "distance_to_water.tif"))


water_bodies<- raster::extract(ilorin_raster_wb, 
                               ilorin_shapefile, 
                               buffer = buffer,
                               fun = mean, 
                               df =TRUE)

distance <- water_bodies$distance_to_water
distance_water = cbind(ilorin_shapefile, distance) 


p1=ggplot()+
  geom_sf(data = distance_water, aes(geometry = geometry, fill = distance), color = "white")+
  scale_fill_continuous( name="settlement type", low = "#316cb3", high = "#d1eaf6" )+
  labs(subtitle='', title='', x = "", y = "", fill = "")+
  map_theme()

p2 = ggplot()+
  geom_freqpoly(data =  distance_water, aes(x = distance), binwidth = 300, color = "#316cb3", size = 1)+
  theme_manuscript()+
  labs(title = "Distance to water bodies in meters", x = NULL)+
  theme(panel.border = element_blank())


p3 = ggplot(data =  distance_water, aes(x = distance))+
  stat_ecdf(geom = "step", color = "#316cb3", size = 1)+
  labs(title = "Distance to water bodies in meters", x = NULL)+
  theme_manuscript() +
  theme(panel.border = element_blank())


all = ggarrange(p3, p1, heights = c(0.5, 1), ncol = 1)
ggsave(paste0(projectpath, "/", Sys.Date(), '_distance_water.pdf'), all, width =5, height =4)