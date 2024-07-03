rm(list=ls())

source("load_path.R", echo=FALSE)

## -----------------------------------------
### Paths and functions 
## -----------------------------------------
library(raster)
# ----------------------------------------------
### Visualizing urban ward and LGA boundaries  
## ---------------------------------------------

## ward shape files
p1=ggplot(ilorin_shapefile) +
  geom_sf(aes(fill = WardName))+
  geom_text_repel(
    data = ilorin_shapefile,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() + 
  labs(title= "Wards in Illorin")+
  xlab("")+
  ylab("")+
  theme(legend.position = "none")
ggsave(paste0(ResultDir, Sys.Date(), '_illorin_metro_colored_by_wards.pdf'), p1, width = 10, height =8)


#LGA shape files
p2=ggplot(ilorin_LGA) +
  geom_sf(aes(fill= LGAName))+
  geom_text_repel(
    data = ilorin_LGA,
    aes(label =  LGAName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme() + 
  labs(title= "LGAs in Illorin")+
  xlab("")+
  ylab("")

#make LGA and ward map
p3 <- ggplot(data =  ilorin_LGA) +
  geom_sf(data=ilorin_shapefile, aes(fill = LGACode))+
  scale_fill_manual(name= "", values= c("#999abe","#e694ae", "#abd7da"))+
  geom_text_repel(
    data = ilorin_shapefile,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() +
  xlab("") +
  ylab("")
ggsave(paste0(ResultDir,"/", Sys.Date(), '_illorin_wards_colored_by_LGA.pdf'), p3, width = 10, height =8)

#Filter down to Ilorin South and West 
ggplot(data =  filter(ilorin_LGA, LGACode == 24007)) +
  geom_sf(data=filter(ilorin_shapefile, LGACode == 24007), fill ="#e694ae" )+
  geom_text_repel(
    data = filter(ilorin_shapefile, LGACode == 24007),
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() +
  xlab("") +
  ylab("")
ggsave(paste0(ResultDir,"/", Sys.Date(), '_illorin_south.pdf'), width = 6, height =4)

ggplot(data =  filter(ilorin_LGA, LGACode == 24008)) +
  geom_sf(data=filter(ilorin_shapefile, LGACode == 24008), fill ="#abd7da")+
  geom_text_repel(
    data = filter(ilorin_shapefile, LGACode == 24008),
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() +
  xlab("") +
  ylab("")
ggsave(paste0(ResultDir,"/", Sys.Date(), '_illorin_west.pdf'), width = 6, height =4)

#urban/rural class
p4=ggplot(ilorin_shapefile) +
  geom_sf(aes(fill = Urban))+
  geom_text_repel(
    data = ilorin_shapefile,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() + 
  labs(title= "Wards in Illorin")+
  xlab("")+
  ylab("")
ggsave(paste0(ResultDir, "/", Sys.Date(), '_illorin_wards_colored_by_ResidenceType.pdf'), p4, width = 10, height =8)


# ---------------------------------------------------------------
### Visualizing settlements  
## --------------------------------------------------------------

#illorin
#settlement type overall 
df_settle = st_read(file.path(rasterfiles, "nigeria_settlement_classification", "blocks_V1.1", "Nigeria_Blocks_V1.shp")) %>% filter(state == 'Kwara')  
# newsf <- st_zm(df_settle , drop=T, what='ZM')
# sf::st_write(newsf, file.path(ProjectDir, "nigeria_settlement_classification", "blocks_V1.1", "Kwara_settlement_class.shp"))
df_settle=st_join(ilorin_shapefile, df_settle, join =st_overlaps)
df_res = df_settle %>%   filter(landuse == 'Residential')
ana_df = df_res %>%  dplyr::select(WardName, settle_type=type, LGACode) %>%  group_by(WardName, settle_type)%>% summarise(number=n())

st_geometry(ana_df) <- NULL 

ana_df = ana_df %>%  pivot_wider(names_from = settle_type, values_from=number)   
ana_df[is.na(ana_df)]= 0
colnames(ana_df) = paste('settlement', colnames(ana_df), sep = '_')
colnames(ana_df)[1] = 'WardName'
ana_df$prop_poor_settlement = (ana_df$settlement_A + ana_df$settlement_B + ana_df$settlement_M)/rowSums(ana_df[, c(2:6)]) 
ana_df$prop_rich_settlement = (ana_df$settlement_D + ana_df$settlement_F)/rowSums(ana_df[, c(2:6)]) 
ana_df =ana_df %>%  dplyr::select(WardName, prop_poor_settlement, prop_rich_settlement)


#map 
pdat = left_join(ilorin_shapefile, ana_df)

p1=ggplot(pdat) +
  geom_sf(aes(fill = prop_rich_settlement))+
  scale_fill_gradient(low = "red", high = "#ebc084", na.value = NA) +
  map_theme() + 
  labs(title= "Wards in Illorin")+
  xlab("")+
  ylab("")
ggsave(paste0(ResultDir, "/", Sys.Date(), '_illorin_wards_colored_by_rich.pdf'), p1, width = 10, height =8)

#agricultural work 
#df_agric = df_settle %>%   filter(landuse == 'Residential')


#----------------------------------------------------------

### Visualizing population density for all wards

##---------------------------------------------------------

raster <- raster(file.path(rasterfiles, "NGA_pop_density","gpw_v4_population_density_rev11_2020_30_sec.tif"))
pop_den <- raster::extract(raster, df, buffer = buffer, fun = mean, df =TRUE)
df_popd = cbind(df, pop_den)


p1=ggplot(df_popd) +
  geom_sf(aes(fill = gpw_v4_population_density_rev11_2020_30_sec))+
  scale_fill_gradient(low = "#eed6f1", high = "#ba5dc8", na.value = NA) +
  map_theme() + 
  labs(title= "Wards in Illorin")+
  xlab("")+
  ylab("")
ggsave(paste0(ResultDir, "/", Sys.Date(), '_illorin_wards_colored_by_density.pdf'), p1, width = 10, height =8)


#----------------------------------------------------------

### Visualizing distance to water bodies for all wards

##---------------------------------------------------------

raster <- raster(file.path(rasterfiles, "distance_to_water_bodies", "distance_to_water.tif"))
dw <- raster::extract(raster, df, buffer = buffer, fun = mean, df =TRUE)
df_w= cbind(df, dw)


p1=ggplot(df_w) +
  geom_sf(aes(fill = distance_to_water))+
  scale_fill_gradient(low = "#f8e9d8", high = "#006fb9", na.value = NA) +
  geom_text_repel(
    data = df,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
  map_theme() + 
  labs(title= "Wards in Illorin")+
  xlab("")+
  ylab("")
ggsave(paste0(ResultDir, "/", Sys.Date(), '_illorin_wards_colored_by_distance_water.pdf'), p1, width = 10, height =8)


##############################################################################################################################################################
#Population sizes from the microplan
#######################################################################################################

pop_sizes <-read_csv(file.path(IlorinDir, "Ilorin_microplan", "Ilorin_ward_pop_microplan.csv")) %>%  
  mutate(Ward = str_to_title(Ward)) %>%
  #filter(Ward == "Agbeyangi/Gbadamu") %>% 
  drop_na(Population_2023) %>% 
  group_by(LGA, Ward) %>%  
  summarise(n_communities = n(), population = sum(as.numeric(Population_2023), na.rm = TRUE))

#link to ward file 
pop_shp <- left_join(ilorin_shapefile, pop_sizes, by =c("WardName" = "Ward"))

ggplot(pop_shp) +
  geom_sf(aes(fill = population))+
  scale_fill_gradient(low = "#ffcde9", high = "#fe0492", na.value = NA) +
  geom_text_repel(
    data = ilorin_shapefile,
    aes(label =  WardName, geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  map_theme() + 
  labs(title= "Ward population sizes (2023 microplan)")+
  xlab("")+
  ylab("")
ggsave(paste0(ResultDir,"/", Sys.Date(), '_ilorin_ward_pop_sizes_microplan.pdf'), width = 6, height =4)

#ilorin plot of wards, communities and population by LGA 

micro_plan_sum <- pop_sizes %>%  group_by(LGA) %>%  summarise(ward =n(), communities = sum(n_communities)/10, pop = sum(population)/10000) %>% 
  pivot_longer(!LGA, names_to = "category", values_to = "count")

micro_plan_sum <- pop_sizes %>%  group_by(LGA) %>%  summarise(ward =n(), communities = sum(n_communities), pop = sum(population))


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

p<-ggplot(micro_plan_sum, aes(x=LGA, y=count)) + 
  geom_point(aes(fill =category), size = 4, alpha = 0.6, shape = 21, stroke =NA)+
  scale_fill_manual(name= "", values= c("#73edf7","#ed7455", "#c862f7"))+
  theme_manuscript()
ggsave(paste0(projectpath,"/", Sys.Date(), '_ilorin_community_population_ward_sizes.pdf'), width = 6, height =4)
