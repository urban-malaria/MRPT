rm(list=ls())

source("load_path.R", echo=FALSE)


newdata <- read.csv( "./datafiles/ward_level_unsmoothed_tpr_u5.csv")

inla_data_sf <- inner_join(ilorin_shapefile, newdata, by = c("WardName" = "Wardname"))


new_data_average <- inla_data_sf %>% 
  group_by(WardName) %>% 
  summarise(tpr_u5 = sum(tested_u5_new, na.rm = T)/sum(conf_u5_new, na.rm = T)) %>%  
  mutate(tpr_u5_new = ifelse(tpr_u5 > 1, 1, tpr_u5))


w <- spdep::poly2nb(new_data_average, queen = TRUE)
w_listw <- spdep::nb2listw(w)


# Compute the average test positivity rate from neighboring polygons

mean_neighbors <- weights(w_listw, new_data_average$tpr_u5_new)

missing_indices <- which(is.na(new_data_average$tpr_u5_new))

neighbors_list <- w_listw$neighbours

neighbors_list <- w_listw$neighbours

# Impute missing 'tpr_u5' values with the mean of neighboring values
for (index in seq_along(missing_indices)) {
  polygon <- missing_indices[index]
  neighbor_tprs <- neighbors_list[[polygon]]
  new_data_average$tpr_u5_new[polygon] <- mean(new_data_average$tpr_u5_new[neighbor_tprs], na.rm = TRUE)
}


merge_dataset <- new_data_average %>% 
  mutate(class = cut(tpr_u5_new, seq(0,1,0.2), include.lowest = T))



p1<- ggplot(data =merge_dataset)+
  geom_sf(aes(fill = tpr_u5_new), color = "white")+
  # scale_fill_discrete(name="TPR U5", type = discrete_palettes)+
  scale_fill_continuous(low ="pink" , high = "red", name = "test positivity rate") +
  map_theme()


p2 = ggplot()+
  geom_freqpoly(data =  merge_dataset, aes(x = tpr_u5_new), binwidth = 0.03, color = "red", size = 1)+
  theme_manuscript()+
  labs(title = "U5 Test positivity rate", x = NULL)+
  theme(panel.border = element_blank())

p3 = ggplot(data = merge_dataset, aes(x = tpr_u5_new))+
  stat_ecdf(geom = "step", color = "red", size = 1)+
  labs(title = "U5 Test positivity rate", x = NULL)+
  theme_manuscript() +
  theme(panel.border = element_blank())



all = ggarrange(p3, p1, heights = c(0.5, 1), ncol = 1)
ggsave(paste0(projectpath, "/", Sys.Date(), '_TPR.pdf'), all, width =5, height =4)



