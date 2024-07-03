rm(list=ls())

source("load_path.R", echo=FALSE)

read.csv("./datafiles/scoring_data02.csv")

# colour palettes for the map 
palettes_00 <- list(rev(RColorBrewer::brewer.pal(5, "OrRd")))[[1]][5:1]
palettes <- list(rev(RColorBrewer::brewer.pal(5, "RdYlBu")))

#data files 
tpr  = read.csv("./datafiles/ward_levelsmoothed_tpr_u5.csv") %>% 
  select(c(WardName, tpr_u5_new))

scoring_data02 = read.csv("datafiles/scoring_data02.csv") %>% 
  inner_join(tpr)



# model permutations
model <- c("normal_score_tpr + normal_score_stype", 
           "restructured_ds + normal_score_stype", 
           "normal_score_meanEVI + normal_score_stype", 
           "normal_score_tpr + restructured_ds + normal_score_stype", 
           "normal_score_tpr + normal_score_meanEVI + normal_score_stype", 
           "restructured_ds + normal_score_meanEVI + normal_score_stype", 
           "normal_score_tpr + restructured_ds + normal_score_meanEVI + normal_score_stype", 
           "normal_score_tpr + 0.5 * restructured_ds + normal_score_meanEVI + normal_score_stype", 
           "normal_score_tpr + restructured_ds + 0.5 * normal_score_meanEVI + normal_score_stype", 
           "normal_score_tpr + 0.5 * restructured_ds + 0.5 * normal_score_meanEVI + normal_score_stype")


# variable names 
new_names = c("enhanced vegetation index",
              "settlement type",
              "distance to water bodies", 
              "U5 test positivity rate" )

names(new_names) = c("normal_score_meanEVI",
                     "normal_score_stype",
                     "restructured_ds", 
                     "normal_score_tpr")


# Data normalization, Scoring, and manipulations  

scoring_data <- scoring_data02 %>% 
  mutate(normal_score_meanEVI = (meanEVI - min(meanEVI ))/(max(meanEVI ) - min(meanEVI )),
         normal_score_stype = (settlement_type - min(settlement_type))/
           (max(settlement_type) - min(settlement_type)),
         normal_score_dwater = (min(distance) - distance)/(max(distance) - min(distance)),
         normal_score_tpr = (tpr_u5_new - min(tpr_u5_new))/(max(tpr_u5_new) - min(tpr_u5_new))
  ) %>% 
  mutate(restructured_ds = (normal_score_dwater - min(normal_score_dwater))/(max(normal_score_dwater) - min(normal_score_dwater)))


scoring_data_zet <- scoring_data %>% 
  select(WardName, 
         normal_score_stype,
         normal_score_tpr,
         restructured_ds,
         normal_score_meanEVI) %>% 
  mutate(
    model01 = normal_score_tpr + normal_score_stype,
    model02 = restructured_ds + normal_score_stype,
    model03 = normal_score_meanEVI + normal_score_stype,
    model04 = normal_score_tpr + restructured_ds + normal_score_stype,
    model05 = normal_score_tpr + normal_score_meanEVI + normal_score_stype,
    model06 = restructured_ds + normal_score_meanEVI + normal_score_stype,
    model07 = normal_score_tpr + restructured_ds + normal_score_meanEVI + normal_score_stype,
    model08 = normal_score_tpr + 0.5 * restructured_ds + normal_score_meanEVI + normal_score_stype,
    model09 = normal_score_tpr + restructured_ds + 0.5 * normal_score_meanEVI + normal_score_stype,
    model10 = normal_score_tpr + 0.5 * restructured_ds + 0.5 * normal_score_meanEVI + normal_score_stype,
    model11 = normal_score_tpr + normal_score_meanEVI +  restructured_ds,
    model12 = normal_score_tpr +  restructured_ds,
    model13 = normal_score_tpr + normal_score_meanEVI,
    model14 =  normal_score_meanEVI +  restructured_ds,
  )




# data reshaping 
plotting_scoring_data <- scoring_data_zet %>%
  select(WardName, 
         normal_score_meanEVI, 
         restructured_ds,
         normal_score_stype, 
         normal_score_tpr) %>% 
  reshape::melt(id.vars = c("WardName")) %>% 
  mutate(class = cut(value, seq(0,1, length.out = 6), include.lowest = T)) %>% 
  inner_join(ilorin_shapefile, by = c("WardName"))



plottingdata <- scoring_data_zet %>% 
  select(WardName, model01:model10 ) %>% # 
  #  model01, model02,  model03, model04,  model05, model06, model07, model08, model09, model10
  reshape2::melt(id.vars = c("WardName")) %>% 
  inner_join(ilorin_shapefile, by = c("WardName")) %>% 
  group_by(variable) %>%
  mutate(new_value = (value - min(value))/(max(value) - min(value)),
         class = cut(new_value, seq(0, 1, 0.2), include.lowest = T)) %>%
  arrange(value) %>%
  mutate(rank = 1:n())


#Figure 4 - map plot for normalized variables 
all=ggplot(data = ilorin_shapefile) +
  geom_sf(color = "black", fill = "white") +
  geom_sf(data = plotting_scoring_data, 
          aes(geometry = geometry, fill = class), color = "gray") +
  facet_wrap(~variable, labeller = labeller(variable = new_names)) +  
  scale_fill_discrete(drop=FALSE, name="", type = palettes_00)+
  labs(subtitle = '', fill = "") +
  theme(panel.background = element_blank(), size = 20) +
  theme_void()

ggsave(paste0(projectpath, "/", Sys.Date(), '_normalized_variables.pdf'), all, width =10, height =6)


#Figure 4 - ecdf plot for normalized variables
p3 = ggplot(data = plotting_scoring_data, aes(x = value))+
  stat_ecdf(geom = "step", color = "brown", size = 1)+
  facet_wrap(~variable, labeller = labeller(variable = new_names), scales = "free") +  
  theme_manuscript() +
  theme(panel.border = element_blank())

ggsave(paste0(projectpath, "/", Sys.Date(), '_normalized_variables_ecdf.pdf'), p3, width =10, height =8)


# Ranking maps for all 10 composite score
palettes <- list(RColorBrewer::brewer.pal(5, "YlOrRd"))

p<-ggplot(data = ilorin_shapefile)+
  geom_sf(color = "black", fill = "white")+
  geom_sf(data = plottingdata, aes(geometry = geometry, fill = class))+
  geom_sf_text(data = plottingdata, aes(geometry = geometry, label =  rank), 
               size = 3 )+ 
  facet_wrap(~variable, labeller = label_parsed, ncol = 2) +
  scale_fill_discrete(drop=FALSE, name="rank", type = palettes)+
  # scale_fill_manual(values = palettes)+
  labs(subtitle='', 
       title='',
       fill = "ranking score")+
  theme(panel.background = element_blank(), size = 20)+
  theme_void()

ggsave(paste0(projectpath, "/", Sys.Date(), '_ward_ranking.pdf'), p, width =10, height =11)

# analyse the ranking output and apply the median function row-wise to your data frame  

newdata <- plottingdata %>%
  select(WardName, variable, rank) %>%
  pivot_wider(
    names_from = variable,
    values_from = rank  
  ) %>%
  sf::st_drop_geometry() %>% 
  mutate(means = rowMeans(across(-WardName)))

selected_columns = 2:6
newdata$medians <- apply(newdata[, selected_columns], 1, median)
newdata$modes <- apply(newdata[, selected_columns], 1, calculate_mode)


df_long <- newdata %>%
  select(-c(medians, modes, means)) %>% 
  reshape2::melt(id.vars = c("WardName"))


# Calculate the median values for ordering
medians <- df_long %>%
  group_by(WardName) %>%
  summarize(median_value = median(value)) %>%
  arrange(desc(median_value)) %>%
  #mutate(row_number = 1:n())
  .$WardName

df_long = left_join(df_long, ilorin_shapefile)
df_long$WardName <- factor(df_long$WardName, levels = medians)

# Figure 7 - distribution of ward ranks colored by LGA 
ggplot(df_long, aes(x = value, y = WardName)) +
  geom_boxplot(aes(color = LGA)) +
  scale_color_manual(values =c("#999abe", "#e694ae", "#abd7da"))+
  labs(title = "", x = "Rank", y = "Ward") +
  labs(title = "", x = "Rank", y = "Ward") +
  scale_x_continuous(limits = c(0, 36), 
                     breaks = seq(0, 36, 3)) +
  theme_manuscript() 
ggsave(paste0(projectpath, "/", Sys.Date(), '_overall_ward_ranking_4_models.pdf'), width =10, height =11)



######################################################
# ranks in 10 scoring model vs 4 scoring model 
####################################################

ten_df = read_csv("medians_10_models.csv") %>%  mutate(name = "model10") 
four_df = read_csv("medians_4_models.csv") %>%  mutate(name = "model4")
all = rbind(ten_df, four_df)

data = gather(all, "key", "value",name, -WardName) %>% 
  group_by(WardName) %>% 
  summarise(start = range(row_number)[1], end = range(row_number)[2]) %>% 
  ungroup()


#Figure 8 - median ward ranks in a combination of scoring formulas with and without settlement classification 
p<-ggplot(data, aes(x = start, y = reorder(WardName,-start)))+
  geom_segment(aes(x=start, xend = end, yend =WardName)) +
  geom_point(
    data = gather(all, "key", "value",name, -WardName),
    aes(row_number, WardName, color = value), 
    size = 4, alpha =0.7
  ) +
  scale_color_manual(name= "", values=c("deeppink4", "darkgoldenrod1")) +
  theme_manuscript() +
  labs(y = "", x = "Median ward ranks")

ggsave(paste0(projectpath, "/", Sys.Date(), '_model10_VS_model4.pdf'), width =10, height =11)

