rm(list = ls())
source("load_path.R")




palettes <- list(rev(RColorBrewer::brewer.pal(4, "RdYlBu")))

# Sample text data (you can replace this with your own text)
text_data <- read.csv("./datafiles/ilorin_cluster_classification.csv") %>% filter(classification !="rural") %>% 
  group_by(classification, Codes) %>% 
  summarise(total = n()) 

deprioritization_data <- read.csv("./datafiles/elite_communities.csv") %>% 
  group_by(classifications, communities) %>% 
  summarise(total = n()) %>% 
  mutate(classifications == "formal ", "formal", classifications)



ggplot(
  text_data,
  aes(
    label = Codes,
    size = total,
    color = classification  # Color by the 'classification' column
  )
) +
  geom_text_wordcloud() +
  scale_color_manual(name ="", values = c("#7f3f98", "#f69283", "#93325a"))+
  scale_size_area() +
  theme_minimal()
ggsave(paste0(projectpath, "/", Sys.Date(), '_word_cloud.pdf'), width=8, height=11)



ggplot(
  deprioritization_data,
  aes(
    label = communities,
    size = total,
    color = classifications  
  )
) +
  geom_text_wordcloud() +
  scale_color_discrete(drop=FALSE, type = palettes)+
  scale_size_area() +
  theme_minimal()
