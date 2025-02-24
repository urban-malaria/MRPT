source("scripts/load_paths.R")



survey_responses <- read.csv("data/all_survey_data02.csv")


names(survey_responses) <- c("organization", "role", "challenges encountered", 
                             "solutions", "addresses challenges", "application adoption", 
                             "alternatives application", "additional comments")



survey_responses <- survey_responses %>% 
  mutate(participants  = 1:n()) %>% 
  select(participants, everything(.)) %>% 
  mutate(organization = case_when(
    organization == "SMEP M&E" ~ "State Malaria Elimination Program (SMOH)",
    organization == "State Ministry of Health (SMEP)" ~ "State Malaria Elimination Program (SMOH)",
    organization == "Management Sciences for Health" ~ "Implementing Partners",
    organization == "PATH/MACEPA" ~ "Implementing Partners",
    organization == "PMI Evolve Project" ~ "Implementing Partners",
    .default = organization
  ))




survey_responses_long <- reshape2::melt(survey_responses, 
                                        id.vars = c("participants", "organization", "role") )


# text cleaning 
survey_responses_long$Response <- gsub("\n", " ", survey_responses_long$value)
survey_responses_long$Response <-  gsub("\\s+", " ", survey_responses_long$Response )






responses_long_clean <- survey_responses_long %>%  
  mutate(Sentiment_Details = map(Response, ~ sentimentr::sentiment(.x, 
                                                                   amplifier.weight = 1,
                                                                   n.before = 1,
                                                                   n.after = 1, 
                                                                   neutral.nonverb.like = F,
                                                                   missing_value = NULL)),  
         Polarity = map_dbl(Sentiment_Details, ~ mean(.x$sentiment))) %>%  
  mutate(Sentiment = case_when(
    Polarity > 0 ~ "Positive",
    Polarity < 0 ~ "Negative",
    TRUE ~ "Neutral"
  )) 
  

dat2overal <- responses_long_clean %>% 
  group_by(organization, variable) %>% 
  summarise(Polarity = mean(Polarity, na.rm= T)) %>% 
  mutate(Sentiment = case_when(
    Polarity > 0 ~ "Positive",
    Polarity < 0 ~ "Negative",
    TRUE ~ "Neutral"
  )) %>% 
  drop_na(Polarity)

overall_data <- dat2overal %>% 
         group_by(variable) %>% 
         summarise(Polarity  = mean(Polarity), 
                   organization = "overall", 
                   Sentiment = case_when(
                     Polarity > 0 ~ "Positive",
                     Polarity < 0 ~ "Negative",
                     TRUE ~ "Neutral"
                   ))
  
dat2overal00 <- rbind(dat2overal, overall_data)


ggplot(data = dat2overal00, aes(x = variable,
                              y = Polarity, group =  organization, color = organization)) +
  geom_point(size = 5) +
  geom_point(
    data = subset(overall_data),
    aes(x = variable, y = Polarity),
    shape = 21,
    color = "black",
    size = 5,
    stroke = 1.2
  ) +
  geom_hline(aes(yintercept = 0, color = "nuetral point"), linetype = "dashed") +
  labs(
    title = "participant's sentiments per question",
    subtitle = "",
    x = "Question number",
    y = "sentiments polarity",
    color = "Participants"
  ) +
  theme_manuscript()


#Cross package validation 

sentiment_comparison <- responses_long_clean %>%
  # Apply to your data
  group_by(participants, variable) %>%
  summarise(
    text = paste(Response, collapse=" "),
    .groups = 'drop'
  ) %>%
  rowwise() %>%
  mutate(
    scores = list(get_multiple_sentiments(text))
  ) %>%
  unnest_wider(scores)


cor_result <- cor.test(sentiment_comparison$sentimentr, 
                       # Calculate correlation
                       sentiment_comparison$syuzhet)




print(paste("Correlation between methods:", round(cor_result$estimate, 3)))
print(paste("P-value:", round(cor_result$p.value, 3)))

