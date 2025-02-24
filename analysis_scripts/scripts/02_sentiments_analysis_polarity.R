source("scripts/load_paths.R")


responses <- read.csv( "data/openeded.csv")



data <- tibble(Response = responses$openended)


data <- data %>%
  # Sentiment analysis using TextBlob lexicon in R
  # Get detailed sentiment data
  # Average polarity for each response
  mutate(Sentiment_Details = map(Response, ~ sentimentr::sentiment(.x)), 
         Polarity = map_dbl(Sentiment_Details, ~ mean(.x$sentiment))) %>%  
  mutate(Sentiment = case_when(
    Polarity > 0 ~ "Positive",
    Polarity < 0 ~ "Negative",
    TRUE ~ "Neutral"
  ))


ggplot(data, aes(x = Sentiment, fill = Sentiment)) +
  # Sentiment distribution plot
  geom_bar() +
  labs(title = "Sentiment Distribution of Survey Responses",
       x = "Sentiment", y = "Count") +
  theme_manuscript()


wordcloud_data <- data %>%
  # Generate word cloud
  # Remove stopwords
  unnest_tokens(word, Response) %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% stop_words$word) 

wordcloud2(wordcloud_data, size = 0.8)


data %>%
  # Average polarity by sentiment plot
  group_by(Sentiment) %>%
  summarize(Average_Polarity = mean(Polarity)) %>%
  filter(Sentiment  != "Neutral") %>% 
  ggplot(aes(x = Sentiment, y = Average_Polarity, fill = Sentiment)) +
  geom_col() +
  labs(title = "Average Polarity by Sentiment",
       x = "Sentiment", y = "Average Polarity") +
  theme_manuscript()





