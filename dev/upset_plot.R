# Load packages
library(tidyverse)
library(UpSetR)
library(ggupset)
library(lubridate)
# Prepare data for UpSetR package
sentiment_txt_data_upset <- sentiment_txt_data %>% 
  mutate(date = lubridate::date(date),
         year = lubridate::year(date),
         month = lubridate::month(date),
         id = 1:nrow(sentiment_txt_data),
         all_sentimtents_unnest = all_sentiments) %>% 
  group_by(year, month) %>% 
  mutate(date = date(floor_date(date))) %>% 
  ungroup() %>% 
  select(id, date, improve, all_sentiments, polarity, all_sentimtents_unnest) %>% 
  unnest(cols = all_sentimtents_unnest) %>% 
  distinct() %>% 
  mutate(value = TRUE) %>% 
  pivot_wider(id_cols = c("id", "date", "improve", "all_sentiments", "polarity"), names_from = all_sentimtents_unnest, values_from = value) %>% 
  janitor::clean_names() %>% 
  select(-na) %>%
  replace_na(list(trust = FALSE, 
                  anticipation = FALSE, 
                  positive = FALSE, 
                  negative = FALSE, 
                  sadness = FALSE, 
                  fear = FALSE, 
                  joy = FALSE,
                  anger = FALSE, 
                  disgust = FALSE, 
                  surprise = FALSE)
             ) %>% 
  mutate_if(is.logical, as.numeric)


sentiment_txt_data_upset %>% 
  mutate(date = lubridate::floor_date(date))


sentiment_txt_data_upset_plot <- sentiment_txt_data_upset %>% 
  select(date, polarity, c("anger", "anticipation", "disgust", "fear", "joy", "negative", "positive", "sadness", "surprise", "trust")) %>% 
  as.data.frame()

upset(data = sentiment_txt_data_upset_plot, 
      nintersects  = 25,
      sets = c("anger", "anticipation", "disgust", "fear", "joy", "negative", "positive", "sadness", "surprise", "trust"),
      # queries = list(list(query = intersects, 
      #                     params = list("positive"), 
      #                     color = "green", 
      #                     active = F)),
      order.by = "freq", text.scale = 1.3
      # empty.intersections = "on",
      # boxplot.summary = c("polarity")
      
      # attribute.plots = list(gridrows = 50, 
      #                        plots = list(list(plot = histogram, 
      #                                          x = "date",
      #                                          queries = F), 
      #                                     list(plot = scatter_plot,
      #                                          x = "date", 
      #                                          y = "polarity",
      #                                          queries = T)), 
      #                        ncols = 1)
      )


