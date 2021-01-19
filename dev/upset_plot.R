# Load packages
library(tidyverse)
library(UpSetR)
library(ggupset)
library(lubridate)
library(ComplexHeatmap)

# Prepare data for UpSetR package
# https://jokergoo.github.io/ComplexHeatmap-reference/book/upset-plot.html

sentiment_txt_data_upset <- sentiment_txt_data %>% 
  dplyr::mutate(date = lubridate::date(date),
         year = lubridate::year(date),
         month = lubridate::month(date),
         id = 1:nrow(sentiment_txt_data),
         all_sentimtents_unnest = all_sentiments) %>% 
  dplyr::group_by(year, month) %>% 
  dplyr::mutate(date = lubridate::date(floor_date(date))) %>% 
  dplyr::ungroup() %>% 
  dplyr::select(id, date, super, division2, improve, all_sentiments, polarity, all_sentimtents_unnest) %>% 
  tidyr::unnest(cols = all_sentimtents_unnest) %>% 
  dplyr::distinct() %>% 
  dplyr::mutate(value = TRUE) %>% 
  tidyr::pivot_wider(id_cols = c("id", "date", "super", "division2", "improve", "all_sentiments", "polarity"), 
                     names_from = all_sentimtents_unnest, 
                     values_from = value) %>% 
  janitor::clean_names() %>% 
  dplyr::select(-na) %>%
  tidyr::replace_na(list(trust = FALSE, 
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
  dplyr::mutate_if(is.logical, as.numeric)



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
      order.by = "freq", 
      text.scale = 1.3
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


