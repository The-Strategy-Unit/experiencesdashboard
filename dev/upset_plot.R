# Load packages
library(tidyverse)
library(UpSetR)
library(ggupset)
library(lubridate)
library(ComplexHeatmap)
library(ComplexUpset)

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

sentiments_ordered <- c("positive", "trust", "joy", "anticipation", "surprise", "fear", "sadness", "disgust", "anger", "negative")

sentiments_ordered_sentence <- stringr::str_to_sentence(sentiments_ordered)

sentiment_txt_data_upset %>%
  dplyr::filter(date > "2013-01-01", date < "2018-01-01") %>% 
  tidyr::unnest(cols = all_sentiments) %>% 
  dplyr::mutate(all_sentiments = factor(x = all_sentiments, 
                                        levels = sentiments_ordered,
                                        labels = sentiments_ordered_sentence)) %>% 
  ggplot2::ggplot(ggplot2::aes(date, fill = all_sentiments)) +
  # geom_density(position = "fill") +
  ggplot2::geom_histogram(position = "fill") +
  ggplot2::scale_x_date() +
  ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  ggplot2::scale_fill_viridis_d(direction = -1) +
  ggplot2::labs(x = "Date", y = "Density", fill = "Sentiments")
 