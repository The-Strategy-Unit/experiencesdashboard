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
 

library(tidyverse)










sentiments_ordered <- stringr::str_sort(sentiments_selected)

sentiments_ordered %>% stringr::str_c(collapse = "")


sentiment_txt_data_upset %>%
  mutate(all_sentiments = str_c(all_sentiments))


sentiments_test <- c("anticipation", "positive")
sentiments_test <- c("positive")
sentiments_test <- c("surprise", "fear", "sadness", "disgust", "positive", "trust", "joy", "anticipation", "anger", "negative")
sentiments_test <- c("negative")
sentiments_test <- c("negative", "positive")
length(sentiments_test)

sentiment_txt_data_upset %>% 
  dplyr::select(id, all_sentiments, improve) %>% 
  dplyr::mutate(length = lengths(all_sentiments),
                all_sentimtents_unnest = all_sentiments) %>%
  dplyr::filter(length == length(sentiments_test)) %>%
  tidyr::unnest(cols = all_sentimtents_unnest) %>% 
  dplyr::group_by(id) %>% 
  dplyr::mutate(test_sentiment = dplyr::case_when(all_sentimtents_unnest %in% sentiments_test ~ TRUE),
         sum_temp = sum(test_sentiment)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(is.na(sum_temp) == FALSE) %>% 
  dplyr::select(id, improve, all_sentiments) %>%
  dplyr::distinct() %>% 
  View()


sentiment_txt_data_upset$super %>% unique()



"Communication" = "Communication"
"Staff/Staff Attitude" = "Staff/Staff Attitude"
"Environment/Facilities" = "Environment/Facilities"
"Access to Services" = "Access to Services"
"Care/ Treatment" = "Care/ Treatment"
"Couldn't be improved" = "Couldn't be improved"
"Service Quality/Outcomes" = "Service Quality/Outcomes"
"Involvement" = "Involvement"
"Food" = "Food"
"Privacy and Dignity" = "Privacy and Dignity"     
"MHA" = "MHA"
"Equality/Diversity" = "Equality/Diversity"
"Smoking" = "Smoking"
"Leave" = "Leave"
"Safety" = "Safety"                  
"Physical Health" = "Physical Health"
"Record Keeping" = "Record Keeping"

c("Communication", 
"Staff/Staff Attitude", 
"Environment/Facilities", 
"Access to Services", 
"Care/ Treatment", 
"Couldn't be improved", 
"Service Quality/Outcomes", 
"Involvement", 
"Food", 
"Privacy and Dignity", 
"MHA", 
"Equality/Diversity", 
"Smoking", 
"Leave", 
"Safety", 
"Physical Health", 
"Record Keeping")

sentiments_selected_1 <- c("surprise", "fear", "sadness", "disgust", "positive", "trust", "joy", "anticipation", "anger", "negative")
sentiments_selected_2 <- c("negative")
sentiments_selected_3 <- c("negative", "positive")

sentiments_test <- c("negative", "positive")

str_detect(string = sentiments_selected_1, pattern = sentiments_test)

str_detect(string = sentiments_selected_2, pattern = sentiments_test)

str_detect(string = sentiments_selected_3, pattern = sentiments_test)



sentiments_selected_1 %% sentiments_test

TRUE + TRUE

























