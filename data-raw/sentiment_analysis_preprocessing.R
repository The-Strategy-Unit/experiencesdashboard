library(tidyverse)
library(tidytext)
library(here)

text_blob_scores <- readr::read_csv(here('data-raw/text_blob_scores.csv')) # TextBlob's polarity scores (https://planspace.org/20150607-textblob_sentiment/)
test_data <- readr::read_csv(here('data-raw/y_pred_and_x_test_super.csv'))
index_test_data <- unlist(read.table(here("data-raw/index_test_data_super.txt")))

# Add "Couldn't be improved" class
categoriesTable <- categoriesTable %>% 
  bind_rows(
    data.frame(Category = "General", Super = "Couldn't be improved", 
               Number = 4444, type = "both")
  )

# Choose variables of interest from trustData. 
pipeline_data <- trustData %>%
  mutate_if(is.factor, as.character) %>%
  mutate_if(lubridate::is.Date, as.POSIXct) %>%
  janitor::clean_names() %>%
  left_join(
    select(categoriesTable, Number, Super), 
    by = c('imp1' = 'Number')
  ) %>%
  janitor::clean_names() %>%
  select(super, date, division2, directorate2, improve) %>%
  filter(across(c("super", "improve"), ~ !is.na(.x))) %>%
  as_tibble()

# Put all sentiment types of the NRC dictionary in a character vector for later use.
nrc_sentiments <- tidytext::get_sentiments("nrc") %>%
  dplyr::select(sentiment) %>%
  dplyr::distinct() %>%
  dplyr::pull() %>%
  sort()

# Calculate the number of times each NRC sentiment type occurs in a given feedback text.
net_sentiment_nrc <- pipeline_data %>%
  dplyr::mutate(linenumber = dplyr::row_number()) %>%
  tidytext::unnest_tokens(word, improve) %>%
  dplyr::left_join(tidytext::get_sentiments("nrc"), by = "word") %>% # We want a left join so as not to lose comments with no sentiment
  dplyr::count(linenumber, sentiment, name = 'sentiment_count') %>%
  dplyr::mutate(sentiment_count = dplyr::case_when(
    is.na(sentiment) ~ NA_integer_,
    TRUE ~ sentiment_count
  )) %>%
  dplyr::select(linenumber, sentiment, sentiment_count) %>%
  tidyr::pivot_wider(names_from = sentiment, 
                     values_from = sentiment_count, 
                     values_fill = 0,
                     names_sort = TRUE
  ) %>%
  dplyr::full_join(
    pipeline_data %>%
      dplyr::mutate(linenumber = dplyr::row_number()),
    by = "linenumber"
  ) %>%
  dplyr::select(improve, everything(), -`NA`) %>%
  dplyr::mutate(all_sentiments =  
                  dplyr::select(., dplyr::all_of(nrc_sentiments)) %>%
                  split(seq(nrow(.))) %>%
                  lapply(function(x) unlist(names(x)[x != 0]))
  ) %>%
  dplyr::select(improve, all_sentiments, everything())

# Bring together all data.
pipeline_data <- pipeline_data %>%
  bind_cols(
    net_sentiment_nrc %>%
      select(-improve, -linenumber, -super, -date, -division2, -directorate2)
  ) %>%
  bind_cols(
    text_blob_scores %>%
      select(polarity)
  )

pipeline_data$super_predicted <- NA
pipeline_data$super_predicted[index_test_data] <- test_data$pred


# Check out data
pipeline_data %>% 
  head() %>% 
  View()

#looks good, save!
saveRDS(pipeline_data, file = here("data-raw/sentiment_txt_data.rds"))

library(tidyverse)

sentiment_txt_data %>% 
  mutate(date = lubridate::date(date))
