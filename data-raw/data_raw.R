## code to prepare datasets goes here

## Load packages ----
library(tidyverse)
library(nottshc)
library(nottshcMethods)

## MySQL ----

open_db_data <- get_px_exp(from = "2020-10-01",
                           open_data = FALSE, 
                           remove_demographics = FALSE,
                           remove_optout = TRUE)

trust_a <- tidy_px_exp(open_db_data) %>% 
  arrange(date) %>% 
  collect()

# MUST randomise the demographic features

trust_a <- trust_a %>% 
  mutate(across(gender : baby, ~ sample(.x, n())))

usethis::use_data(trust_a, overwrite = TRUE)

# ## Load results from Andreas ----
# sentiment_txt_data <- readRDS(file = here::here("data-raw/sentiment_txt_data.rds")) %>% 
#   dplyr::rename(division = division2, directorate = directorate2)
# 
# # add fake dates
# 
# sentiment_txt_data$date <- sample(tidy_trust_data$date, 
#                                   nrow(sentiment_txt_data), replace = TRUE)
# 
# sentiment_txt_data <- sentiment_txt_data %>% 
#   dplyr::mutate(division = dplyr::recode(division, 
#                           "Local partnerships- CH" = "General health services",
#                           "Forensic services" = "Forensic"))
# 
# usethis::use_data(sentiment_txt_data, overwrite = TRUE)
