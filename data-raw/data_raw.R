## code to prepare datasets goes here

## Load packages ----
library(tidyverse)
library(nottshc)

## MySQL ----

con_open <- suppressMessages(connect_mysql(open_data = TRUE))

trustData <- get_px_exp(con_open, from = "2020-10-01",
                        open_data = TRUE,
                        return = "tbl_df")

tidy_trust_data <- tidy_px_exp(trustData)

usethis::use_data(tidy_trust_data, overwrite = TRUE)

## Load results from Andreas ----
sentiment_txt_data <- readRDS(file = here::here("data-raw/sentiment_txt_data.rds")) %>% 
  dplyr::rename(division = division2, directorate = directorate2)

# add fake dates

sentiment_txt_data$date <- sample(tidy_trust_data$date, 
                                  nrow(sentiment_txt_data), replace = TRUE)

sentiment_txt_data <- sentiment_txt_data %>% 
  dplyr::mutate(division = dplyr::recode(division, 
                          "Local partnerships- CH" = "General health services",
                          "Forensic services" = "Forensic"))

usethis::use_data(sentiment_txt_data, overwrite = TRUE)



