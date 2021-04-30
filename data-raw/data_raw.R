## code to prepare datasets goes here

## Load packages ----
library(tidyverse)
library(nottshc)
library(nottshcMethods)
library(DBI)

## MySQL ----

open_db_data <- get_px_exp(from = "2020-10-01",
                           open_data = FALSE, 
                           remove_demographics = FALSE,
                           remove_optout = TRUE)

trust_a <- open_db_data %>% 
  arrange(Date) %>% 
  collect()

# MUST randomise the demographic features

trust_a <- trust_a %>% 
  mutate(across(Gender : Baby, ~ sample(.x, n())))

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "Maria DB",
                      Server   = Sys.getenv("HOST_NAME"),
                      UID      = Sys.getenv("DB_USER"),
                      PWD      = Sys.getenv("MYSQL_PASSWORD"),
                      Port     = 3306,
                      database = "TEXT_MINING",
                      encoding = "UTF-8")

dbWriteTable(con, 'trust_a', trust_a, overwrite = TRUE)

dbDisconnect(con)

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
