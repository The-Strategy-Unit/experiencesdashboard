library(data.validator)
library(assertr)
library(dplyr)
library(mockery)

tidy_trust_data <- readr::read_csv(here::here("tests/test_data.csv"),
  show_col_types = FALSE
) %>%
  tidy_all_trusts()

# read data that mimic the data template Trusts will use to upload their data
phase_2_upload_data <- readRDS(here::here("tests/phase_2_test_template.rds"))

# read data that mimic the database data
phase_2_db_data <- readRDS(here::here("tests/p2_db_data_template.rds")) %>%
  dplyr::mutate(across(c(category, super_category), ~ purrr::map(.x, rawToChar))) %>% 
  dplyr::mutate(sentiment = sample(1:5, nrow(.), replace = T))

unique_data <- phase_2_db_data %>%
  dplyr::distinct(pt_id, .keep_all = TRUE)

single_labeled_filter_data = phase_2_db_data |> 
  head(100) |> 
  get_tidy_filter_data(TRUE)
