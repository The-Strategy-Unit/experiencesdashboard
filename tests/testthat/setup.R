library(data.validator)
library(assertr)
library(dplyr)


tidy_trust_data <- readr::read_csv(here::here("tests/test_data.csv"),
                             show_col_types = FALSE
  ) %>%
  tidy_all_trusts()

# read data that mimic the data template Trusts will use to upload their data
phase_2_upload_data <- readRDS(here::here("tests/phase_2_test_template.rds"))

# read data that mimic the database data
phase_2_db_data <- readRDS(here::here("tests/p2_db_data_template.rds")) %>%
  dplyr::mutate(across(c(category, super_category), ~ purrr::map(.x, rawToChar)))

unique_data <- phase_2_db_data %>%
  dplyr::distinct(pt_id, .keep_all = TRUE)
