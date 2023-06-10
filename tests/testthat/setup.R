library(data.validator)
library(assertr)
library(dplyr)
load(here::here("tests/framework.rda"))

test <- readr::read_csv(here::here("tests/test_data.csv"),
  show_col_types = FALSE
)

tidy_trust_data <- test %>%
  tidy_all_trusts()

# data base data
pool <- odbc::dbConnect(
  drv = odbc::odbc(),
  driver = Sys.getenv("odbc_driver"),
  server = Sys.getenv("HOST_NAME"),
  UID = Sys.getenv("DB_USER"),
  PWD = Sys.getenv("MYSQL_PASSWORD"),
  database = "TEXT_MINING",
  Port = 3306
)

# read data that mimic the data base
phase_2_upload_data <- readRDS(here::here("tests/phase_2_test_template.rds"))

phase_2_db_data <- readRDS(here::here("tests/p2_db_data_template.rds")) %>%
  dplyr::mutate(across(c(category, super_category), ~ purrr::map(.x, rawToChar)))

unique_data <- phase_2_db_data %>%
  dplyr::distinct(pt_id, .keep_all = TRUE)
