library(data.validator)
library(assertr)
library(dplyr)
load(here::here("data/framework.rda"))

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

db_data <- dplyr::tbl(
  pool,
  dbplyr::in_schema(
    "TEXT_MINING",
    "phase_2_demo"
  )
) %>%
  tidy_all_trusts() %>%
  dplyr::collect()
