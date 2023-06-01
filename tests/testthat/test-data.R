library(dplyr)
library(testthat)
library(data.validator)
library(assertr)

test_that("Global Databse Data pass all checks", {
  report <- data_validation_report()
  
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
      get_golem_config("trust_name")
    )
  ) %>%
    head() %>% 
    tidy_all_trusts() %>% 
    collect() %>% 
    dplyr::mutate(across(category, ~ purrr::map(.x, jsonlite::fromJSON))) # unserialise the category data from json into list 
  
  # data base data
  db_data %>%
    validate(name = "Verifying filter_data") %>%
    data.validator::validate_if(has_all_names("date", "location_1",
                                              "comment_type", "comment_txt", "fft",
                                              "category"), description = "required columns are present") %>%
    validate_if(is_uniq(comment_id), description = "ID column is unique") %>%
    validate_if(!is.na(comment_id) & comment_id != "", description = "comment_id column is not empty") %>%
    validate_if(!is.na(pt_id) & pt_id != "", description = "pat_id column is not empty") %>%
    validate_if(is.list(category), description = "category column is a list") %>%
    validate_if(inherits(date, 'Date'), description = "date column is in date format") %>%
    validate_if(lubridate::year(min(date)) > 2015, description = "Start Date is after 2015") %>%
    add_results(report)
  
  # the single row per category data 
  db_data %>% 
    tidyr::unnest(category) %>% # Unnest the category column into rows and columns
    dplyr::mutate(super_category = assign_highlevel_categories(category)) %>% 
    validate(name = "Verifying Single row filter_data") %>%
    data.validator::validate_if(has_all_names("date", "location_1",
                                              "comment_type", "comment_txt", "fft",
                                              "category", "super_category"), description = "required columns are present") %>%
    validate_rows(col_concat, is_uniq, comment_id, category, description = "comment_id and category combination is unique") %>%
    validate_if(!is.na(comment_id) & comment_id != "", description = "comment_id column is not empty") %>%
    validate_if(!is.na(pt_id) & pt_id != "", description = "pat_id column is not empty") %>%
    validate_if(is.character(category), description = "category column is a list") %>%
    validate_if(inherits(date, 'Date'), description = "date column is in date format") %>%
    validate_if(lubridate::year(min(date)) > 2015, description = "Start Date is after 2015") %>%
    add_results(report)
  
  # get_results(report) %>% View()
  
  is_validation_success <- all((get_results(report) %>% dplyr::pull(type)) == "success")
  expect_true(is_validation_success)
})