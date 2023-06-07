test_that("get_complex_comments works", {
 
  # multilabel_column value must be available in the dataframme
  expect_error(
    get_complex_comments(data = db_data, multilabel_column = "Category")
  )
  
  test1 <- get_complex_comments(data = db_data, multilabel_column = "category")
    
  
  report <- data_validation_report()
  
  test1 %>%
    data.validator::validate(name = "Verifying complex_data") %>%
    validate_if(!is.list(category), description = "category column is a list") %>%
    validate_if(inherits(date, "Date"), description = "date column is in date format") %>%
    add_results(report)
  is_validation_success <- all((get_results(report) %>% dplyr::pull(type)) == "success")
  
  expect_true(is.data.frame(test1))
  expect_true(is_validation_success)
  
})
