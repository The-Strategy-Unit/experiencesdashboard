test_that("Calculate table works", {
  
  test_table_imp <- calculate_table(table_data = tidy_trust_data, 
                                    count_column = "category", 
                                    comment_type = "imp", 
                                    click_column = NULL)
  
  expect_gt(nrow(test_table_imp), 0)
  
  test_table_best <- calculate_table(table_data = tidy_trust_data, 
                                     count_column = "category", 
                                     comment_type = "best", 
                                     click_column = NULL)
  
  expect_gt(nrow(test_table_best), 0)
  
  expect_false(isTRUE(all.equal(test_table_best, test_table_imp)))
})
