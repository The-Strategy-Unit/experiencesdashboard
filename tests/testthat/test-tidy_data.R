test_that("Data tidies in all trusts", {
  
  new_data <- tidy_trust_data |>
    mutate(comment_txt = sample(c(unique(comment_txt), "", NA, " ", 'NA', 'NULL', NA), 
                                nrow(tidy_trust_data)))
  
  db_data <- new_data %>%
    tidy_all_trusts()
  
  text <- unique(db_data$comment_txt)
  expect_true(all(text != 'NA'))
  expect_true(all(text != ''))
  expect_true(all(text != ' '))
  expect_true(all(!is.na(text)))

  testthat::expect_gt(nrow(db_data), 0)
})
