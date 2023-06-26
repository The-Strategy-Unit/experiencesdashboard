test_that("Data tidies in all trusts", {
  db_data <- tidy_trust_data %>%
    tidy_all_trusts()

  testthat::expect_gt(nrow(db_data), 0)
})
