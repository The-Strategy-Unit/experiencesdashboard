test_that("Data tidies in all trusts", {
  
  new_data <- tidy_trust_data |>
    mutate(comment_txt = sample(c(unique(comment_txt), "", NA, " ", 'NA', 'NULL', NA, "A"), 
                                nrow(tidy_trust_data)))
  
  db_data <- new_data %>%
    tidy_all_trusts()
  
  text <- unique(db_data$comment_txt)
  expect_true(all(text != 'NA'))
  expect_true(all(text != ''))
  expect_true(all(text != ' '))
  expect_true(all(!is.na(text)))
  
  db_data |> 
    mutate(no_char = nchar(comment_txt)) |>
    filter(no_char <= 1) |>
    nrow() |> expect_equal(0)

  testthat::expect_gt(nrow(db_data), 0)
})
