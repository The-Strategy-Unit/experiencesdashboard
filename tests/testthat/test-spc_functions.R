test_that("spc functions works", {
  result <- split_data_spc(unique_data, variable = "fft", chunks = "monthly")
  expect_true(inherits(result, "data.frame"))

  expect_error(plot_fft_spc(result))
  
  df <- data.frame(
    date = sample(c("2023-01-10", "2023-02-11", "2023-03-12", "2023-04-13",
             "2023-05-14", "2023-06-15", "2023-07-16", "2023-08-17",
             "2023-09-18", "2023-10-19", "2023-11-10", "2023-12-11"), 500, replace = TRUE),
    fft = sample(1:6, 500, replace = TRUE)
    ) %>% 
    mutate(date = as.Date(date))
    
  result2 <- split_data_spc(df, variable = "fft", chunks = "monthly")
  
  expect_equal(nrow(result2), 12)
  pt <- plot_fft_spc(result2)
  
  expect_true(inherits(pt, "ggplot"))
})

test_that("split data spc works", {
  monthly <- split_data_spc(tidy_trust_data,
                            variable = "fft",
                            chunks = "monthly"
  )
  
  equal <- split_data_spc(tidy_trust_data,
                          variable = "fft",
                          chunks = 0
  )
  
  expect_equal(nrow(monthly), 0)
})