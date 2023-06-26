test_that("spc functions works", {
  result <- split_data_spc(unique_data, variable = "fft", chunks = "monthly")
  expect_true(inherits(result, "data.frame"))

  result2 <- plot_fft_spc(result)
  expect_true(inherits(result2, "ggplot"))
})
