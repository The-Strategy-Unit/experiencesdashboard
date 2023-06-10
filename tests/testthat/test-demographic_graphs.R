test_that("demographic graph functions works", {
  result <- unique_data %>%
    demographic_distribution(variable = "age")
  expect_true(inherits(result, "ggplot"))

  result2 <- unique_data %>%
    compare_demographics(variable = "age", questions = list("fft"))
  expect_true(inherits(result2, "plotly"))
})
