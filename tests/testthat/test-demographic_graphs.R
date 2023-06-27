test_that("demographic_distribution functions works:  return plotly object by default", {
  result <- unique_data %>%
    demographic_distribution(variable = "age")
  expect_true(inherits(result, "plotly"))
})

test_that("demographic_distribution functions works: return ggplot object", {
  result <- unique_data %>%
    demographic_distribution(variable = "age", return_ggplot = TRUE)
  expect_true(inherits(result, "ggplot"))
})

test_that("compare_demographics functions works", {
  result2 <- unique_data %>%
    compare_demographics(variable = "age", score_column = list("fft"))
  expect_true(inherits(result2, "plotly"))
  expect_snapshot(result2)
})
