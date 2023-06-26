test_that("demographic_distribution functions works", {
  result <- unique_data %>%
    demographic_distribution(variable = "age")
  expect_true(inherits(result, "plotly"))
})

test_that("compare_demographics functions works", {

  result2 <- unique_data %>%
    compare_demographics(variable = "age", score_column = list("fft"))
  expect_true(inherits(result2, "plotly"))
  expect_snapshot(result2)
})
