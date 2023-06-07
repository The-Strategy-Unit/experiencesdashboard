test_that("split data spc works", {
  monthly <- split_data_spc(tidy_trust_data,
    variable = "fft",
    chunks = "monthly"
  )

  equal <- split_data_spc(tidy_trust_data,
    variable = "fft",
    chunks = 12
  )

  expect_gt(nrow(monthly), 0)

  expect_equal(length(unique(equal$date)), 12)
})
