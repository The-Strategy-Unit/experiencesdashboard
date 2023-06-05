test_that("Show text works", {
  test_text <- show_text(
    data = tidy_trust_data,
    filter_by_column = "category",
    filter_by_text = "Care received",
    comment_type_filter = "comment_1"
  )

  expect_gt(length(test_text), 1)
})
