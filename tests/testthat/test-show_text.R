test_that("Show text works", {
  
  test_text <- show_text(data = tidy_trust_data,
                         filter_by_column = "super_category",
                         filter_by_text = "Access",
                         comment_type_filter = "improve")
  
  expect_gt(length(test_text), 1)
})

