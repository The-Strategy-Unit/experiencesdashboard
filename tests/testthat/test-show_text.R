test_that("Show text works", {
  
  test_text <- show_text(data = tidy_trust_data,
                         filter_by_column = "category",
                         filter_by_text = "Access",
                         comment_type_filter = "imp")
  
  expect_gt(length(test_text), 1)
})

