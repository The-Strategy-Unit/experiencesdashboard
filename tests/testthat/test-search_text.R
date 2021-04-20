test_that("Searching text works", {
  
  test_text <- returnSearchText(text_data = tidy_trust_data, 
                   filter_text = c("staff, doctor, nurse"), 
                   comment_type_filter = "imp")
  
  expect_gt(length(test_text), 1)
})
