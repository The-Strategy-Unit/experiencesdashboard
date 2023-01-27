test_that("Searching text works", {
  
  test_text <- return_search_text(text_data = tidy_trust_data, 
                   filter_text = "happy", 
                   comment_type_filter = "comment_1",
                   search_type= 'or')
  
  expect_gt(length(test_text), 1)
})
