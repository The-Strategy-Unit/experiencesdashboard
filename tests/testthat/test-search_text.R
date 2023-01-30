test_that("Searching text works", {
  
  text_data <- tibble::tribble(
    ~comment_type, ~comment_txt,
    "a", "hello world",
    "a", "world things",
    "a", "other stuff"
  )
  
  expect_equal(
    text_data |>  return_search_text("&things,wo%^$rld", "a", "or"),
    c("<p>hello world</p>", "<p>world things</p>")
  )
  
  expect_equal(
    text_data |>  return_search_text("thi!ngs,world>", "a", "and"),
    '<p>world things</p>')
  
  test_text <- return_search_text(text_data = tidy_trust_data, 
                                  filter_text = "Listen, staff", 
                                  comment_type_filter = "comment_2",
                                  search_type= 'or')
  
  expect_equal(length(test_text), 3)
  
  test_text <- return_search_text(text_data = tidy_trust_data, 
                                  filter_text = "Listen, staff", 
                                  comment_type_filter = "comment_2",
                                  search_type= 'and')
  
  expect_equal(length(test_text), 1)

})
