test_that("Sentiment tidy works", {
  
  test_df <- tidy_sentiment_txt(sentiment_txt_data)
  
  expect_gt(nrow(test_df), 0)
})
