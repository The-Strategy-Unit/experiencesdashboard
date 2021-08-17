test_that("Sentiment works", {
  
  sentiment_df <- calc_sentiment(tidy_trust_data)
  
  test_df <- tidy_sentiment_txt(sentiment_df)
  
  table_df <- test_df %>% 
    make_sentiment_table(c(
      "anger",
      "anticipation",
      "disgust",
      "fear",
      "joy",
      "negative"))
  
  expect_gt(nrow(sentiment_df), 0)
  
  expect_gt(nrow(test_df), 0)
})
