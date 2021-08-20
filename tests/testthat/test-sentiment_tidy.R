test_that("Sentiment works", {
  
  nrc_sentiments <- sentiment_nrc %>%
    dplyr::select(sentiment) %>%
    dplyr::distinct() %>%
    dplyr::pull() %>%
    sort()
  
  sentiment_df <- calc_sentiment(tidy_trust_data, nrc_sentiments)
  
  test_df <- tidy_sentiment_txt(sentiment_df)
  
  table_df <- test_df %>% 
    make_sentiment_table(nrc_sentiments)
  
  expect_gt(nrow(sentiment_df), 0)
  
  expect_gt(nrow(test_df), 0)
})
