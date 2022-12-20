
# load sentiment_nrc

# install.packages("textdata")

sentiment_nrc <- tidytext::get_sentiments("afinn")

usethis::use_data(sentiment_nrc)
