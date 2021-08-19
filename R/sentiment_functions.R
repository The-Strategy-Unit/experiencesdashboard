#' Produce sentiment dataframe for tidying
#' @description Take the raw dataframe and mark it up for sentiment analysis
#' @param data dataframe- raw from the database
#' @param sentiment_names all of the nrc sentiments in a vector of strings
#' @return dataframe with sentiment columns
#' @export
calc_sentiment <- function(data, sentiment_names){
  
  data %>%
    dplyr::mutate(linenumber = dplyr::row_number()) %>% 
    tidytext::unnest_tokens(word, comment_txt) %>%
    dplyr::left_join(sentiment_nrc, by = "word") %>% 
    dplyr::count(linenumber, sentiment, name = 'sentiment_count') %>%
    dplyr::mutate(sentiment_count = dplyr::case_when(
      is.na(sentiment) ~ NA_integer_,
      TRUE ~ sentiment_count)) %>%
    dplyr::select(linenumber, sentiment, sentiment_count) %>%
    tidyr::pivot_wider(names_from = sentiment, 
                       values_from = sentiment_count, 
                       values_fill = 0,
                       names_sort = TRUE) %>%
    dplyr::full_join(
      data %>%
        dplyr::mutate(linenumber = dplyr::row_number()),
      by = "linenumber") %>%
    dplyr::mutate(all_sentiments =  
                    dplyr::select(., dplyr::any_of(sentiment_names)) %>%
                    split(seq(nrow(.))) %>%
                    lapply(function(x) unlist(names(x)[x != 0]))
    )
}

#' Tidy sentiment data for further analysis and visualisation
#' @description This function currently has no arguments, but will become more 
#' flexible with time 
#' @param data Dataframe with sentiment analysis results
#'
#' @return tidied dataframe suitable for plotting
#' @export
tidy_sentiment_txt <- function(data) {
  
  data %>% 
    dplyr::mutate(date = lubridate::date(date),
                  year = lubridate::year(date),
                  id = 1:nrow(data),
                  all_sentiments_unnest = all_sentiments) %>% 
    dplyr::select(id, date, year, category, location_1, comment_txt, 
                  all_sentiments, all_sentiments_unnest) %>% 
    tidyr::unnest(cols = all_sentiments_unnest) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(value = TRUE) %>% 
    tidyr::pivot_wider(id_cols = c("id", "date", "year", "category", 
                                   "location_1", 
                                   "comment_txt", "all_sentiments"), 
                       names_from = all_sentiments_unnest, 
                       values_from = value) %>% 
    janitor::clean_names() %>% 
    tidyr::replace_na(list(trust = FALSE, 
                           anticipation = FALSE, 
                           positive = FALSE, 
                           negative = FALSE, 
                           sadness = FALSE, 
                           fear = FALSE, 
                           joy = FALSE,
                           anger = FALSE, 
                           disgust = FALSE, 
                           surprise = FALSE)
    ) %>% 
    dplyr::mutate_if(is.logical, as.numeric)
  
}

#' Produce a table of comments with different sentiments
#' for example to be drawn with {reactable}
#' 
#' @param data dataframe you almost certainly made with 
#' \code{\link{tidy_sentiment_txt}}
#' @param sentiment_names all of the nrc sentiments in a vector of strings
#' @export
#' 
#' @return a table suitable for {reactable}
make_sentiment_table <- function(data, sentiment_names){
  
  data %>% 
    dplyr::select(id, all_sentiments, comment_txt) %>% 
    # First get number of total sentiments in all comments
    dplyr::mutate(length = lengths(all_sentiments),
                  all_sentiments_unnest = all_sentiments) %>%
    # # Now filter for number of selected sentiments
    # Unnest to create long version of data
    tidyr::unnest(cols = all_sentiments_unnest) %>% 
    # Group by comment id so that every computation is now for each comment
    dplyr::group_by(id) %>%
    dplyr::mutate(test_sentiment = dplyr::case_when(
      all_sentiments_unnest %in% sentiment_names ~ TRUE),
      sum_temp = sum(test_sentiment, na.rm = TRUE)) %>%
    dplyr::ungroup()
}
