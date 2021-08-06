#' Produce sentiment dataframe for tidying
#' @description Take the raw dataframe and mark it up for sentiment analysis
#' @param data dataframe- raw from the database
#' @return dataframe with sentiment columns
#' @export
calc_sentiment <- function(data){
  
  # doing data here is a little wasteful, TBH, but this operation is quick
  
  nrc_sentiments <- sentiment_nrc %>%
    dplyr::select(sentiment) %>%
    dplyr::distinct() %>%
    dplyr::pull() %>%
    sort()
  
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
                    dplyr::select(., dplyr::all_of(nrc_sentiments)) %>%
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
    tidyr::pivot_wider(id_cols = c("id", "date", "year", "category", "location_1", 
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
