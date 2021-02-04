#' Tidy sentiment data for further analysis and visualisation
#' @description This function currently has no arguments, but will become more flexible with time 
#' @param data Dataframe with sentiment analysis results
#'
#' @return
#' @export
tidy_sentiment_txt <- function(data) {
  
  data %>% 
    dplyr::mutate(date = lubridate::date(date),
                  year = lubridate::year(date),
                  id = 1:nrow(data),
                  all_sentimtents_unnest = all_sentiments) %>% 
    dplyr::select(id, date, year, super, division2, improve, all_sentiments, 
                  all_sentimtents_unnest) %>% 
    tidyr::unnest(cols = all_sentimtents_unnest) %>% 
    dplyr::distinct() %>% 
    dplyr::mutate(value = TRUE) %>% 
    tidyr::pivot_wider(id_cols = c("id", "date", "year", "super", "division2", 
                                   "improve", "all_sentiments"), 
                       names_from = all_sentimtents_unnest, 
                       values_from = value) %>% 
    janitor::clean_names() %>% 
    dplyr::select(-na) %>%
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
