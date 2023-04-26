#' Filter a dataframe based on specified comment type
#'
#' @param text_data the dataframe from the database
#' @param comment_type_filter comment type value to be used to filter
#'
#' @return list of strings 
filter_df <- function(text_data, comment_type_filter){
  text_data %>%
    dplyr::filter(comment_type == comment_type_filter) %>%
    dplyr::pull(comment_txt)
} 


#' Takes input strings and splits them/sanitizes them
#'
#' @description split list of strings by commas and remove all non-alphanumeric characters form them
#'  
#' @param filter_text comma separated string with search terms in
#'
#' @return string as a vector
input_sanitizer <- function(filter_text){
  strsplit(filter_text, ",")[[1]] %>%
  stringr::str_to_lower() %>%
  stringr::str_remove_all("[^[:alnum:]]")
}


#' Takes a list of comments and splits them into tokens (words)
#'
#' @param comments list of strings
#'
#' @return list of tokenized words 
split_comments <- function(comments){
  comments %>%
    gsub("[^[:graph:]]", " ", .) %>%
    tolower() %>%
    strsplit(" ")
}


#' Find  rows with matching string(s) 
#'
#' @param split_comments list of tokenized words 
#' @param search_fn type of search ('and', 'or')
#' @param search_strings list of strings with search terms in it
#'
#' @return
matched_comments <- function(split_comments, search_fn, search_strings) {
  split_comments %>% 
    vapply(\(x) search_fn(search_strings %in% x), logical(1))
}


#' Return text from a freetext search
#' @description combine search terms with OR and AND then return text from a specific
#' question
#' @param text_data the dataframe, raw from the database
#' @param filter_text comma separated string with search terms in
#' @param comment_type_filter which comment to return- 1 or 2
#' @param search_type type of search ('and', 'or')
#'
#' @export
#' @return string vector of search terms, separated by <p>, </p> for
#' display as raw HTML by Shiny
return_search_text <- function(text_data, filter_text, comment_type_filter, search_type = c("or", "and")) {
  
  # check argument is valid and choose the correct logical predicate
  search_type <- match.arg(search_type)
  stopifnot("search type must be one of 'or', or 'and'" = length(search_type) == 1)
  search_fn <- switch(search_type,
    "or" = any,
    "and" = all
  )
  
  # split on commas and remove trailing punctuation from both input strings
  search_strings <- input_sanitizer(filter_text)
  cat("search strings: ") # for logging
  print(search_strings)   # for logging

  comments <- filter_df(text_data, comment_type_filter)

  matched_rows <- comments %>% 
    split_comments() %>% 
    matched_comments(search_fn, search_strings)
  
  if (any(matched_rows)){
    
    paste0("<p>", comments[matched_rows], "</p>")
    
  }else{
    
    paste0("<p>no matching result</p>")
  }
}

