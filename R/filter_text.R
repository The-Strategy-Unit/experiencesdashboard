#' Filter a dataframe based on specified comment type
#'
#' @param text_data the dataframe from the database
#' @param comment_type_filter comment type value to be used to filter
#'
#' @noRd
#'
#' @return list of strings
filter_df <- function(text_data, comment_type_filter) {
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
#' @noRd
#'
#' @return string as a vector
input_sanitizer <- function(filter_text) {
  strsplit(filter_text, ",")[[1]] %>%
    stringr::str_to_lower() %>%
    stringr::str_remove_all("[^[:alnum:]]")
}

#' Takes a list of comments and return it in lowercase
#'
#' @param comments list of strings
#'
#' @noRd
#'
#' @return list of strings
lowered_comments <- function(comments) {
  comments %>%
    gsub("[^[:graph:]]", " ", .) %>%
    tolower()
}

#' Find list of words in a string
#'
#' @param string a string
#' @param search_strings list of strings with search terms in it
#' @param search_fn type of search ('and', 'or')
#'
#' @noRd
#'
#' @return
check_match <- function(string, search_strings, search_fn) {
  search_fn(
    lapply(
      search_strings,
      \(p) grepl(paste0("\\b.*", p, ".*\\b"), string)
    ) %>% unlist()
  )
}

#' Find index in a list of string where search word(s) exist
#'
#' @param lowered_comments list of strings in lowercase
#' @param search_fn type of search type ('and', 'or')
#' @param search_strings list of strings with search terms in it
#'
#' @return list of logical values
#'
#' @examples
#' matched_comments(
#'   lowered_comments = c(
#'     "tricky times, I recommend quick appraisals ",
#'     "time of my situation.",
#'     ">quick response time, competent appraisal of my situation"
#'   ),
#'   search_fn = all, search_strings = c("quick", "time", "appraisal")
#' )
matched_comments <- function(lowered_comments, search_fn, search_strings) {
  lowered_comments %>%
    lapply(\(comment) check_match(comment, search_strings, search_fn)) %>%
    unlist()
}

#' Return text from a freetext search
#'
#' @description combine search terms with OR and AND then return text from a specific
#' question
#'
#' @param text_data the dataframe, raw from the database
#' @param filter_text comma separated string with search terms in
#' @param comment_type_filter which comment to return- 1 or 2
#' @param search_type type of search ('and', 'or')
#'
#' @export
#'
#' @return string vector of search terms, separated by <p>, </p> for
#' display as raw HTML by Shiny
#'
#' @example
#' return_search_text(
#' text_data = data.frame(comment_txt = c("tricky times, I recommend quick appraisals ",
#' "time of my situation.", ">quick response time, competent appraisal of my situation"),
#' comment_type = c('comment_1', 'comment_1', 'comment_1')),
#' filter_text = 'Uick, time, appraisal$, &!',
#' comment_type_filter='comment_1', search_type = "and")
return_search_text <- function(text_data, filter_text, comment_type_filter, search_type = c("or", "and")) {
  # check argument is valid and choose the correct logical predicate
  search_type <- match.arg(search_type)
  stopifnot("search type must be one of 'or', or 'and'" = length(search_type) == 1)
  search_fn <- switch(search_type,
    "or" = any,
    "and" = all
  )

  # sanitise the input strings and add their stemmed words to the list of words to search
  
  sanitized_input <- input_sanitizer(filter_text) 
  stemmed_input = tm::stemDocument(sanitized_input)
  search_strings <- union(sanitized_input, stemmed_input)
  cat("search strings: ") # for logging
  print(search_strings) # for logging

  comments <- filter_df(text_data, comment_type_filter)

  matched_rows <- comments %>%
    lowered_comments() %>%
    matched_comments(search_fn, search_strings)

  if (any(matched_rows)) {
    paste0(hr(), comments[matched_rows])
  } else {
    paste0(hr(), "no matching result")
  }
}
