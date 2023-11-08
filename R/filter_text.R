#' Filter a dataframe based on specified comment type
#'
#' @param text_data the dataframe from the database
#' @param comment_type_filter comment type value to be used to filter
#'
#' @noRd
#'
#' @return list of strings
filter_df <- function(text_data, comment_type_filter = NULL) {
  if (is.null(comment_type_filter)) {
    text_data
  } else {
    text_data %>%
      dplyr::filter(comment_type == comment_type_filter)
  }
}

#' Takes input strings and splits them/sanitizes them
#'
#' @description split list of strings by commas and remove all non-alphanumeric characters form them
#'
#' @param filter_text comma separated string with search terms in
#'
#' @noRd
#'
#' @return vector of string
input_sanitizer <- function(filter_text) {
  sanitized_input <- strsplit(filter_text, ",")[[1]] %>%
    stringr::str_to_lower() %>%
    stringr::str_remove_all("[^[:alnum:]]")

  return(sanitized_input[sanitized_input != ""])
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
    stringr::str_replace_all("[^[:graph:]]+", " ") %>%
    stringr::str_to_lower()
}

#' Check if a word or its variation (singular or plural
#' version - if they are valid words) exist in a comment
#'
#' @param comment string, a single comment
#' @param search_fn type of search ('and', 'or')
#' @param sanitized_input list of strings, best derived from `input_sanitizer``
#' @return logical
#' @noRd
match_term_or_stem <- function(comment, search_fn, sanitized_input) {
  tokens <- gsub("[[:punct:] ]+", " ", comment) %>%
    strsplit(" ") %>%
    unlist()

  lapply(sanitized_input, \(x) any(
    x %in% tokens, # actual word
    ifelse(stringr::str_sub(x, start = -1L) == "s",
      stringr::str_sub(x, end = -2L)  %in% tokens,
      FALSE
    ), # attempt to get the singular form of a word by removing last "s"
    paste0(x, "s") %in% tokens # the plural version
  )) %>%
    unlist() %>%
    search_fn()
}

#' Find comment in list of comments where all/any of search strings exist
#'
#' @param lowered_comments list of strings in lowercase
#' @param search_fn type of search type ('and', 'or')
#' @param search_strings list of strings with search terms in it
#'
#' @return list of logical values
#' @export
#'
#' @examples
#' matched_comments(
#'   lowered_comments = c(
#'     "tricky times, I recommend quick appraisals ",
#'     "time of my situation is quick",
#'     ">quick response time, competent appraisal of my situation"
#'   ),
#'   search_fn = all, search_strings = c("quick", "time", "appraisal")
#' )
matched_comments <- function(lowered_comments, search_fn, search_strings) {
  lowered_comments %>%
    lapply(\(comment) match_term_or_stem(comment, search_fn, search_strings)) %>%
    unlist()
}

#' Return text from a freetext search
#' @description combine search terms with OR and AND then return text from a specific
#' question
#'
#' @param text_data the dataframe, raw from the database
#' @param filter_text comma separated string with search terms in
#' @param comment_type_filter which comment to return- 1 or 2
#' @param search_type type of search ('and', 'or')
#' @param return_dataframe should the output be a dataframe or an html string
#'
#' @return string vector of search terms, separated by <p>, </p> for
#' display as raw HTML by Shiny or a dataframe
#' @examples
#' return_search_text(
#'   text_data = data.frame(
#'     comment_txt = c(
#'       "tricky times, I recommend quick appraisals ",
#'       "time of my situation.", ">quick response time, competent appraisal of my situation"
#'     ),
#'     comment_type = c("comment_1", "comment_1", "comment_1")
#'   ),
#'   filter_text = "Uick, time, appraisal$, &!",
#'   comment_type_filter = "comment_1", search_type = "and",
#'   return_dataframe = FALSE
#' )
#' @export
return_search_text <- function(text_data, filter_text, comment_type_filter = NULL,
                               search_type = c("or", "and"), return_dataframe = TRUE) {
  # check argument is valid and choose the correct logical predicate
  search_type <- match.arg(search_type, c("or", "and"))
  stopifnot("search type must be one of 'or', or 'and'" = length(search_type) == 1)
  search_fn <- switch(search_type,
    "or" = any,
    "and" = all
  )

  # sanitise the input strings and add their stemmed words to the list of words to search
  search_strings <- input_sanitizer(filter_text)
  cat("search strings: ") # for logging
  print(search_strings) # for logging

  text_data <- filter_df(text_data, comment_type_filter)

  comments <- text_data %>%
    dplyr::pull(comment_txt)

  matched_rows <- comments %>%
    lowered_comments() %>%
    matched_comments(search_fn, search_strings)

  if (return_dataframe) {
    return(text_data[matched_rows, ])
  }

  if (any(matched_rows)) {
    paste0(hr(), comments[matched_rows])
  } else {
    paste0(hr(), "no matching result")
  }
}
