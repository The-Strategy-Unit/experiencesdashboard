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
  # split on commas and remove trailing punctuation from both input strings
  search_strings <- strsplit(filter_text, ",")[[1]] %>%
    sub("[^[:alpha:]+]$", "", .) %>%
    tolower()

  # check argument is valid and choose the correct logical predicate
  search_type <- match.arg(search_type)
  stopifnot("search type must be one of 'or', or 'and'" = length(search_type) == 1)
  search_fn <- switch(search_type,
    "or" = any,
    "and" = all
  )

  comments <- text_data %>%
    dplyr::filter(comment_type == comment_type_filter) %>%
    dplyr::pull(comment_txt)

  matched_comments <- comments %>%
    sub("[^[:graph:]]", " ", .) %>%
    tolower() %>%
    strsplit(" ") %>%
    vapply(\(x) search_fn(search_strings %in% x), logical(1))

  paste0("<p>", comments[matched_comments], "</p>")
}
