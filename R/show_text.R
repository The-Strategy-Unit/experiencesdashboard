show_text <- function(data, filter_by_column, filter_by_text, comment_type_filter) {
  return(
    data %>%
      dplyr::filter(
        comment_type == comment_type_filter,
        .data[[filter_by_column]] == filter_by_text
      ) %>%
      dplyr::pull(comment_txt) %>%
      paste0(., hr())
  )
}


#' find the common comments between two categories
#'
#' @param data a dataframe with a unique row identifier names `comment_id`
#' @param theme_column the column where the lookup values to compare is
#' @param filter_by_themes list of two or three values to compare
#'
#' @return strings
#' @noRd
show_multilabeled_text <- function(data, theme_column, filter_by_themes) {
  # check argument is valid
  stopifnot("length of filter_by_themes must '2' or '3'" = (length(filter_by_themes) == 2 | length(filter_by_themes) == 3))

  a <- data %>%
    dplyr::filter(.data[[theme_column]] == filter_by_themes[1]) %>%
    dplyr::pull(comment_id)

  b <- data %>%
    dplyr::filter(.data[[theme_column]] == filter_by_themes[2]) %>%
    dplyr::pull(comment_id)

  returned_id <- dplyr::intersect(a, b)

  if (length(filter_by_themes) == 3) {
    c <- data %>%
      dplyr::filter(.data[[theme_column]] == filter_by_themes[3]) %>%
      dplyr::pull(comment_id)
    returned_id <- dplyr::intersect(returned_id, c)
  }

  final_data <- data %>%
    dplyr::filter(comment_id %in% returned_id) %>%
    dplyr::select(-tidyselect::all_of(theme_column), -super_category) %>%
    dplyr::distinct()

  if (nrow(final_data) > 0) {
    return(
      final_data %>%
        dplyr::pull(comment_txt) %>%
        paste0(., tags$hr())
    )
  } else {
    return(paste("No Matching comment"))
  }
}

#' find the common comments between two categories
#'
#' @param data a dataframe with a unique row identifier names `comment_id`
#' @param theme_column the column where the lookup values to compare is
#' @param filter_by_themes list of two or three values to compare
#'
#' @return a dataframe
#' @noRd
relationship_table <- function(data, theme_column, filter_by_themes) {
  # check argument is valid
  stopifnot("length of filter_by_themes must '2' or '3'" = (length(filter_by_themes) == 2 | length(filter_by_themes) == 3))

  a <- data %>%
    dplyr::filter(.data[[theme_column]] == filter_by_themes[1]) %>%
    dplyr::pull(comment_id)

  b <- data %>%
    dplyr::filter(.data[[theme_column]] == filter_by_themes[2]) %>%
    dplyr::pull(comment_id)

  returned_id <- dplyr::intersect(a, b)

  if (length(filter_by_themes) == 3) {
    c <- data %>%
      dplyr::filter(.data[[theme_column]] == filter_by_themes[3]) %>%
      dplyr::pull(comment_id)
    returned_id <- dplyr::intersect(returned_id, c)
  }

  data %>%
    dplyr::filter(comment_id %in% returned_id)
}
