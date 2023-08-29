#' calculate percentage tables for comment themes
#'
#' @param table_data A dataframe
#' @param count_column String with the name of the column to count in the joined table
#' @param comment_type String with type of comment
#' @return a dataframe with category name, n, and %
#' @export
calculate_table <- function(table_data, count_column,
                            comment_type = NULL) {
  if (!is.null(comment_type)) {
    table_data <- table_data %>%
      dplyr::filter(comment_type == rlang::expr(!!comment_type))
  }

  table_data %>%
    dplyr::count(.data[[count_column]]) %>%
    purrr::set_names(c(count_column, "n")) %>%
    dplyr::filter(!is.na(.data[[count_column]])) %>%
    dplyr::mutate(percent = round(n / sum(n) * 100, 1)) %>%
    dplyr::arrange(dplyr::desc(percent))
}

#' calculate the percentage and sum for two groups in a data
#'
#' @param data a dataframe. similar to what you get from the database
#' @param group_col1 the first column to group by e.g. fft
#' @param group_col2 the second column to group by e.g. sentiment
#'
#' @return a dataframe
#' @noRd
multigroup_calculated_data <- function(data, group_col1, group_col2) {
  data %>%
    dplyr::group_by(.data[[group_col1]], .data[[group_col2]]) %>%
    dplyr::summarize(n = length(unique(comment_id))) %>% 
    dplyr::mutate(percent = n / sum(n) * 100) %>%
    dplyr::ungroup()
}
