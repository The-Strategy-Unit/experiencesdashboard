#' calculate percentage tables for comment themes
#' 
#' @param table_data A dataframe
#' @param count_column String with the name of the column to count in the joined table
#' @param comment_type String with type of comment- improve/ best
#' @param click_column A string with the name of the category that has been clicked, if any
#' @return a dataframe with category name, n, and %
#' @export
calculate_table <- function(table_data, count_column, 
                            comment_type = c("improve", "best"), 
                            click_column = NULL) {
  
  if(!is.null(click_column)){
    table_data <- table_data %>% 
      dplyr::filter(super_category == click_column)
  }
  
  table_data %>% 
    dplyr::filter(comment_type == rlang::expr(!!comment_type)) %>%
    dplyr::filter(!(category == "Miscellaneous: Nothing was good/bad" &
                      comment_type == "improve")) %>% 
    dplyr::count(.data[[count_column]]) %>%
    purrr::set_names(c("Category", "n")) %>% 
    dplyr::filter(!is.na(Category)) %>%
    dplyr::mutate(percent = round(n / sum(n) * 100, 1)) %>%
    dplyr::arrange(dplyr::desc(percent))
}
