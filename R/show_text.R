show_text <- function(data, filter_by_column, filter_by_text, comment_type_filter){
  
  return(
    data %>%
      dplyr::filter(comment_type == comment_type_filter,
                    .data[[filter_by_column]] == filter_by_text) %>% 
      dplyr::pull(comment_txt) %>%
      paste0(.,hr())
  )
}


#' find the common comments between two categories
#'
#' @param data a dataframe with a unique row identifier names `row_id`
#' @param theme_column the column where the lookup values to compare is
#' @param filter_by_themes list of two or three values to compare
#'
#' @return strings
#' @noRd
show_multilabeled_text <- function(data, theme_column, filter_by_themes) {
  
  a = data %>%
    dplyr::filter(.data[[theme_column]] == filter_by_themes[1]) %>% 
    dplyr::pull(row_id)
  
  b = data %>%
    dplyr::filter(.data[[theme_column]] == filter_by_themes[2]) %>%
    dplyr::pull(row_id)
  
  returned_id <-  dplyr::intersect(a, b)
  
  if (length(filter_by_themes) == 3){
    c = data %>%
      dplyr::filter(.data[[theme_column]] == filter_by_themes[3]) %>% 
      dplyr::pull(row_id)
    returned_id <- dplyr::intersect(returned_id, c)
  }
  
  final_data <- data %>% 
    dplyr::filter(row_id %in% returned_id) %>% 
    dplyr::distinct(dplyr::across(c(-name, -value))) 
  
  # return(final_data)

  if (nrow(final_data) > 0){
    return(
      final_data %>% 
        dplyr::pull(comment_txt) %>%
        paste0(., hr())
    )
  } else{
    return(paste('No Matching comment'))
  }
}
