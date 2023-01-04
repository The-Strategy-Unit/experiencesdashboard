#' Tidy patient experience data
#'
#' @param data dataframe or SQL object, that you can make with get_px_exp()
#'
#' @return
#' @export
#'
#' @section Last updated by:
#' Chris Beeley
#' @section Last updated date:
#' 2021-04-25
tidy_all_trusts <- function(data) {
  
  # this line only works if there is data in the table
  
  if(data %>%
     dplyr::tally() %>% 
     dplyr::pull(n) > 0) {
    
    data %>%  
      dplyr::mutate(category = dplyr::case_when(
        is.null(comment_txt) ~ NA_character_,
        is.na(comment_txt) ~ NA_character_,
        comment_txt %in% c("NULL", "NA", "N/A") ~ NA_character_,
        TRUE ~ category
      ))
  } else {
    
    data
  }
}
