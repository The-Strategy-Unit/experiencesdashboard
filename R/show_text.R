show_text <- function(data, filter_by_column, filter_by_text, comment_type_filter){
  
  return(
    data %>%
      dplyr::filter(comment_type == comment_type_filter,
                    .data[[filter_by_column]] == filter_by_text) %>% 
      dplyr::distinct(comment_key, .keep_all = TRUE) %>%
      dplyr::pull(comment_txt) %>%
      paste0("<p>", ., "</p>")
  )
}
