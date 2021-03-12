show_text <- function(data, filter_by, category_filter, comment_type_filter){
  
  return(
    data %>%
      dplyr::filter(.data[[filter_by]] == comment_type,
                    comment_type == comment_type_filter) %>% 
      dplyr::distinct(key_comment, .keep_all = TRUE) %>%
      dplyr::pull(comment_txt) %>%
      paste0("<p>", ., "</p>")
  )
}

show_text(data = tidy_trust_data, 
          filter_by = "super_category", 
          category_filter = category_selected, 
          comment_type_filter = comment_type)