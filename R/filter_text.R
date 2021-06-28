returnSearchText <- function(text_data, filter_text, comment_type_filter){
  
  # remove trailing punctuation from both input strings
  
  search_string <- filter_text
  
  searchTextInclude <- sub("[[:punct:]]$", "", trimws(search_string))
  
  text_data %>%
    dplyr::filter(comment_type == comment_type_filter) %>% 
    dplyr::filter(grepl(paste(
      trimws(unlist(strsplit(searchTextInclude, ","))), 
      collapse = "|"), comment_txt)) %>% 
    dplyr::pull(comment_txt) %>%
    paste0("<p>", ., "</p>")
  
}