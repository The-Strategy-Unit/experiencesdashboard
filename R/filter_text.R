#' Return text from a freetext search
#' @description combine search terms with OR and return text from a specific
#' question
#' @param text_data the dataframe, raw from the database
#' @param string comma separated string with search terms in
#' @param comment_type_filter which comment to return- 1 or 2
#' 
#' @export
#' @return string vector of search terms, separated by <p>, </p> for 
#' display as raw HTML by Shiny

return_search_text <- function(text_data, filter_text, comment_type_filter){
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