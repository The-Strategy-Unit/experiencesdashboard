#' Return text from a freetext search
#' @description combine search terms with OR and return text from a specific
#' question
#' @param text_data the dataframe, raw from the database
#' @param filter_text comma separated string with search terms in
#' @param comment_type_filter which comment to return- 1 or 2
#' @param search_type type of search ('and', 'or')
#' 
#' @export
#' @return string vector of search terms, separated by <p>, </p> for 
#' display as raw HTML by Shiny

return_search_text <- function(text_data, filter_text, comment_type_filter, search_type= c('or','and')){
  # remove trailing punctuation from both input strings
  search_string <- filter_text

  searchTextInclude <- sub("[[:punct:]]$", "", trimws(search_string))

  text_data <- text_data %>%
    dplyr::filter(comment_type == comment_type_filter)

  if (search_type == 'and'){
    lt = text_data %>% dplyr::pull(comment_txt)

    for (i in 1:length(searchTextInclude)){
      search_result = text_data %>%
        dplyr::filter(grepl(searchTextInclude[i], comment_txt, ignore.case = TRUE)) %>%
        dplyr::pull(comment_txt)
      lt = Reduce(intersect, list(lt,search_result))
    }
    lt %>% paste0("<p>", ., "</p>")
  }
  else{
    pattern <- paste(trimws(unlist(strsplit(searchTextInclude, ","))), collapse = "|")
    text_data %>%
      dplyr::filter(
        grepl(pattern, comment_txt, ignore.case = TRUE)
      ) %>%
      dplyr::pull(comment_txt) %>% 
      paste0("<p>", ., "</p>")
  }
}

# return_search_text <- function(text_data, filter_text, comment_type_filter){
#   # remove trailing punctuation from both input strings
#   
#   search_string <- filter_text
#   
#   searchTextInclude <- sub("[[:punct:]]$", "", trimws(search_string))
#   
#   text_data %>%
#     dplyr::filter(comment_type == comment_type_filter) %>% 
#     dplyr::filter(grepl(paste(
#       trimws(unlist(strsplit(searchTextInclude, ","))), 
#       collapse = "|"), comment_txt)) %>% 
#     dplyr::pull(comment_txt) %>%
#     paste0("<p>", ., "</p>")
#   
# }

