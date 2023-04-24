#' api_pred 
#'
#' @description A function to call the `pxtextmining` API 
#' 
#' @param endpoint API endpoint
#' @param json JSON list of dictionaries with the following compulsory keys:
#' `comment_id` (string) and `comment_text` (string). For example, 
#' `[{'comment_id': '1', 'comment_text': 'Thank you'}, {'comment_id': '2', 'comment_text': 'Food was cold'}]`
#'
#' @return a dataframe 
#' @export
api_pred <- function(endpoint, json){
  
  r = httr::POST(endpoint, body=json)
  
  
  # throw an error when the API call result in an error
  if(r$status_code != 200) {
    
    print(httr::http_status(r)$message)  # for debugging
    
    stop(httr::http_status(r)$message, call. = FALSE)
  }
  
  data = httr::content(r, "text", encoding="UTF-8") |> 
    jsonlite::fromJSON()
  
  return(data)
}

# library(tidyverse)

# # convert data to json for API as specified in the API doc
# json_data <- readr::read_csv("./data-raw/multilabeled_data.csv",
#                                            show_col_types = FALSE) |>
#   select(-`...1`)  |>
#   dplyr::select(comment_id, comment_txt) |>
#   jsonlite::toJSON()
# 
# preds <-  api_pred("http://127.0.0.1:8000/predict_multilabel", json=json_data)
