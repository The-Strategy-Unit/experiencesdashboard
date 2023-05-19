#' api_pred 
#' @description A function to call the `pxtextmining` API 
#' @param json JSON list of dictionaries with the following compulsory keys:
#' `comment_id` (string), `comment_text` (string) and `question_type` (any of ("what_good", "could_improve", "nonspecific"). For example, 
#' `[{'comment_id': '1', 'comment_text': 'Thank you', 'question_type': 'what_good'}, {'comment_id': '2', 'comment_text': 'Food was cold', 'question_type': 'could_improve'}]`
#' @return a dataframe 
#' @export
api_pred <- function(json){
  endpoint <- "https://connect.strategyunitwm.nhs.uk/content/015061a2-94ef-41ac-a5ac-313248fd82c9/predict_multilabel"
  
  r = httr::POST(endpoint, body=json, encode = "json",
                 httr::add_headers("Content-Type" = "application/json")
                 )
  
  # throw an error when the API call result in an error
  if(r$status_code != 200) {
    
    print(httr::http_status(r)$message)  # for debugging
    
    stop(httr::http_status(r)$message, call. = FALSE)
  }
  
  data = httr::content(r, "text", encoding="UTF-8") |> 
    jsonlite::fromJSON()
  
  return(data)
}

#' Make prediction in batches from the pxtextming API
#' @param df a dataframe  
#' @return a dataframe of length `df`
#' @examples batch_predict(data)
#' @noRd
batch_predict <- function(df){
  preds <- data.frame()
  a = nrow(df)
  f = c(seq(1, a, by = 1000))
  l = setdiff(unique(c(seq(0, a, by = 1000), a)), 0)
  
  for (i in 1:length(f)){
    d = df[f[i]:l[i], ]
    cat(paste0("Making predictions for batch ", i, '/', length(f)), " \n")
    p <- d |> 
      jsonlite::toJSON() |> 
      api_pred()
    preds  <- dplyr::bind_rows(preds, p)
  }
  return(preds)
}

# test code 

# library(tidyverse)

# # convert data to json for API as specified in the API doc
# df <- readr::read_csv("data/phase_2/p2_db_sample.csv",
#                       show_col_types = FALSE) |> slice_sample(n = 50, replace = T) %>% 
#   rename(comment_id = row_id, 
#          comment_text= comment_txt,
#          question_type = comment_type) %>% 
#   dplyr::select(comment_id, comment_text, question_type) 
# 
# json_data <- df |>
#   jsonlite::toJSON()
# 
# preds <-  api_pred(json_data) %>% 
#   dplyr::mutate(comment_id = as.integer(comment_id))
# 
# df %>% dplyr::left_join(preds, by = c('comment_id', 'comment_text')) %>% 
#   dplyr::rename(category = labels,
#                 comment_txt = comment_text, 
#                 comment_type = question_type
#   ) %>% View()
