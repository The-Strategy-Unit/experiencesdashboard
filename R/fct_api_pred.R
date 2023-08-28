#' Get the prediction URL or the data from the `pxtextmining API`
#' @description A function to call the `pxtextmining` API
#'
#' @param data Dataframe with column `comment_id`, `comment_text` and `question_type`
#' @param api_key api key to access the api
#' @return a dataframe or a url to get the data
#' @export
api_pred_url <- function(data, api_key = Sys.getenv("API_Key")) {
  endpoint <- paste0("https://pxtextmining-docker-api.azurewebsites.net/api/StartContainerInstance?code=", api_key)

  json_data <- data |>
    jsonlite::toJSON()

  r <- httr::POST(endpoint,
    body = json_data, encode = "json",
    httr::add_headers("Content-Type" = "application/json")
  )

  # throw an error when the API call result in an error
  if (!any((r$status_code == 200) | (r$status_code == 202))) {
    print(httr::http_status(r)$message) # for debugging
    stop(httr::http_status(r)$message, call. = FALSE)
  }

  print("Successful Call")
  # return the url to go get the data or the data
  if (r$status_code == 202) {
    results_url <- httr::content(r, "text")
    print(glue::glue("URL for results is `{results_url}`"))
    return(results_url)
  } else {
    return(
      httr::content(r, "text", encoding = "UTF-8") |>
        jsonlite::fromJSON()
    )
  }
}

#' Get the prediction from the URL returned from the pxtextmining API
#'
#' @param api_url the url return from `api_pred_url()` where you can get the prediction data when its ready
#'
#' @return
#' @export
#'
#' @examples
get_pred_from_url <- function(api_url) {
  results_response <- httr::GET(api_url)

  if (results_response$status_code == 200) {
    return_data <- httr::content(results_response, "text") |>
      jsonlite::fromJSON()
    end <- lubridate::seconds(Sys.time())

    time_taken <- end - start
    print(glue::glue("Time taken for {nrow(df)} comments: {round(time_taken,2)} seconds"))

    return(return_data)
  } else if (results_response$status_code == 202) {
    print("Machine learning API is still busy. Trying again in 60 seconds...")
    return("Busy")
    
  } else {
    stop(httr::http_status(results_response)$message)
  }
}

#' api_pred
#' @description A function to call the `pxtextmining` API
#' @param json JSON list of dictionaries with the following compulsory keys:
#' `comment_id` (string), `comment_text` (string) and `question_type` (any of ("what_good", "could_improve", "nonspecific"). For example,
#' `[{'comment_id': '1', 'comment_text': 'Thank you', 'question_type': 'what_good'}, {'comment_id': '2', 'comment_text': 'Food was cold', 'question_type': 'could_improve'}]`
#' @return a dataframe
#' @export
api_pred <- function(json) {
  endpoint <- "https://connect.strategyunitwm.nhs.uk/content/015061a2-94ef-41ac-a5ac-313248fd82c9/predict_multilabel"
  
  r <- httr::POST(endpoint,
    body = json, encode = "json",
    httr::add_headers("Content-Type" = "application/json")
  )

  # throw an error when the API call result in an error
  if (r$status_code != 200) {
    print(httr::http_status(r)$message) # for debugging
    stop(httr::http_status(r)$message, call. = FALSE)
  }

  data <- httr::content(r, "text", encoding = "UTF-8") |>
    jsonlite::fromJSON()

  return(data)
}

#' Make prediction in batches from the pxtextming API
#' @param df a dataframe
#' @return a dataframe of length `df`
#' @examples batch_predict(data)
#' @noRd
batch_predict <- function(df) {
  preds <- data.frame()
  a <- nrow(df)
  f <- c(seq(1, a, by = 1000))
  l <- setdiff(unique(c(seq(0, a, by = 1000), a)), 0)

  for (i in 1:length(f)) {
    d <- df[f[i]:l[i], ]
    cat(paste0("Making predictions for batch ", i, "/", length(f)), " \n")
    p <- d |>
      # jsonlite::toJSON() |>
      api_pred()
    preds <- dplyr::bind_rows(preds, p)
  }
  return(preds)
}

#' Match config questions to api code
#'
#' @param value get_golem_config('comment_2')
#' @noRd
#' @examples api_question_code(get_golem_config("comment_1"))
api_question_code <- function(value) {
  dplyr::case_when(
    value == "What did we do well" ~ "what_good",
    value == "What could be improved" ~ "could_improve",
    TRUE ~ "nonspecific"
  )
}
