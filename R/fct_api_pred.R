#' Get the prediction URL or the data from the `pxtextmining API`
#' @description A function to call the `pxtextmining` API
#'
#' @param data Dataframe with column `comment_id`, `comment_text` and `question_type`
#' @param api_key api key to access the api
#' @param target to determine the type of prediction, the options are `m` for multilabel,
#' `s` for sentiment or `ms` for both. default to `ms`
#' @return a dataframe or a url to get the data
#' @export
get_api_pred_url <- function(data, api_key, target = "ms") {
  endpoint <- "https://pxtextmining-docker-api.azurewebsites.net/api/StartContainerInstance"

  # validate the target argument
  stopifnot("target must be one of 'ms', 'm' or 's'" = target %in% c("ms", "m", "s"))
  
  # convert the dataframe to nested list
  json_data <- data |>
    purrr::array_tree()

  r <- httr::POST(
    endpoint,
    body = json_data,
    query = list(code = api_key, target = target),
    encode = "json"
  )

  # throw an error when the API call result in an error
  if (!any((r$status_code == 200) | (r$status_code == 202))) {
    print(httr::http_status(r)$message) # for debugging
    stop(httr::http_status(r)$message, call. = FALSE)
  }

  cat("API call was Successful \n")

  # return the url to go get the data or the data
  if (r$status_code == 202) {
    results_url <- httr::content(r, "text")
    cat(glue::glue("URL for results is `{results_url}` \n"))
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
#' @param api_url the url return from `get_api_pred_url()` where you can get the prediction data when its ready
#'
#' @return a dataframe or string depending on whether the API is done prediction or not
#' @export
get_pred_from_url <- function(api_url) {
  results_response <- httr::GET(api_url)

  if (results_response$status_code == 200) {
    return(
      httr::content(results_response, "text") |>
        jsonlite::fromJSON()
    )
  } else if (results_response$status_code == 202) {
    print("Machine learning API is still busy. check back again...")
    return("Busy")
  } else {
    stop(httr::http_status(results_response)$message)
  }
}

#' convert the API prediction into a format that is suitable for the database
#'
#' @param prediction dataframe, prediction from the pxtextmining api with column "comment_id","labels" and "sentiment"
#'
#' @return dataframe, transform prediction data
#' @noRd
transform_prediction_for_database <- function(prediction) {
  stopifnot('"comment_id","labels" and "sentiment" columns are required' = all(c("comment_id", "labels", "sentiment") %in% names(prediction)))

  prediction |>
    dplyr::rename(
      category = labels
    ) |>
    dplyr::mutate(dplyr::across(c(comment_id, sentiment), as.integer)) |>
    #  assign the super categories
    tidyr::unnest(category) |> # Unnest the category column into rows and columns
    dplyr::mutate(super_category = assign_highlevel_categories(category)) |> # assign super categories
    dplyr::group_by(comment_id) %>%
    dplyr::summarise(
      across(-tidyselect::all_of(c("category", "super_category")), unique),
      across(c(category, super_category), list) # duplicate super category value is preserved. This will allow easy manipulation later. for ex. see get_tidy_filter_data()
    ) |>
    dplyr::ungroup() |>
    # convert the category and super category column to raw json before loading into the database
    dplyr::mutate(across(c(category, super_category), ~ purrr::map(.x, jsonlite::toJSON))) |>
    dplyr::mutate(across(c(category, super_category), ~ purrr::map(.x, charToRaw)))
}

#' Track the API job table. If prediction is done, it writes it to the main table and delete the job from the api job table
#'
#' @param job an instance of the api job table
#' @param conn database connection
#' @param write_db logical should the prediction data be written to the database or returned as a dataframe?
#'
#' @return dataframe (if `write_db` is FALSE)
#' @export
track_api_job <- function(job, conn, write_db = TRUE) {
  job_id <- as.character(job["job_id"])
  url <- as.character(job["url"])
  trust_id <- as.character(job["trust_id"])

  cat("Checking Job", job_id, "\n")
  prediction <- NULL

  tryCatch(
    {
      prediction <- get_pred_from_url(url)
    },
    error = function(.) {
      # update the job status as failed
      DBI::dbGetQuery(conn, paste("UPDATE api_jobs SET status='failed' WHERE job_id =", job_id))
    }
  )

  if (is.data.frame(prediction)) {
    cat("Job", job_id, "is done \n")

    # update the job status as complete (Prediction has been returned)
    DBI::dbExecute(conn, paste("UPDATE api_jobs SET status='completed' WHERE job_id =", job_id))

    if (!write_db) {
      # update the job status as uploaded (successfully write prediction to main table)
      DBI::dbExecute(conn, paste("UPDATE api_jobs SET status='uploaded' WHERE job_id =", job_id))

      return(prediction)
    }

    prediction <- prediction |>
      transform_prediction_for_database()

    # update the main table
    cat("Updating database with prediction \n")
    
    dplyr::rows_update(
      dplyr::tbl(conn, trust_id),
      prediction,
      by = "comment_id",
      unmatched = "ignore",
      copy = TRUE,
      in_place = TRUE
    )

    # update the job status as uploaded (successfully write prediction to main table)
    DBI::dbExecute(conn, paste("UPDATE api_jobs SET status='uploaded' WHERE job_id =", job_id))

    cat("Job", job_id, "prediction has been successfully written to database \n")
  } else if (is.character(prediction)) {
    cat("Job", job_id, "is still busy \n")
  } else {
    cat("Job", job_id, "failed \n")
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

#' Get for api jobs in the api table in the database
#'
#' @param pool database connection
#' @param trust_id string, the trust id
#' @param schedule_time integer, number of mins used in the Rmarkdown schedule that tracks the api job table.
#' default is 15mins. it's only needed to guess how long users might have to wait for the prediction to complete
#'
#' @return list of latest_time and estimated_wait_time (in mins)
#' @noRd
check_api_job <- function(pool, trust_id = get_golem_config("trust_name"), schedule_time = 15) {
  data <- dplyr::tbl(
    pool,
    dbplyr::in_schema(
      "TEXT_MINING",
      "api_jobs"
    )
  ) |>
    dplyr::filter(
      trust_id == !!trust_id,
      status %in% c("submitted", "completed")
    )

  if (data |>
    dplyr::tally() |>
    dplyr::pull() < 1) {
    return(list("latest_time" = NULL, "estimated_wait_time" = 0))
  } else {
    data <- data |>
      dplyr::filter(date == max(date, na.rm = TRUE))
  }

  latest_time <- data |>
    dplyr::select(date) |>
    dplyr::pull() |>
    lubridate::as_datetime()

  no_comments <- data |>
    dplyr::select(no_comments) |>
    dplyr::pull()

  wait_time <- floor(as.numeric(lubridate::now(tz = "UTC") - latest_time, units = "mins"))

  estimated_wait_time <- dplyr::case_when(
    no_comments < 1000 ~ schedule_time + 15 - wait_time,
    no_comments < 10000 ~ schedule_time + 30 - wait_time,
    no_comments < 25000 ~ schedule_time + 60 - wait_time,
    TRUE ~ 120
  )

  estimated_wait_time <- if (estimated_wait_time > 0) estimated_wait_time else 10
  
  list("latest_time" = latest_time, "estimated_wait_time" = estimated_wait_time)
}
