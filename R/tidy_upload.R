# function to do trust specific data cleaning
tidy_trust_nuh <- function(db_tidy) {
  db_tidy %>%
    dplyr::mutate(age = as.integer(age)) %>%
    dplyr::mutate(
      age = dplyr::case_when(
        age < 8 ~ "0 - 7",
        age < 12 ~ "8 - 11",
        age < 16 ~ "12 - 15",
        age < 26 ~ "16 - 25",
        age < 36 ~ "26 - 35",
        age < 46 ~ "36 - 45",
        age < 56 ~ "46 - 55",
        age < 66 ~ "56 - 65",
        age > 65 ~ "Over 65",
        TRUE ~ NA_character_
      )
    )
}

tidy_trust_neas <- function(db_tidy) {
  db_tidy %>%
    dplyr::mutate(
      fft_score = dplyr::case_when(
        fft_score == "Very good" ~ 1,
        fft_score == "Good" ~ 2,
        fft_score == "Neither good nor poor" ~ 3,
        fft_score == "Poor" ~ 4,
        fft_score == "Very poor" ~ 5,
        fft_score == "Don’t know" ~ 6,
        TRUE ~ NA
      )
    )
}

tidy_trust_nth <- function(db_tidy) {
  db_tidy %>%
    dplyr::mutate(age = as.integer(age)) %>%
    dplyr::mutate(
      age = dplyr::case_when(
        age == 0 ~ "0 - 16",
        age == 1 ~ "17 - 24",
        age == 2 ~ "25 - 34",
        age == 3 ~ "35 - 44",
        age == 4 ~ "45 - 54",
        age == 5 ~ "55 - 64",
        age == 6 ~ "65 - 74",
        age == 7 ~ "75+",
        TRUE ~ "Prefer not to say"
      ),
      sex = dplyr::case_when(
        sex == 1 ~ "Male",
        sex == 2 ~ "Female",
        sex == 3 ~ "Prefer not to say",
        TRUE ~ "Prefer to self-describe"
      ),
      gender = dplyr::case_when(
        gender == 1 ~ "Male",
        gender == 2 ~ "Female",
        gender == 3 ~ "Prefer not to say",
        TRUE ~ "Prefer to self-describe"
      )
    )
}

#' Clean the comment column of uploaded dataset from a user of the dashboard
#'
#' @description uploaded data will often have garbage in the comments
#' box such as "NULL", "NA", "N/A", etc. Clean these comments and drop all
#' rows without comment before they go to the pipeline
#' @param data a dataframe of uploaded patient experience data
#' @param comment_column a string for the name of the column for the comment text
#'
#' @return dataframe with cleaned comment_column
#' @export
clean_dataframe <- function(data, comment_column) {
  data %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character),
        \(.x) stringr::str_replace_all(.x, "[^[:alnum:][:punct:]]+", " ") # remove non-graphical characters from all character columns, ‘⁠[:graph:  is not reliable⁠’
      ),
      dplyr::across(
        where(is.character), ~ dplyr::case_when(
          grepl("^[?]+$", .) ~ NA_character_, # remove multiple question marks
          . %in% c("NULL", "#NAME?", "NA", "N/A", "", " ") ~ NA_character_,
          TRUE ~ .
        )
      )
    ) %>%
    dplyr::filter(
      !is.na(.data[[comment_column]]),
      !is.null(.data[[comment_column]]),
      !.data[[comment_column]] %in% c("Did not answer"),
      nchar(.data[[comment_column]]) > 1
    )
}

#' Tidy data upload from users
#'
#' @param data dataframe, loaded within Shiny application
#' @param conn connection, from existing {pool}. Can be `NULL` if `write_db = FALSE`
#' @param trust_id string. Which trust are you uploading data for?
#' @param user the current user's username gotten from `session$user`
#' @param write_db logical should the final data be written to the database or returned as a dataframe?
#'
#' @return boolean, indicating success or failure in upload
upload_data <- function(data, conn, trust_id, user, write_db = TRUE) {
  # throw error if for any reason the trust_id is not same a trust name
  stopifnot("trust_id should be same as trust_name" = get_golem_config("trust_name") == trust_id)

  last_upload_date <- Sys.time() # to track when the data upload started. to be used in the api job table and the main table

  # reformat and clean the uploaded data ----
  required_cols <- c(
    "date", "pt_id", "location_1", "location_2", "location_3",
    "comment_type", "comment_text", "fft_score", "sex",
    "gender", "age", "ethnicity", "sexuality", "disability", "religion",
    "extra_variable_1", "extra_variable_2", "extra_variable_3"
  )

  # get the Maximum ids from the database table if data needs to be written to database
  if (write_db) {
    # get the current maximum pt_id value in the database table
    max_ptid <- DBI::dbGetQuery(conn, paste0("SELECT MAX(pt_id) FROM ", trust_id))$`MAX(pt_id)`
    max_ptid <- if (!is.na(max_ptid)) max_ptid else 0 # when there is no data in the database

    # get the current maximum comment_id value in the database table
    max_id <- DBI::dbGetQuery(conn, paste0("SELECT MAX(comment_id) FROM ", trust_id))$`MAX(comment_id)`
    max_id <- if (!is.na(max_id)) max_id else 0 # when there is no data in the database
  } else {
    max_ptid <- 0
    max_id <- 0
  }

  # parse the date (if it hasn't been parsed) and confirm if it's well parsed (the assumption here is that, data older than year 2000 won't be uploaded).
  data <- parse_date(data)
  stopifnot("Start year should reasonably be after year 2000" = lubridate::year(min(data$date)) > 2000)

  db_tidy <- data %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(pt_id = seq.int(max_ptid + 1, max_ptid + nrow(.))) %>% # to uniquely identify individual responder
    tidyr::pivot_longer(
      cols = dplyr::starts_with("question"),
      names_to = "comment_type",
      values_to = "comment_text"
    ) %>%
    dplyr::select(dplyr::any_of(required_cols)) %>%
    clean_dataframe("comment_text") %>%
    dplyr::mutate(
      comment_id = seq.int(max_id + 1, max_id + nrow(.)) # to uniquely identify individual comment
    ) 

  # do trust specific data cleaning ----
  if (trust_id == "trust_NUH") db_tidy <- db_tidy %>% tidy_trust_nuh()
  if (trust_id == "trust_NEAS") db_tidy <- db_tidy %>% tidy_trust_neas()
  if (trust_id == "trust_NTH") db_tidy <- db_tidy %>% tidy_trust_nth()

  # call API for predictions ----
  ## prepare the data for the API ----
  tidy_data <- db_tidy |>
    dplyr::mutate(question_type = comment_type) |>
    dplyr::mutate(
      question_type = stringr::str_replace_all(
        question_type, "question_1",
        api_question_code(get_golem_config("comment_1"))
      )
    ) |>
    dplyr::select(comment_id, comment_text, question_type)

  if (isTruthy(get_golem_config("comment_2"))) {
    tidy_data <- tidy_data |>
      dplyr::mutate(
        question_type = stringr::str_replace_all(
          question_type, "question_2",
          api_question_code(get_golem_config("comment_2"))
        )
      ) |>
      dplyr::select(comment_id, comment_text, question_type)
  } else {
    tidy_data <- tidy_data %>%
      dplyr::filter(question_type == api_question_code(get_golem_config("comment_1")))
    
    db_tidy <- db_tidy %>%
      dplyr::filter(comment_type == "comment_1")
  }

  ## get prediction url ----
  cat("Making sentiment and label predictions for", nrow(tidy_data), "comments from pxtextming API \n")
  api_result <- get_api_pred_url(tidy_data, Sys.getenv("API_key"))

  ## update api job table ----
  # get the maximum job id from the api job table
  max_job_id <- DBI::dbGetQuery(conn, paste0("SELECT MAX(job_id) FROM api_jobs"))$`MAX(job_id)`
  max_job_id <- if (is.na(max_job_id)) 0 else max_job_id

  job_table <- dplyr::tibble(
    job_id = max_job_id + 1,
    date = last_upload_date,
    url = api_result,
    trust_id = trust_id,
    user = user,
    no_comments = nrow(tidy_data),
    status = "submitted"
  )

  DBI::dbWriteTable(conn, "api_jobs", job_table, append = TRUE)

  cat("Done with API call. Remember to get prediction from URL... \n")

  # final data tidy ----
  cat("Doing final data tidy \n")
  final_df <- db_tidy %>%
    # rename the columns to make the data compatible with old data format currently in use
    dplyr::rename(
      fft = fft_score,
      comment_txt = comment_text
    ) %>%
    dplyr::mutate(
      hidden = 0,
      comment_type = stringr::str_replace_all(.data$comment_type, "question", "comment"),
      last_upload_date = last_upload_date # update the last upload date column with todays date
    )

  # Do a final check on the data before loading to db ----
  # throw error if comment_id is not unique
  stopifnot("values in 'comment ID' should be unique" = final_df$comment_id %>% duplicated() %>% sum() == 0)
  stopifnot("comment_id column should not be empty" = all(!is.na(final_df$comment_id) & final_df$comment_id != ""))

  if (write_db) {
    cat("Just started appending ", nrow(final_df), " rows of data into database ... \n")

    # write the processed data to database
    DBI::dbWriteTable(conn, trust_id, final_df, append = TRUE) # this doesn't throw error when data can't be read e.g dues to mismatch datatypes.

    print("Done appending to database ...")
  } else {
    return(final_df)
  }
}
