# function to do trust specific data cleaning

tidy_trust_gosh <- function(db_tidy) {
  db_tidy %>%
    dplyr::mutate(age = as.integer(age)) %>%
    dplyr::mutate(
      age = dplyr::case_when(
        age < 12 ~ "0 - 11",
        age > 11 | age < 18 ~ "12 - 17",
        age > 17 | age < 26 ~ "18 - 25",
        age > 25 | age < 40 ~ "26 - 39",
        age > 39 | age < 65 ~ "40 - 64",
        age > 64 | age < 80 ~ "65 - 79",
        age > 79 ~ "80+",
        TRUE ~ as.character(age)
      )
    )
}

tidy_trust_d <- function(db_tidy) {
  db_tidy %>%
    dplyr::mutate(fft = 6 - fft) %>%
    dplyr::mutate(date = lubridate::as_date(date, format = "%m/%d/%Y"))
}

tidy_trust_c_e <- function(db_tidy) {
  db_tidy %>%
    dplyr::mutate(fft = dplyr::case_when(
      fft %in% c("Dont know", "Dont Know") ~ NA_integer_,
      fft == "Very poor" ~ 1L,
      fft == "Poor" ~ 2L,
      fft == "Neither good nor poor" ~ 3L,
      fft == "Good" ~ 4L,
      fft == "Very good" ~ 5L,
      TRUE ~ NA_integer_
    )) %>%
    dplyr::mutate(age = dplyr::case_when(
      age == "Up to 25" ~ "0 - 25",
      TRUE ~ age
    )) %>%
    dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y"))
}


#' Clean an uploaded dataset from a user of the dashboard
#' @description uploaded data will often have garbage in the comments
#' box such as "NULL", "NA", "N/A", etc. Clean these comments and drop all
#' rows without comment before they go to the pipeline
#' @param data a dataframe of uploaded patient experience data
#' @param comment_column a string for the name of the column for the comment text
#'
#' @return dataframe with cleaned text
#' @export
clean_dataframe <- function(data, comment_column) {
  data %>%
    dplyr::mutate(
      dplyr::across(
        where(is.character), ~ dplyr::case_when(
          grepl("^[?]+$", .) ~ NA_character_, # remove multiple question marks
          . %in% c("NULL", "#NAME?", "") ~ NA_character_,
          TRUE ~ .
        )
      )
    ) %>%
    dplyr::filter(
      !is.na(.data[[comment_column]]),
      !is.null(.data[[comment_column]]),
      !.data[[comment_column]] %in% c("NULL", "NA", "N/A")
    )
}

#' Tidy data upload from users
#'
#' @param data dataframe, loaded within Shiny application
#' @param conn connection, from existing {pool}. Can be `NULL` if `write_db = FALSE`
#' @param trust_id string. Which trust are you uploading data for?
#' @param write_db logical should the final data be written to the database or returned as a dataframe?
#'
#' @return boolean, indicating success or failure in upload
upload_data <- function(data, conn, trust_id, write_db = TRUE) {
  
  # throw error if for any reason the trust_id is not same a trust name
  stopifnot('trust_id should be same as trust_name' = get_golem_config('trust_name') == trust_id)
  
  if (trust_id == "demo_trust") {
    db_tidy <- data %>%
      dplyr::mutate(location_1 = sample(
        c(
          "Location A", "Location B",
          "Location C"
        ),
        nrow(data),
        replace = TRUE
      )) %>%
      dplyr::mutate(date = sample(
        seq(as.Date("2019-01-01"),
          as.Date("2021-01-01"),
          by = "days"
        ),
        nrow(data),
        replace = TRUE
      ))

    # delete ALL previous data

    DBI::dbExecute(conn, "TRUNCATE TABLE demo_trust")
  }

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

  db_tidy <- data %>%
    dplyr::mutate(pt_id = seq.int(max_ptid + 1, max_ptid + nrow(.))) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("question"),
      names_to = "comment_type",
      values_to = "comment_text"
    ) %>%
    dplyr::select(dplyr::any_of(required_cols)) %>%
    dplyr::mutate(comment_id = 1:nrow(.)) %>% # to uniquely identify individual comment
    clean_dataframe("comment_text")

  # do trust specific data cleaning ----
  if (trust_id == "trust_GOSH") db_tidy <- db_tidy %>% tidy_trust_gosh()

  # call API for label predictions ----
  # prepare the data for the API

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
  }

  cat("Making predictions for ", nrow(db_tidy), "comments from pxtextming API \n")

  preds <- batch_predict(tidy_data) %>%
    dplyr::mutate(comment_id = as.integer(comment_id))

  print("Done with API call ...")

  # rename the columns to make the data compatible with old data format currently in use
  final_df <- db_tidy %>%
    dplyr::left_join(preds, by = c("comment_id", "comment_text")) %>%
    dplyr::rename(
      fft = fft_score, category = labels,
      comment_txt = comment_text
    ) %>%
    dplyr::select(-comment_id) %>%
    dplyr::mutate(
      hidden = 0,
      date = as.Date(.data$date)
    )

  print("Doing final data tidy")
  # set the starting value for the auto-incremented comment_id
  # This will ensure that when we append the new data,
  # the comment_id values will be sequential and there will be no gaps.
  final_df <- final_df %>%
    dplyr::mutate(
      comment_id = seq.int(max_id + 1, max_id + nrow(.)),
      comment_type = stringr::str_replace_all(.data$comment_type, "question", "comment")
    ) %>%
    # update the last upload date column with todays date
    dplyr::mutate(last_upload_date = Sys.time()) %>%
    #  assign the super categories
    tidyr::unnest(category) %>% # Unnest the category column into rows and columns
    dplyr::mutate(super_category = assign_highlevel_categories(category)) %>% # assign super categories
    dplyr::group_by(comment_id, comment_type) %>%
    dplyr::summarise(
      across(-tidyselect::all_of(c("category", "super_category")), unique),
      across(c(category, super_category), list) # duplicate super category value is preserved. This will allow easy manipulation later. for ex. see get_tidy_filter_data()
    ) %>%
    dplyr::ungroup() %>%
    # convert the category and super category column to raw json before loading into the database
    dplyr::mutate(across(c(category, super_category), ~ purrr::map(.x, jsonlite::toJSON))) %>%
    dplyr::mutate(across(c(category, super_category), ~ purrr::map(.x, charToRaw)))

  # throw error if comment_id is not unique
  stopifnot("values in 'comment ID' should be unique" = final_df$comment_id %>% duplicated() %>% sum() == 0)

  if (write_db) {
    # write the processed data to database
    cat("Just started appending ", nrow(final_df), " rows of data into database ... \n")
    # DBI::dbWriteTable(conn, trust_id,  final_df, append = TRUE) # this doesn't throw error when data can't be read e.g dues to mismatch datatypes.
    # this throw error when data can't be appended e.g when data column can't be coerce into the db table datatype
    dplyr::rows_append(
      dplyr::tbl(conn, trust_id),
      final_df,
      copy = TRUE,
      in_place = TRUE
    )
    print("Done appending to database ...")
  } else {
    return(final_df)
  }
}
