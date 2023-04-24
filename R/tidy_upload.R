# function to do trust specific data cleaning

tidy_trust_d <- function(db_tidy){
  
  db_tidy %>% 
    dplyr::mutate(fft = 6 - fft) %>%
    dplyr::mutate(date = lubridate::as_date(date, format = "%m/%d/%Y"))
  
}

tidy_trust_c_e <- function(db_tidy){
  
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
      TRUE ~ age)) %>% 
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
clean_dataframe <- function(data, comment_column){
  
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
#' @param conn connection, from existing {pool}
#' @param trust_id string. Which trust are you tidying data for?
#'
#' @return boolean, indicating success or failure in upload
upload_data <- function(data, conn, trust_id){
  
  if(trust_id == "demo_trust"){
    
    db_tidy <- data %>% 
      dplyr::mutate(location_1 = sample(c("Location A", "Location B", 
                                          "Location C"),
                                        nrow(data), replace = TRUE)) %>% 
      dplyr::mutate(date = sample(seq(as.Date("2019-01-01"), 
                                      as.Date("2021-01-01"), by = "days"), 
                                  nrow(data), replace = TRUE))
    
    # delete ALL previous data 
    
    DBI::dbExecute(conn, "TRUNCATE TABLE demo_trust")
  }
  
  # reformat and clean the uploaded data ----
  
  required_cols <- c("date", "location_1", "location_2", "location_3", 
                     "comment_type","comment_text", "fft_score",
                     "gender", "age", "ethnicity", "sexuality", "disability",
                     "faith", "pt_id")
  
  # get the current maximum pt_id value in the database table
  
  max_ptid <- DBI::dbGetQuery(conn, paste0("SELECT MAX(pt_id) FROM ", trust_id))$`MAX(pt_id)`
  
  db_tidy <- data %>%  
    dplyr::mutate(pt_id = seq.int(max_ptid + 1, max_ptid + nrow(.))) %>% 
    # dplyr::mutate(pt_id = uuid::UUIDgenerate(use.time = TRUE, n = nrow(.))) %>% # generate time based unique id
    tidyr::pivot_longer(cols = dplyr::starts_with("question"),
                        names_to = "comment_type",
                        values_to = "comment_text") %>% 
    dplyr::select(dplyr::any_of(required_cols)) %>% 
    dplyr::mutate(comment_id = 1:nrow(.)) %>%   # to track individual comment
    clean_dataframe('comment_text') 
  
  # do trust specific data cleaning ----
  
  if (trust_id == "trust_c") db_tidy <- db_tidy %>% tidy_trust_c_e()
  if (trust_id == "trust_d")  db_tidy <- db_tidy %>% tidy_trust_d()
  if (trust_id == "trust_e") db_tidy <- db_tidy %>% tidy_trust_c_e()
  
  # call API for label predictions ----
  
  # convert data to json for API as specified in the API doc
  json_data <- db_tidy  |>
    dplyr::select(comment_id, comment_text) |>
    jsonlite::toJSON()
  
  preds <-  api_pred("http://127.0.0.1:8000/predict_multilabel", json=json_data) %>% 
    dplyr::mutate(comment_id = as.integer(comment_id))
  
  print('Done with API call ...')
  
  # rename the columns to make the data compatible with old data format currently in use
  
  final_df <- db_tidy %>% dplyr::left_join(preds, by = c('comment_id', 'comment_text')) %>% 
    dplyr::rename(fft = fft_score, category = labels,
                  comment_txt = comment_text, 
    ) %>% 
    dplyr::select(-comment_id)  %>% 
    dplyr::mutate(hidden = 0,
                  date = as.Date(.data$date)
    ) %>%
    tidy_label_column('category') 
  
  # get the current maximum  row_id value in the database table
  
  max_id <- DBI::dbGetQuery(conn, paste0("SELECT MAX(row_id) FROM ", trust_id))$`MAX(row_id)`
  
  # set the starting value for the auto-incremented row_id
  # This will ensure that when we append the new data, 
  # the row_id values will be sequential and there will be no gaps.
  final_df <- final_df %>% 
    dplyr::mutate(
      row_id = seq.int(max_id + 1, max_id + nrow(.)),
      comment_type = stringr::str_replace_all(.data$comment_type, 'question', 'comment')
    )
  
  # write the processed data to database
  print('Just started appending to database ...')
  
  # DBI::dbWriteTable(conn, trust_id,  final_df, append = TRUE) # this doesn't throw error when data can't be read e.g dues to mismatch datatypes.
  
  # this throw error when data can't be appended e.g when data column can't be coerce into the db table datatype
  dplyr::rows_append(
    dplyr::tbl(conn, trust_id),
    final_df,
    copy = TRUE,
    in_place = TRUE
  )
  
  print('Done appending to database ...')
  
  # reset the db data ------------------- remove later ------------------------
  old_row_count <- 52745
  nw_row_count <- DBI::dbReadTable(conn, 'trust_a_bk') %>% dplyr::arrange(desc(row_id)) %>% nrow()
  row_difference <- nw_row_count - old_row_count
  
  cat(nrow(final_df), 'rows in data to append \n')
  cat(row_difference, 'rows added \n') # confirm data addition
  DBI::dbExecute(conn, paste0('DELETE FROM `trust_a_bk` WHERE `row_id`> ',old_row_count))
}
