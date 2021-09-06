#' Clean an uploaded dataset from a user of the dashboard
#' @description uploaded data will often have garbage in the comments 
#' box such as "NULL", "NA", "N/A", etc. Clean these comments before they go 
#' to the pipeline
#' @param data a dataframe of uploaded patient experience data
#' @param text_cols a vector of strings with the name of the text columns
#' 
#' @return dataframe with cleaned text
#' @export
clean_dataframe <- function(data, text_cols){
  
  data %>% 
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(text_cols), ~ dplyr::case_when(
          grepl("^[?]+$", .) ~ NA_character_,
          . %in% c("NULL", "#NAME?", "") ~ NA_character_,
          TRUE ~ .
        )))
  
}

#' Tidy data upload from spreadsheet
#'
#' @param data dataframe, loaded within Shiny application
#' @param conn connection, from existing {pool}
#' @param trust_id string. Which trust are you tidying data for?
#'
#' @return boolean, indicating success or failure in upload
#'
#' @section Last updated by:
#' Chris Beeley
#' @section Last updated date:
#' 2021-04-25

upload_data <- function(data, conn, trust_id){
  
  text_fields <- grep("comment_", colnames(data), value = TRUE)
  code_fields <- grep("code", colnames(data), value = TRUE)
  score_fields <- "fft"
  
  db_tidy <- data
  
  if(trust_id == "demo_trust"){
    
    db_tidy <- db_tidy %>% 
      dplyr::mutate(location_1 = sample(c("Location A", "Location B", 
                                          "Location C"),
                                        nrow(db_tidy), replace = TRUE)) %>% 
      dplyr::mutate(date = sample(seq(as.Date("2019-01-01"), 
                                      as.Date("2021-01-01"), by = "days"), 
                                  nrow(db_tidy), replace = TRUE))
    
    # delete ALL previous data 
    
    DBI::dbExecute(conn, "TRUNCATE TABLE demo_trust")
  }
  
  db_tidy <- db_tidy %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("comment_"),
                        names_to = "comment_type",
                        values_to = "comment_txt") %>% 
    dplyr::select(any_of(c("date", "location_1", "location_2",
                           "location_3", "fft", "comment_type",
                           "comment_txt",
                           "gender", "age", "sexuality", "disability",
                           "faith", "ethnicity", "pt_id"))) %>% 
    clean_dataframe(., c("comment_txt",
                         "gender", "age", "sexuality", "disability",
                         "faith", "ethnicity"))
  
  if(trust_id == "trust_c"){
    
    db_tidy <- db_tidy %>% 
      dplyr::mutate(fft = dplyr::case_when(
        fft %in% c("Dont know", "Dont Know") ~ NA_integer_,
        fft == "Very poor" ~ 1L,
        fft == "Poor" ~ 2L,
        fft == "Neither good nor poor" ~ 3L, 
        fft == "Good" ~ 4L,
        fft == "Very good" ~ 5L
      )) %>%
      dplyr::mutate(age = dplyr::case_when(
        age == "Up to 25" ~ "0 - 25",
        TRUE ~ age)) %>% 
      dplyr::mutate(date = as.Date(date, format = "%d/%m/%Y"))
  }
  
  preds <- pxtextmineR::factory_predict_unlabelled_text_r(
    dataset = db_tidy,
    predictor = "comment_txt",
    pipe_path_or_object = "fitted_pipeline.sav",
    column_names = "preds_only") %>% 
    dplyr::select(category = comment_txt_preds)
  
  criticality <- pxtextmineR::factory_predict_unlabelled_text_r(
    dataset = db_tidy,
    predictor = "comment_txt",
    pipe_path_or_object = "pipeline_criticality.sav",
    column_names = "preds_only") %>% 
    dplyr::select(crit = comment_txt_preds)
  
  final_df <- dplyr::bind_cols(
    db_tidy, 
    preds,
    criticality)
  
  success <- DBI::dbWriteTable(conn, trust_id,
                               final_df, append = TRUE)
  
  return(success)
}