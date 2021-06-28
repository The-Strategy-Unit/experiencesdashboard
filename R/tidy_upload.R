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
  
  # TIDY FUNCTION HERE
  db_tidy <- data %>%
    dplyr::filter(!is.na(date)) %>% 
    tidyr::pivot_longer(cols = dplyr::starts_with("comment_"),
                        names_to = "comment_type",
                        values_to = "comment_txt") %>% 
    dplyr::select(any_of(c("date", "location_1", "location_2",
                           "location_3", "fft", "comment_type",
                           "comment_txt")))
  
  if(trust_id != "demo_trust"){
    
    db_tidy <- db_tidy %>%
      dplyr::mutate_at(dplyr::all_of(score_fields), ~ dplyr::case_when(
        . %in% 0 : 5 ~ .,
        TRUE ~ NA_real_)) 
  }
  
  preds <- experienceAnalysis::calc_predict_unlabelled_text(
    x = db_tidy,
    python_setup = FALSE,
    text_col_name = 'comment_txt',
    preds_column = NULL,
    column_names = "all_cols",
    pipe_path = 'fitted_pipeline.sav'
  ) %>% 
    dplyr::select(code = comment_txt_preds)
  
  criticality <- experienceAnalysis::calc_predict_unlabelled_text(
    x = db_tidy,
    python_setup = FALSE,
    text_col_name = 'comment_txt',
    preds_column = NULL,
    column_names = "all_cols",
    pipe_path = 'pipeline_criticality.sav'
  ) %>% 
    dplyr::select(crit = comment_txt_preds)
  
  final_df <- dplyr::bind_cols(
    db_tidy, 
    preds,
    criticality)
  
  success <- DBI::dbWriteTable(pool, trust_id,
                               final_df, append = TRUE)
  
  return(success)
}