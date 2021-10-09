
# load NHS.uk data

list_df <- purrr::map(2017 : 2021, function(x){
  
  nhs_uk <- httr::GET(
    # first, the URL to query
    "https://api.nhs.uk/comments/Comments",
    query = list( "odsCode" = "RHA", "limit" = 200, 
                  "fromDate" = glue::glue("{x}-01-01"),
                  "toDate" = glue::glue("{x}-12-31")
    ),
    httr::content_type_json(),
    # we need to tell httr how to encode the "body" argument, in this case we want to encode using json
    encode = "json",
    # finally we add the subscription key
    httr::add_headers(
      "subscription-key" = Sys.getenv("nhs_api_key")
    )
  )
  
  nhs_uk_content <- httr::content(nhs_uk)
  
  comment_list <- lapply(nhs_uk_content$comments,function(x){
    
    x$comment$commentText
  })
  
  date_list <- lapply(nhs_uk_content$comments,function(x){
    
    x$comment$dateSubmitted
  })
  
  return(
    tibble::tibble(
      "date" = lubridate::as_date(unlist(date_list)),
      "comment_txt" = unlist(comment_list))
  )
})

final_df <- do.call("rbind", list_df$value)

write_df <- final_df %>% 
  dplyr::select(comment_txt, date) %>% 
  dplyr::mutate(comment_type = "comment_1")

# predict comments

preds_theme <- pxtextmineR::factory_predict_unlabelled_text_r(
  dataset = write_df,
  predictor = "comment_txt",
  pipe_path_or_object = "fitted_pipeline.sav",
  column_names = "preds_only") %>% 
  dplyr::rename(category = comment_txt_preds)

preds_crit <- pxtextmineR::factory_predict_unlabelled_text_r(
  dataset = write_df,
  predictor = "comment_txt",
  pipe_path_or_object = "pipeline_criticality.sav",
  column_names = "preds_only") %>% 
  dplyr::rename(crit = comment_txt_preds)

nhs_uk_write <- cbind(
  write_df, preds_theme, preds_crit
) %>% 
  dplyr::mutate(pt_id = 1 : nrow(.))

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "Maria DB",
                      Server   = Sys.getenv("HOST_NAME"),
                      UID      = Sys.getenv("DB_USER"),
                      PWD      = Sys.getenv("MYSQL_PASSWORD"),
                      Port     = 3306,
                      database = "TEXT_MINING",
                      encoding = "UTF-8")

DBI::dbWriteTable(con, 'nhs_uk', nhs_uk_write, append = TRUE)

DBI::dbDisconnect(con)
