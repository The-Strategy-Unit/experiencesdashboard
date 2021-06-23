# pool <- pool::dbPool(drv = odbc::odbc(),
#                      driver = "Maria DB",
#                      server = Sys.getenv("HOST_NAME"),
#                      UID = Sys.getenv("DB_USER"),
#                      PWD = Sys.getenv("MYSQL_PASSWORD"),
#                      database = "TEXT_MINING",
#                      Port = 3306)

raw_df <- readr::read_csv("tests/text_mining_upload_c (copy).csv")

raw_df <- raw_df %>% 
  dplyr::filter(!is.na(date))

preds <- experienceAnalysis::calc_predict_unlabelled_text(
  x = raw_df,
  python_setup = FALSE,
  text_col_name = 'comment',
  preds_column = NULL,
  column_names = "__all__",
  pipe_path = 'fitted_pipeline.sav'
) %>% 
  dplyr::select(code = comment_preds)

criticality <- experienceAnalysis::calc_predict_unlabelled_text(
  x = raw_df,
  python_setup = FALSE,
  text_col_name = 'comment',
  preds_column = NULL,
  column_names = "__all__",
  pipe_path = 'pipeline_criticality.sav'
) %>% 
  dplyr::select(criticality = comment_preds)

final_df <- dplyr::bind_cols(
  raw_df, 
  preds,
  criticality)

# DBI::dbWriteTable(pool, "trust_c",
#                   final_df, append = TRUE)
