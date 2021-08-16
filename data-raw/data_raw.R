## code to prepare datasets goes here

## Load packages ----
library(tidyverse)
library(nottshcData)
library(nottshcMethods)
library(DBI)
library(experiencesdashboard)

## MySQL ----

open_db_data <- get_px_exp(from = "2017-01-01",
                           open_data = FALSE, 
                           remove_demographics = FALSE,
                           remove_optout = TRUE)

trust_a <- open_db_data %>% 
  arrange(Date) %>% 
  collect()

# final dataset needs location_1, location_2 location_3, date, 
# comment_txt, comment_type, fft, gender, age, ethnicity,
# sexuality, patient_carer, disability, faith

trust_a <- trust_a %>% 
  dplyr::select(location_1 = Division2, location_2 = DirT, location_3 = TeamN,
                date = Date, comment_1 = Improve, comment_2 = Best, 
                fft = Service, gender = Gender, 
                age = Age, ethnicity = Ethnic, sexuality = Sexuality, 
                patient_carer = SU, disability = Disability, faith = Religion) %>% 
  tidyr::pivot_longer(cols = c(Improve, Best),
                      names_to = "comment_type",
                      values_to = "comment_txt") %>% 
  clean_dataframe(., comment_txt)

trust_a <- trust_a %>% 
  dplyr::mutate(gender = dplyr::case_when(
    gender %in% c("M", "F", "O", NA) ~ gender,
    TRUE ~ NA_character_
  )) %>% 
  dplyr::mutate(ethnicity = dplyr::case_when(
    substr(ethnicity, 1, 1) == "W" ~ "White",
    substr(ethnicity, 1, 1) == "M" ~ "Mixed",
    substr(ethnicity, 1, 1) == "A" ~ "Asian",
    substr(ethnicity, 1, 1) == "B" ~ "Black",
    ethnicity == "O" ~ "Other",
    ethnicity == "GRT" ~ "Gypsy/ Romany/ Traveller",
    TRUE ~ NA_character_
  ))

# MUST randomise the demographic features

random_a <- trust_a %>% 
  dplyr::mutate(dplyr::across(c(gender, age, ethnicity, sexuality, 
                                patient_carer, disability, faith), 
                              ~ sample(.x, dplyr::n())))

preds <- experienceAnalysis::calc_predict_unlabelled_text(
  x = random_a,
  python_setup = FALSE,
  text_col_name = 'comment_txt',
  preds_column = NULL,
  column_names = "all_cols",
  pipe_path = 'fitted_pipeline.sav'
) %>% 
  dplyr::select(code = comment_txt_preds)

criticality <- experienceAnalysis::calc_predict_unlabelled_text(
  x = random_a,
  python_setup = FALSE,
  text_col_name = 'comment_txt',
  preds_column = NULL,
  column_names = "all_cols",
  pipe_path = 'pipeline_criticality.sav'
) %>% 
  dplyr::select(crit = comment_txt_preds)

final_df <- dplyr::bind_cols(
  random_a, 
  preds,
  criticality)

# blank comments should give blank predictions

final_df$code[is.na(final_df$comment_txt)] = NA
final_df$crit[is.na(final_df$comment_txt)] = NA

final_df <- final_df %>% 
  dplyr::mutate(age = dplyr::recode(
    age, `1` = "Under 12", `2` = "12-17", `3` = "18-25", `4` = "26-39",
    `5` = "40-64", `6` = "65-79", `7` = "80+", .default = NA_character_))

# add code and crit

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "Maria DB",
                      Server   = Sys.getenv("HOST_NAME"),
                      UID      = Sys.getenv("DB_USER"),
                      PWD      = Sys.getenv("MYSQL_PASSWORD"),
                      Port     = 3306,
                      database = "TEXT_MINING",
                      encoding = "UTF-8")

dbWriteTable(con, 'trust_a', final_df, overwrite = TRUE)

dbDisconnect(con)
