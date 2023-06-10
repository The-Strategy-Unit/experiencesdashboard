library(data.validator)
library(assertr)
library(dplyr)
load(here::here("tests/framework.rda"))

test <- readr::read_csv(here::here("tests/test_data.csv"),
  show_col_types = FALSE
)

tidy_trust_data <- test %>%
  tidy_all_trusts()

# data base data
pool <- odbc::dbConnect(
  drv = odbc::odbc(),
  driver = Sys.getenv("odbc_driver"),
  server = Sys.getenv("HOST_NAME"),
  UID = Sys.getenv("DB_USER"),
  PWD = Sys.getenv("MYSQL_PASSWORD"),
  database = "TEXT_MINING",
  Port = 3306
)
# saveRDS(phase_2_upload_data, file = here::here('tests/phase_2_test_template.rds'))
phase_2_upload_data <- readRDS(here::here("tests/phase_2_test_template.rds"))

# phase_2_db_data <- upload_data(head(phase_2_upload_data, 1000), conn = pool, trust_id = "trust_NUH", write_db =F)
# saveRDS(phase_2_db_data, file = here::here('tests/p2_db_data_template.rds'))
phase_2_db_data <- readRDS(here::here("tests/p2_db_data_template.rds")) %>%
  dplyr::mutate(across(category, ~ purrr::map(.x, rawToChar)))

unique_data <- phase_2_db_data %>%
  dplyr::distinct(pt_id, .keep_all = TRUE)

# a <- upload_data(head(phase_2_upload_data, 1000), conn = NULL, trust_id = "trust_NUH", write_db =F)

#
#
# Sys.setenv("R_CONFIG_ACTIVE" = "trust_LPT")
pkgload::load_all(export_all = T)
#
#
# # api_question_code(get_golem_config("comment_1")))
#
#
#
# db_data <- dplyr::tbl(
#   pool,
#   dbplyr::in_schema(
#     "TEXT_MINING",
#     "phase_2_demo"
#   )
# ) |>
#   # tidy_all_trusts() |>
#   dplyr::collect()
#
#
# # phase_2_upload_data <- phase_2_upload_data |>
# #   mutate(location_1 = stringr::str_replace_all(location_1, 'GOSH', 'Community Health Services'),
# #          location_1 = stringr::str_replace_all(location_1, 'NEAS', 'Ambulance Services'),
# #          location_1 = stringr::str_replace_all(location_1, 'NHFT', 'Forensic'),
# #          location_1 = stringr::str_replace_all(location_1, 'LPT', 'Children Services'),
# #          location_1 = stringr::str_replace_all(location_1, 'NUTH', 'Adult Services'),
# #          location_1 = stringr::str_replace_all(location_1, 'NUH', 'Mental Health'))
#
# unique(phase_2_db_data$location_1)
#
# # df = df %>% select(-`age == stringr::str_replace_all(age, \"Dec-17\", \"26-39\")`)
#
#
# # phase_2_db_data$location_1 |> unique()
# #
# # comment <- db_data$comment_txt |> na.omit()
# #
# # df = df %>% mutate(question_1 =  comment[1:nrow(df)],
# #                    question_2 = comment[3001:(nrow(df)+3000)])
# #
# # df$question_2 |> drop_nulls() %>% length()
# #   nrow(df)
# #
