## code to prepare datasets goes here

## Load packages ----
library(tidyverse)
library(nottshc)
library(nottshcMethods)

## MySQL ----

open_db_data <- get_px_exp(from = "2020-10-01",
                           open_data = FALSE, 
                           remove_demographics = FALSE,
                           remove_optout = TRUE)

trust_a <- tidy_px_exp(open_db_data) %>% 
  arrange(date) %>% 
  collect()

# MUST randomise the demographic features

trust_a <- trust_a %>% 
  mutate(across(gender : baby, ~ sample(.x, n())))

# test because of encoding problems

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "MySQL ODBC 8.0 Unicode Driver",
                      Server   = Sys.getenv("HOST_NAME"),
                      UID      = Sys.getenv("DB_USER"),
                      PWD      = Sys.getenv("MYSQL_PASSWORD"),
                      Port     = 3306,
                      database = "SUCE",
                      encoding = "UTF-8")

df <- data.frame(x = 1, y = "佃煮惣菜", z = "It's okay", a = "Zürich")
dbWriteTable(con, 'test-utf8', df, temporary = TRUE)
dbReadTable(con, 'test-utf8')

dbDisconnect(con)

# ## Load results from Andreas ----
# sentiment_txt_data <- readRDS(file = here::here("data-raw/sentiment_txt_data.rds")) %>% 
#   dplyr::rename(division = division2, directorate = directorate2)
# 
# # add fake dates
# 
# sentiment_txt_data$date <- sample(tidy_trust_data$date, 
#                                   nrow(sentiment_txt_data), replace = TRUE)
# 
# sentiment_txt_data <- sentiment_txt_data %>% 
#   dplyr::mutate(division = dplyr::recode(division, 
#                           "Local partnerships- CH" = "General health services",
#                           "Forensic services" = "Forensic"))
# 
# usethis::use_data(sentiment_txt_data, overwrite = TRUE)
