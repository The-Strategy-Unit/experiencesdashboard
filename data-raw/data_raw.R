## code to prepare datasets goes here

## Load packages ----
library(tidyverse)
library(janitor)
library(odbc)

## MySQL ----

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "MySQL ODBC 8.0 Unicode Driver",
                      Server   = Sys.getenv("HOST_NAME"),
                      UID      = Sys.getenv("DB_USER"),
                      PWD      = Sys.getenv("MYSQL_PASSWORD"),
                      Port     = 3306,
                      database = "SUCE")

trustData = dbGetQuery(con, 
                       "SELECT * FROM Local INNER JOIN Teams 
                       INNER JOIN Directorates ON Directorates.DirC = Teams.Directorate
                       WHERE Local.TeamC = Teams.TeamC
                       AND Local.Date >= Teams.date_from 
                       AND Local.Date <= Teams.date_to
                       AND Local.Date >= Directorates.date_from 
                       AND Local.Date <= Directorates.date_to
                       AND Date > '2020-10-01' 
                       AND (Optout = 'No' OR Optout IS NULL)")

# Tidy variable names
trustData <- trustData %>%
  janitor::clean_names()

categoriesTable <- dbGetQuery(con, "SELECT * from NewCodes") %>%
  janitor::clean_names() %>% 
  mutate(subcategory = paste0(category, ": ", subcategory))

# combine theme codes

trustData <- trustData %>% 
  dplyr::na_if(9) %>% 
  dplyr::mutate(across(where(is.character), ~na_if(., "Unknown"))) %>% 
  dplyr::mutate(across(where(is.character), ~na_if(., "XX"))) %>% 
  dplyr::mutate(key_user = 1 : nrow(.))

trustData <- bind_cols(
  trustData %>% 
    tidyr::pivot_longer(cols = c("improve", "best"), 
                        names_to = "comment_type",
                        values_to = "comment_txt"),
  trustData %>% 
    tidyr::pivot_longer(cols = c("imp_crit", "best_crit"), 
                        names_to = "crit_type",
                        values_to = "crit") %>% 
    select(crit_type, crit)
) %>% 
  dplyr::rowwise() %>% 
  dplyr::mutate(impcodes = list(na.omit(c(imp_n1, imp_n2)))) %>% 
  dplyr::select(-imp_n1, -imp_n2) %>% 
  tidyr::unnest(impcodes) %>% 
  dplyr::left_join(categoriesTable, by = c("impcodes" = "code"))

tidy_trust_data <- trustData %>% 
  dplyr::select(key_user, date, team_n, 
                directorate = dir_t, division = division2,
                comment_txt, comment_type,
                crit, 
                super_category = category, category = subcategory, 
                service) %>% 
  dplyr::mutate(key_comment = paste0(key_user, "_", comment_type)) %>% 
  # only keep comments that are within possible range or NA
  filter(crit %in% -5 : 5 | is.na(crit) == TRUE)

usethis::use_data(tidy_trust_data, overwrite = TRUE)

## Load results from Andreas ----
sentiment_txt_data <- readRDS(file = here::here("data-raw/sentiment_txt_data.rds")) %>% 
  dplyr::rename(division = division2, directorate = directorate2)

# add fake dates

sentiment_txt_data$date <- sample(tidy_trust_data$date, 
                                  nrow(sentiment_txt_data), replace = TRUE)

sentiment_txt_data <- sentiment_txt_data %>% 
  dplyr::mutate(division = dplyr::recode(division, 
                          "Local partnerships- CH" = "General health services"))

usethis::use_data(sentiment_txt_data, overwrite = TRUE)



