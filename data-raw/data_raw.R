## code to prepare datasets goes here

## Load packages ----
library(tidyverse)
library(janitor)
library(odbc)

## Load results from Andreas ----
sentiment_txt_data <- readRDS(file = here::here("data-raw/sentiment_txt_data.rds")) %>% 
  dplyr::rename(division = division2, directorate = directorate2)
usethis::use_data(sentiment_txt_data, overwrite = TRUE)

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
                       AND Date > '2020-10-01'")

# Tidy variable names
trustData <- trustData %>%
  janitor::clean_names()

categoriesTable <- dbGetQuery(con, "SELECT * from NewCodes") %>%
  janitor::clean_names()

# Create tidy data set
tidy_trust_data <- trustData %>%
  dplyr::left_join(categoriesTable, 
                   by = c("imp_n1" = "code")) %>%
  dplyr::left_join(categoriesTable, 
                   by = c("best_n1" = "code"),
                   suffix = c("_imp", "_best")) %>% 
  dplyr::select(date, team_n, directorate = dir_t, division = division2, 
                improve, imp_crit, 
                imp_category = subcategory_imp, imp_super = category_imp, 
                best, best_crit, 
                best_category = subcategory_best, best_super = category_best,
                service) %>% 
  dplyr::arrange(date) %>% 
  dplyr::na_if(9) %>% 
  dplyr::mutate(across(where(is.character), ~na_if(., "Unknown"))) %>% 
  # Come up with better key
  dplyr::mutate(key_user = 1 : nrow(.)) %>% 
  tidyr::pivot_longer(cols = c("improve", "best"), 
                      names_to = "comment_type",
                      values_to = "comment_txt") %>% 
  dplyr::mutate(crit = case_when(comment_type == "improve" ~ imp_crit,
                                 comment_type == "best" ~ best_crit),
                category = case_when(comment_type == "improve" ~ imp_category,
                                     comment_type == "best" ~ best_category),
                super_category = case_when(comment_type == "improve" ~ imp_super,
                                           comment_type == "best" ~ best_super)) %>% 
  dplyr::mutate(key_comment = paste0(key_user, "_", comment_type)) %>% 
  dplyr::select(key_user, key_comment, date, team_n, directorate, division, 
                comment_type, comment_txt, crit, category, super_category,
                service) %>%
  # only keep comments that are within possible range or NA
  filter(crit %in% -5 : 5 | is.na(crit) == TRUE) %>% 
  # Drop comments with missing values
  tidyr::drop_na(comment_txt)

usethis::use_data(tidy_trust_data, overwrite = TRUE)
