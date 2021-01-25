## code to prepare datasets goes here

## Load packages ----
library(tidyverse)
library(janitor)


## Load results from Andreas ----
sentiment_txt_data <- readRDS(file = here("data-raw/sentiment_txt_data.rds"))
usethis::use_data(sentiment_txt_data, overwrite = TRUE)


## Load raw data from Chris ----
## This needs to be improved so that we get data directly from sql
load(url("https://raw.github.com/ChrisBeeley/naturallanguageprocessing/master/cleanData.Rdata"))

# Tidy variable names
trustData <- trustData %>%
  janitor::clean_names()

categoriesTable <- categoriesTable %>%
  janitor::clean_names()

# Create tidy data set
tidy_trust_data <- trustData %>%
  dplyr::left_join(categoriesTable, 
                   by = c("imp1" = "number")) %>%
  dplyr::left_join(categoriesTable, 
                   by = c("best1" = "number"),
                   suffix = c("_imp", "_best")) %>% 
  dplyr::select(date, team_n, directorate2, division2, 
                improve, imp_crit, 
                imp_category = category_imp, imp_super = super_imp, 
                best, best_crit, 
                best_category = category_best, best_super = super_best) %>% 
  # Only work with old categories
  dplyr::filter(date < "2020-10-01") %>%
  #only work with data that has criticality rating
  dplyr::filter(date > "2010-04-01") %>% 
  dplyr::arrange(date) %>% 
  # Come up with better key
  dplyr::mutate(key_user = 1:nrow(.)) %>% 
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
  dplyr::select(key_user, key_comment, date, team_n, directorate2, division2, 
                comment_type, comment_txt, crit, category, super_category) %>%
  # only keep comments that are within possible range or NA
  filter(crit %in% c(1:3) | is.na(crit) == TRUE) %>% 
  # Drop comments with missing values
  tidyr::drop_na(comment_txt)


usethis::use_data(tidy_trust_data, overwrite = TRUE)
