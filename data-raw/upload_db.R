
# prepare environment----

library(readxl)
library(tidyverse)
library(DBI)
library(nottshc)
library(nottshcMethods)

# data prep----

con <- nottshc::connect_mysql(open_data = FALSE)

new_codes <- dplyr::tbl(con, dbplyr::in_schema("SUCE", "NewCodes")) %>%
  dplyr::rename_all(janitor::make_clean_names) %>% 
  collect() %>% 
  mutate(code = toupper(code))

# trust a----

trust_a <- get_px_exp(conn = con, 
                      from = "2020-10-01",
                      open_data = FALSE, 
                      remove_demographics = FALSE,
                      remove_optout = TRUE)

# trust b----

trust_b <- read_excel(
  "data-raw/TrustB.xls",
  sheet = "Completed Comments",
  col_names = FALSE,
  col_types = rep("text", 16)) %>%
  rename_with(~ c("location_1", "location_2", "location_3",
                  "patient", "carer", "fft", "comment", "blank_1", 
                  "code1_neg", "code_2", "crit_neg", "blank_2",
                  "code1_pos", "code_4", "crit_pos", "blank_3")) %>% 
  mutate(
    across(contains("code"), ~ toupper(str_trim(.))),
    across(
      contains("code"), ~ 
        case_when(
          is.na(.) | . %in% "" ~ "xx", 
          TRUE ~ .
        )
    ),
    code = 
      case_when(
        !code1_neg %in% "xx" & code1_pos %in% "xx" ~ code1_neg,
        code1_neg %in% "xx" & !code1_pos %in% "xx" ~ code1_pos,
        !code1_neg %in% "xx" & !code1_pos %in% "xx" ~ 
          case_when(
            crit_neg > crit_pos ~ code1_neg,
            crit_neg < crit_pos ~ code1_pos,
            TRUE ~ sample(c(crit_neg, crit_pos), 1)
          )
      )) %>% 
  mutate(across(contains("crit"), ~ as.numeric(.))) %>% 
  mutate(criticality = 
           case_when(
             code == code1_neg ~ crit_neg * -1,
             TRUE ~ crit_pos
           ),
         criticality = case_when(
           criticality > 5 ~ NA_real_,
           criticality < -5 ~ NA_real_,
           TRUE ~ criticality
         ),
         criticality = case_when(
           code == "xn" ~ abs(criticality),
           TRUE ~ criticality
         )) %>% 
  filter(code %in% new_codes$code) %>% 
  mutate(patient_carer = coalesce(patient, carer)) %>% 
  select(location_1 : location_3, fft, comment, code, criticality, patient_carer) %>% 
  left_join(new_codes, by = "code") %>% 
  mutate(across(starts_with("location"), ~ nottshcMethods::hash(.x, n_char = 8))) %>% 
  mutate(fft = sample_vector(values = c(NA, 1 : 5), weights = c(1, 2, 3, 4, 7, 9), 
                             dplyr::n())) %>% 
  mutate(date = sample( # fake dates
    seq.Date(as.Date("2020-10-01"), Sys.Date(), "day"), 
    n(), replace = TRUE))

# trust c----

trust_c <- read_xlsx("data-raw/TrustC.xlsx") %>%
  select(location_1 = Directorate, location_2 = Specialty, 
         location_3 = LocationName, date = InputDate,
         comment = Comment, code = `Code 1`,
         criticality = Serverity) %>% 
  mutate(
    criticality = as.numeric(criticality),
    code = tolower(code)) %>%
  mutate(fft = sample_vector(values = c(NA, 1 : 5), 
                             weights = c(1, 2, 3, 4, 7, 9), 
                             length = dplyr::n()),
         criticality = case_when(
           criticality > 5 ~ NA_real_,
           criticality < -5 ~ NA_real_,
           TRUE ~ criticality
         )) %>% 
  mutate(across(starts_with("location"), ~ nottshcMethods::hash(.x, n_char = 8))) %>% 
  left_join(new_codes, by = "code") %>% 
  mutate(date = sample( # fake dates
    seq.Date(as.Date("2020-10-01"), Sys.Date(), "day"), 
    n(), replace = TRUE))

# demographics----

trust_b <- trust_b %>% 
  mutate(gender = sample_vector(values = c(NA, "Male", "Female", "Other"),
                                weights = c(10, 50, 50, 2),
                                length = dplyr::n())) %>% 
  mutate(age = sample_vector(values = c("Up to 25", "26 - 35", "36 - 45", "46 - 55", 
                                        "56 - 65", "65-74",  "75-84", "85-94", "95 plus"),
                             weights = c(9 : 1),
                             length = dplyr::n())) %>% 
  mutate(ethnicity = sample_vector(values = c("Asian", "Black / African / Caribbean", 
                                              "Mixed / Multiple ethnic group", 
                                              "Other ethnic group", "White", 
                                              "Not recorded"),
                                   weights = c(4, 2, 4, 1, 15, 8),
                                   length = dplyr::n())) %>% 
  mutate(sexuality = sample_vector(values = c("Bisexual", "Gay man", "Gay woman / lesbian", 
                                              "Heterosexual", "Heterosexual / straight", 
                                              "Other", "Prefer not to say", 
                                              "Prefer to self-describe:", "Not recorded"),
                                   weights = c(2, 2, 2, 7, 9, 1, 5, 2, 8),
                                   length = dplyr::n())) %>% 
  mutate(patient_carer = sample_vector(values = c("Carer", "Parent", "Patient", 
                                                  "Not recorded"),
                                       weights = c(5, 3, 20, 10),
                                       length = dplyr::n())) %>% 
  mutate(disability = sample_vector(values = c("No", "Not limited", "Yes, limited a little", 
                                               "Yes, limited a lot", 
                                               "Prefer not to say", "Not recorded"),
                                    weights = c(20, 20, 10, 5, 5, 10),
                                    length = dplyr::n()))

trust_c <- trust_c %>% 
  mutate(gender = sample_vector(values = c(NA, "Male", "Female", "Other"),
                                weights = c(10, 50, 50, 2),
                                length = dplyr::n())) %>% 
  mutate(age = sample_vector(values = c("Up to 25", "26 - 35", "36 - 45", "46 - 55", 
                                        "56 - 65", "65-74",  "75-84", "85-94", "95 plus"),
                             weights = c(9 : 1),
                             length = dplyr::n())) %>% 
  mutate(ethnicity = sample_vector(values = c("Asian", "Black / African / Caribbean", 
                                              "Mixed / Multiple ethnic group", 
                                              "Other ethnic group", "White", 
                                              "Not recorded"),
                                   weights = c(4, 2, 4, 1, 15, 8),
                                   length = dplyr::n())) %>% 
  mutate(sexuality = sample_vector(values = c("Bisexual", "Gay man", "Gay woman / lesbian", 
                                              "Heterosexual", "Heterosexual / straight", 
                                              "Other", "Prefer not to say", 
                                              "Prefer to self-describe:", "Not recorded"),
                                   weights = c(2, 2, 2, 7, 9, 1, 5, 2, 8),
                                   length = dplyr::n())) %>% 
  mutate(patient_carer = sample_vector(values = c("Carer", "Parent", "Patient", 
                                                  "Not recorded"),
                                       weights = c(5, 3, 20, 10),
                                       length = dplyr::n())) %>% 
  mutate(disability = sample_vector(values = c("No", "Not limited", "Yes, limited a little", 
                                               "Yes, limited a lot", 
                                               "Prefer not to say", "Not recorded"),
                                    weights = c(20, 20, 10, 5, 5, 10),
                                    length = dplyr::n()))

# tidy----

tidy_px_exp <- function(data, conn = conn_mysql_suce, trust_id = "trust_a") {
  
  data <- data %>%
    dplyr::rename_all(janitor::make_clean_names)
  
  if(trust_id == "trust_a"){
    
    code_fields <- c("imp_n1", "imp_n2", "best_n1", "best_n2")
    score_fields <- c("service", "listening", "communication", "respect", 
                      "inv_care", "positive")
    
    # data <- data %>% 
    #   dplyr::rename(comment_1 = improve,
    #                 comment_2 = best,
    #                 crit_1 = imp_crit,
    #                 crit_2 = best_crit,
    #                 location_1 = team_n,
    #                 location_2 = dir_t, 
    #                 location_3 = division2) %>% 
    #   dplyr::mutate(crit_1 = crit_1 * -1)
    
  } else {
    
    if(trust_id == "trust_b"){
      
      text_fields <- c("comment")
      code_fields <- c("code")
    }
    
  }
  
  # get the codes db connection
  db_codes_exp_data <- dplyr::tbl(conn, dbplyr::in_schema("SUCE", "NewCodes")) %>%
    dplyr::rename_all(janitor::make_clean_names)
  
  db_tidy <- data %>%
    # dplyr::mutate(dplyr::across(dplyr::all_of(c("imp_n1", "imp_n2",
    #                                             "best_n1", "best_n2")), toupper)) %>% 
    # dplyr::mutate(dplyr::across(all_of(score_fields), ~ case_when(
    #   . %in% 0 : 5 ~ .,
    #   TRUE ~ NA_integer_))) %>% 
    # dplyr::select(date, time, su, service, communication,
    #               comment_1,
    #               comment_2,
    #               imp_n1,
    #               imp_n2,
    #               best_n1,
    #               best_n2,
    #               crit_1, crit_2,
    #               location_1, location_2, location_3) %>% 
    # tidyr::pivot_longer(cols = dplyr::all_of(c("improve", "best")),
    #                     names_to = "fish_type",
    #                     values_to = "rhino_txt") %>% 
    tidyr::pivot_longer(cols = dplyr::all_of(code_fields),
                        names_to = c("type_category", "type_num"), names_sep = "_",
                        values_to = "code") %>% collect()
  #   tidyr::pivot_longer(cols = dplyr::contains("code"),
  #                       names_to = c("type_category", "type_num"), names_sep = "_",
  #                       values_to = "code")
  # 
  # dplyr::mutate(code = na_if(code, "XX")) %>%
  #   dplyr::filter(comment_type == type_category) %>%
  #   dplyr::select(-c(imp_crit, best_crit)) %>%
  #   dplyr::mutate(crit = dplyr::case_when(
  #     crit %in% -5 : 5 ~ crit,
  #     TRUE ~ NA_integer_
  #   )) %>%
  #   dplyr::left_join(db_codes_exp_data,
  #                    by = c("code")) %>%
  #   dplyr::mutate(comment_key = paste0(key, "_", type_category)) %>%
  #   dplyr::select(key, comment_key, date, team_c, team_n,
  #                 directorate, division,
  #                 su, service : respect, inv_care, positive,
  #                 category = category,
  #                 subcategory = subcategory,
  #                 comment_type : crit,
  #                 any_of(c("optout", "gender", "ethnic", "disability",
  #                          "religion", "sexuality", "age", "relationship",
  #                          "pregnant", "baby")))
  # 
  # # Return
  # return(db_tidy)
  
}

data <- get_px_exp(from = "2020-10-01",
                   open_data = FALSE, 
                   remove_demographics = FALSE,
                   remove_optout = TRUE)

tidy_trust_a <- tidy_px_exp(data, conn = con, trust_id = "trust_a") %>% 
  collect()

# MUST randomise the demographic features

tidy_trust_a <- tidy_trust_a %>% 
  mutate(across(gender : baby, ~ sample(.x, n())))

# tidy_trust_b <- tidy_px_exp(trust_b, conn = con, trust_id = "trust_b")

# con <- DBI::dbConnect(odbc::odbc(),
#                       Driver   = "Maria DB",
#                       Server   = Sys.getenv("HOST_NAME"),
#                       UID      = Sys.getenv("DB_USER"),
#                       PWD      = Sys.getenv("MYSQL_PASSWORD"),
#                       Port     = 3306,
#                       database = "TEXT_MINING",
#                       encoding = "UTF-8")
# 
# dbWriteTable(con, 'trust_b', trust_b)
# 
# dbDisconnect(con)

# usethis::use_data(trust_c)
