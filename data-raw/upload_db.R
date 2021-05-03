
# upload other trusts' data to the TEXT_MINING database

# prepare environment----

library(readxl)
library(tidyverse)
library(DBI)
library(nottshcMethods)

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
  mutate(patient_carer = coalesce(patient, carer)) %>% 
  select(location_1 : location_3, fft, comment, code, criticality, patient_carer) %>% 
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
                                    length = dplyr::n())) %>% 
  mutate(date = sample(seq.Date(as.Date("2020-10-01"), Sys.Date(), by = "day"),
                       dplyr::n(), replace = TRUE))

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
                                    length = dplyr::n())) %>% 
  mutate(date = sample(seq.Date(as.Date("2020-10-01"), Sys.Date(), by = "day"),
                       dplyr::n(), replace = TRUE))

con <- DBI::dbConnect(odbc::odbc(),
                      Driver   = "Maria DB",
                      Server   = Sys.getenv("HOST_NAME"),
                      UID      = Sys.getenv("DB_USER"),
                      PWD      = Sys.getenv("MYSQL_PASSWORD"),
                      Port     = 3306,
                      database = "TEXT_MINING",
                      encoding = "UTF-8")

DBI::dbWriteTable(con, 'trust_b', trust_b, overwrite = TRUE)

dbWriteTable(con, 'trust_c', trust_c, overwrite = TRUE)

dbDisconnect(con)
