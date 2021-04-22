
# load, clean, and upload other data----

library(readxl)
library(tidyverse)

con <- nottshc::connect_mysql(open_data = TRUE)

new_codes <- dplyr::tbl(con, dbplyr::in_schema("SUCE", "NewCodes")) %>%
  dplyr::rename_all(janitor::make_clean_names) %>% 
  collect() %>% 
  mutate(code = tolower(code))

# function to produce probabilities for sampling

make_prob <- function(values){
  
  values / sum(values)
}

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
    across(contains("code"), ~ tolower(str_trim(.))),
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
  mutate(fft = sample(c(NA, 1 : 5), n(), replace = TRUE, 
                      prob = make_prob(c(1, 2, 3, 4, 7, 9))))

# trust c----

trust_c <- read_xlsx("data-raw/TrustC.xlsx") %>%
  select(location_1 = Directorate, location_2 = Specialty, 
         location_3 = LocationName, date = InputDate,
         comment = Comment, code = `Code 1`,
         criticality = Serverity) %>% 
  mutate(
    criticality = as.numeric(criticality),
    code = tolower(code)) %>%
  mutate(fft = sample(c(NA, 1 : 5), n(), replace = TRUE, 
                      prob = make_prob(c(1, 2, 3, 4, 7, 9))),
         criticality = case_when(
           criticality > 5 ~ NA_real_,
           criticality < -5 ~ NA_real_,
           TRUE ~ criticality
         )) %>% 
  mutate(across(starts_with("location"), ~ nottshcMethods::hash(.x, n_char = 8))) %>% 
  left_join(new_codes, by = "code")

# demographics----

# gender

c("Male", "Female", "Other")

# age

c("Up to 25", "26 - 35", "36 - 45", "46 - 55", "56 - 65", "65-74", 
  "75-84", "85-94", "95 plus")

# ethnicity

c("Asian", "Black / African / Caribbean", "Mixed / Multiple ethnic group", 
  "Other ethnic group", "White", "Not recorded")

# sexuality

c("Bisexual", "Gay man", "Gay woman / lesbian", "Heterosexual", 
  "Heterosexual / straight", "Other", "Prefer not to say", 
  "Prefer to self-describe:", "Not recorded")

# patient/ carer

c("Carer", "Parent", "Patient", "Not recorded")

# disability

c("No", "Not limited", "Yes, limited a little", "Yes, limited a lot", 
  "Prefer not to say", "Not recorded")
