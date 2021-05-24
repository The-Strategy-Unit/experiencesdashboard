#' Tidy patient experience data
#'
#' @param data dataframe or SQL object, that you can make with get_px_exp()
#' @param conn connection, that you can make with connect_mysql()- by default
#' this will be done automatically
#' @param trust_id string. Which trust are you tidying data for?
#'
#' @return
#' @export
#'
#' @section Last updated by:
#' Chris Beeley
#' @section Last updated date:
#' 2021-04-25
tidy_all_trusts <- function(data, conn, trust_id = "trust_a") {
  
  data <- data %>%
    dplyr::rename_all(janitor::make_clean_names)
  
  if(trust_id == "trust_a"){
    
    data <- data %>%
      dplyr::rename(comment_1 = improve, comment_2 = best,
                    location_3 = team_n,
                    location_2 = dir_t, location_1 = division2,
                    code1_c1 = imp_n1, code2_c1 = imp_n2,
                    code1_c2 = best_n1, code2_c2 = best_n2,
                    fft = service, positive_q = positive)
    
    score_fields <- c("fft", "listening", "communication", "respect",
                      "inv_care", "positive_q")
  }
  
  if(trust_id %in% c("trust_b", "trust_c")){
    
    data <- data %>%
      dplyr::rename(comment_1 = comment, code1_c1 = code,
                    crit = criticality)
    
    score_fields <- c("fft")
  }
  
  text_fields <- grep("comment_", colnames(data), value = TRUE)
  code_fields <- grep("code", colnames(data), value = TRUE)
  
  # get the codes db connection
  db_codes_exp_data <- dplyr::tbl(
    conn, dbplyr::in_schema("TEXT_MINING", "NewCodes")) %>%
    dplyr::rename_all(janitor::make_clean_names)
  
  # TIDY FUNCTION HERE
  db_tidy <- data %>%
    dplyr::mutate_at(dplyr::all_of(code_fields), toupper) %>%
    dplyr::mutate_at(dplyr::all_of(score_fields), ~ case_when(
      . %in% 0 : 5 ~ .,
      TRUE ~ NA_integer_)) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("comment_"),
                        names_to = "comment_type",
                        values_to = "comment_txt") %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("code"),
                        names_to = c("type_category", "type_num"), names_sep = "_",
                        values_to = "code") %>%
    dplyr::mutate(code = na_if(code, "XX")) %>%
    dplyr::filter(
      str_sub(comment_type, -1) == str_sub(type_category, -1)) %>%
    dplyr::left_join(db_codes_exp_data,
                     by = c("code"), copy = TRUE) %>%
    dplyr::mutate(comment_key = paste0(key, "_", type_category)) %>%
    dplyr::select(colnames(.))
  
  if(trust_id == "trust_a"){
    
    db_tidy <- db_tidy %>%
      dplyr::mutate(crit = case_when(comment_type == "comment_1" ~ imp_crit * -1,
                                     comment_type == "comment_2" ~ best_crit)) %>% 
      dplyr::mutate(age_label = dplyr::case_when(
        age == 1 ~ "Under 12",
        age == 2 ~ "12-17",
        age == 3 ~ "18-25",
        age == 4 ~ "26-39",
        age == 5 ~ "40-64",
        age == 6 ~ "65-79",
        age == 7 ~ "80+",
        TRUE ~ NA_character_
      )) %>% 
      dplyr::rename(ethnicity = ethnic) %>% 
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
  } else {
    
    db_tidy <- db_tidy %>%
      dplyr::mutate(age_label = dplyr::case_when(
        age == "Up to 25" ~ "0 - 25",
        TRUE ~ age))
  }
  
  db_tidy <- db_tidy %>%
    dplyr::mutate(crit = dplyr::case_when(
      crit %in% -5 : 5 ~ crit,
      TRUE ~ NA_integer_
    ))
  
  # Return
  return(db_tidy)
  
}
