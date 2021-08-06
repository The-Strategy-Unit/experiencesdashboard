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
tidy_all_trusts <- function(data, conn, trust_id) {
  
  if(trust_id == "demo_trust"){

    return(data %>% 
             dplyr::rename(category = code)) %>% 
      dplyr::mutate(category = dplyr::case_when(
        is.null(comment_txt) ~ NA,
        comment_txt %in% c("NULL", "NA", "N/A") ~ NA,
        TRUE ~ category
      ))
  }
  
  if(trust_id == "trust_a"){
    
    db_tidy <- data %>% 
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
    
    db_tidy <- data %>%
      dplyr::mutate(age_label = dplyr::case_when(
        age == "Up to 25" ~ "0 - 25",
        TRUE ~ age)) %>% 
      dplyr::rename(category = code)
  }
  
  db_tidy <- db_tidy %>% 
    dplyr::mutate(category = dplyr::case_when(
      is.null(comment_txt) ~ NA,
      comment_txt %in% c("NULL", "NA", "N/A") ~ NA,
      TRUE ~ category
    ))
  
  # Return
  return(db_tidy)
  
}
