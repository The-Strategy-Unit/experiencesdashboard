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
  
  db_tidy <- data %>% 
    dplyr::rename(category = code) %>% 
    dplyr::mutate(category = dplyr::case_when(
      is.null(comment_txt) ~ NA_character_,
      comment_txt %in% c("NULL", "NA", "N/A") ~ NA_character_,
      TRUE ~ category
    ))
  
  if(trust_id == "trust_c"){
    
    db_tidy <- db_tidy %>%
      dplyr::mutate(age_label = dplyr::case_when(
        age == "Up to 25" ~ "0 - 25",
        TRUE ~ age))
  }

  # Return
  return(db_tidy)
  
}
