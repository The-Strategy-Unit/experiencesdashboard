#' return the data used in the datatable
#'
#' @param data the filter data from app server ex. filter_data()$filter_data
#' @param comment_1 comment_1 value from config file ex. get_golem_config("comment_1")
#' @param comment_2 comment_2 value from config file ex. get_golem_config("comment_2")
#' @return dataframe
#' @noRd
dm_data <- function(data, column_names, comment_1, comment_2 = NULL){
    
  if (isTruthy(comment_2)) {
    return_data <- data %>%
      dplyr::filter(hidden == 0) %>%
      dplyr::select(-hidden) %>%
      dplyr::select(dplyr::any_of(column_names)) %>%
      dplyr::mutate(
        comment_type = stringr::str_replace_all(comment_type, "comment_1", comment_1),
        comment_type = stringr::str_replace_all(comment_type, "comment_2", comment_2)
      ) %>%
      dplyr::mutate(date = as.character(date)) %>% # required so that date is not filtered out
      dplyr::select_if(~ !(all(is.na(.)) | all(. == ""))) %>% # delete all empty columns
      dplyr::mutate(date = as.Date(date)) %>%
      dplyr::mutate(across(c(category, super_category), ~ purrr::map(.x, jsonlite::fromJSON)),
                    super_category = lapply(super_category, unique), # to remove the duplicate values from each super category row
                    across(c(category, super_category), ~ purrr::map(.x, to_string)) # format to more user friendly output
      )
  } else {
    return_data <- data %>%
      dplyr::filter(hidden == 0) %>%
      dplyr::select(-hidden) %>%
      dplyr::select(dplyr::any_of(column_names)) %>%
      dplyr::mutate(
        comment_type = stringr::str_replace_all(comment_type, "comment_1", comment_1)
      ) %>%
      dplyr::mutate(date = as.character(date)) %>% # required so that date is not filtered out
      dplyr::select_if(~ !(all(is.na(.)) | all(. == ""))) %>% # delete all empty columns
      dplyr::mutate(date = as.Date(date))
  }
  
  return(return_data)
}
