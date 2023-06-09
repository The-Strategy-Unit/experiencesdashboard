#' app_server-helpers 

set_trust_config <- function(groups = NULL, trust_name = NULL) {
  
  if(is.null(groups)){
    stopifnot("trust_name can't be null if group is null" = !is.null(trust_name))
    return(trust_name)
  }
  
  dashboard_groups <- groups |>
    stringr::str_subset("^experiencedashboard-(?!admins)")
  # should we be killing the session if for some reason the user is a member of
  # multiple groups?
  stopifnot("member of multiple groups" = length(dashboard_groups) <= 1)
  
  if (length(dashboard_groups) > 0) {
    stringr::str_remove(dashboard_groups, "^experiencedashboard-")
  } else {
    Sys.getenv("DEFAULT_TRUST")
  }
}

get_location_data <- function(date_filter, select_location_1, select_location_2, select_location_3){
  
  return_data <- date_filter
  
  if (isTruthy(select_location_1)) {
    return_data <- return_data %>%
      dplyr::filter(location_1 %in% !!select_location_1)
  }
  
  if (isTruthy(select_location_2)) {
    return_data <- return_data %>%
      dplyr::filter(location_2 %in% !!select_location_2)
  }
  
  if (isTruthy(select_location_3)) {
    return_data <- return_data %>%
      dplyr::filter(location_3 %in% !!select_location_3)
  }
  
  return(return_data)
}

get_demography_data <- function(return_data, select_demography_1, select_demography_2, select_demography_3){
  
  demography_data <- return_data
  
  if (isTruthy(select_demography_1)) {
    demography_data <- demography_data %>%
      dplyr::filter(!!rlang::sym(get_golem_config("demography_1")) %in% !!select_demography_1)
  }
  
  if (isTruthy(select_demography_2)) {
    demography_data <- demography_data %>%
      dplyr::filter(!!rlang::sym(get_golem_config("demography_2")) %in% !!select_demography_2)
  }
  
  if (isTruthy(select_demography_3)) {
    demography_data <- demography_data %>%
      dplyr::filter(!!rlang::sym(get_golem_config("demography_3")) %in% !!select_demography_3)
  }
  
  return(demography_data)
}

get_tidy_filter_data <- function(return_data, data_exists = FALSE){
  
  if (data_exists) {
    return_data <- return_data %>%
      dplyr::mutate(across(c(category, super_category), ~ purrr::map(.x, jsonlite::fromJSON))) %>% # unserialise the category data from json into list
      tidyr::unnest(cols = c(category, super_category)) # Unnest the category and super category columns into rows and columns
  } 
  
  return(return_data)
}
