# app_server-helpers

#' Get R config Active value from session group data on server
#' @param groups session$group
#' @noRd
set_trust_config <- function(groups) {
  dashboard_groups <- groups |>
    stringr::str_subset("^experiencedashboard-(?!admins)") %>%
    stringr::str_remove("^experiencedashboard-")

  # should we be killing the session if for some reason the user is a member of
  # multiple groups?
  stopifnot("member of multiple groups" = length(dashboard_groups) <= 1)

  if (dashboard_groups == "developers") {
    Sys.getenv("DEFAULT_TRUST")
  } else if (length(dashboard_groups) > 0) {
    dashboard_groups
  } else {
    stop("Not a member of a group")
  }
}

get_location_data <- function(date_filter, select_location_1, select_location_2, select_location_3) {
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

get_demography_data <- function(return_data, select_demography_1, select_demography_2, select_demography_3) {
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

get_tidy_filter_data <- function(return_data, data_exists = FALSE) {
  if (data_exists) {
    return_data <- return_data %>%
      dplyr::mutate(across(c(category, super_category), ~ purrr::map(.x, jsonlite::fromJSON))) %>% # unserialise the category data from json into list
      tidyr::unnest(cols = c(category, super_category)) # Unnest the category and super category columns into rows and columns
  }

  return(return_data)
}

#' create the database connection
get_pool <- function(){
  pool <- odbc::dbConnect(
    drv = odbc::odbc(),
    driver = Sys.getenv("odbc_driver"),
    server = Sys.getenv("HOST_NAME"),
    UID = Sys.getenv("DB_USER"),
    PWD = Sys.getenv("MYSQL_PASSWORD"),
    database = "TEXT_MINING",
    Port = 3306
  )
}

#' get the database data
get_db_data <- function(pool, trust_name){
  dplyr::tbl(
    pool,
    dbplyr::in_schema(
      "TEXT_MINING",
      trust_name
    )
  ) %>%
    tidy_all_trusts()
}