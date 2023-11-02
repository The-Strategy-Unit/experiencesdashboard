#' return the data used in the datatable
#'
#' @param data the filter data from app server ex. filter_data()$filter_data
#' @param column_names the names of all the columns to select from the data. Columns not present in the data are ignored
#' @param comment_column name of the comment column. see `clean_dataframe()`
#' @param comment_1 comment_1 value from config file ex. get_golem_config("comment_1")
#' @param id see `get_module_id()`
#' @param session see `get_module_id()`
#' @param comment_2 comment_2 value from config file ex. get_golem_config("comment_2")
#'
#' @return dataframe
#' @noRd
prepare_data_management_data <- function(data, id, session, column_names, comment_column, comment_1, comment_2 = NULL) {
  module_id <- get_module_id(id, session)

  if (isTruthy(comment_2)) {
    return_data <- data %>%
      dplyr::filter(comment_type %in% c("comment_1", "comment_2")) %>%
      dplyr::mutate(
        comment_type = stringr::str_replace_all(comment_type, "comment_1", comment_1),
        comment_type = stringr::str_replace_all(comment_type, "comment_2", comment_2)
      )
  } else {
    return_data <- data %>%
      dplyr::filter(comment_type %in% c("comment_1")) %>%
      dplyr::mutate(
        comment_type = stringr::str_replace_all(comment_type, "comment_1", comment_1)
      )
  }

  return_data %>%
    clean_dataframe(comment_column) %>%
    dplyr::mutate(
      checks = add_checkbox_buttons(comment_id, module_id, flagged, bad_code)
    ) %>%
    dplyr::select(dplyr::any_of(column_names)) %>%
    dplyr::select(
      dplyr::where(~ !all(is.na(.)))
    ) %>% # delete all empty columns
    clean_super_category() %>%
    dplyr::arrange(comment_id) %>%
    dplyr::select(checks, everything())
}

#' remove duplicate values from each super category row
#'
#' @param data dataframe. ex. data from the database
#' @noRd
clean_super_category <- function(data) {
  data %>%
    dplyr::mutate(across(c(category, super_category), ~ purrr::map(.x, jsonlite::fromJSON)),
      super_category = lapply(super_category, unique), # to remove the duplicate values from each super category row
      across(c(category, super_category), ~ purrr::map(.x, to_string)) # format to more user friendly output
    )
}

#' Prepare data for download. Ensures the category and
#' super_category columns are converted to strings
#'
#' @param data dataframe. note: category, super_category columns must be present and in list datatype
#' @return dataframe
#' @noRd
prepare_data_for_download <- function(data) {
  data %>%
    # return the category, super_category columns as string
    dplyr::mutate(
      across(c(category, super_category), ~ sapply(.x, paste0, simplify = TRUE, USE.NAMES = F))
    )
}

#' get_complex_comments
#'
#' @param data a dataframe containing comment_txt column and column assigned to multilabel_column`
#' @param multilabel_column the column holding the labels
#' @param long_words rule 1 for defining complex comments
#' @param many_labels rule 2 for defining complex comment
#' @param data
#'
#' @description A fct function to get the rows of data with complex comments.
#' complex comment is defined as comments with labels more than `many_labels` and words more than `long_words`
#'
#' @return a dataframe
#'
#' @noRd
get_complex_comments <- function(data, multilabel_column = "category", long_words = 50, many_labels = 5) {
  if (!multilabel_column %in% names(data)) stop(paste0("'", multilabel_column, "' not in data"), call. = FALSE)

  data %>%
    dplyr::mutate(
      n_word = lengths(stringr::str_split(comment_txt, " ")),
      n_label = sapply(category, length, simplify = TRUE, USE.NAMES = F)
    ) %>%
    dplyr::filter(n_word > long_words | n_label > many_labels) %>%
    dplyr::select(-n_word, -n_label, -pt_id) %>%
    prepare_data_for_download()
}
