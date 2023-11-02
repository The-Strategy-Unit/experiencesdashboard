#' Internal function for preparing data for the `render_comment_table()`function
#'
#' @param comment_data a dataframe
#' @param in_tidy_format boolean if the data was in single labeled (or multilabeled format)
#' @return a formatted datatable
#' @noRd
prep_data_for_comment_table <- function(comment_data, in_tidy_format = TRUE) {
  if (in_tidy_format) {
    comment_data <- comment_data %>%
      single_to_multi_label()
  }

  stopifnot("values in 'comment ID' should be unique. Did you forget to set `in_tidy_format = TRUE`?" = comment_data$comment_id %>% duplicated() %>% sum() == 0)

  # Select the important column and format the "category", "super_category", and "comment_type" to be more user friendly
  comment_data <- comment_data %>%
    dplyr::select(date, comment_type, fft, sentiment, comment_txt, category, super_category) %>%
    dplyr::mutate(
      dplyr::across(c(category, super_category), ~ sapply(.x, paste0, simplify = TRUE, USE.NAMES = FALSE))
    ) %>%
    dplyr::mutate(
      comment_type = stringr::str_replace_all(comment_type, "comment_1", get_golem_config("comment_1"))
    ) %>%
    dplyr::arrange(date)

  # confirm that the trust provided optional 2nd comment type before formating its values
  if (isTruthy(get_golem_config("comment_2"))) {
    comment_data <- comment_data %>%
      dplyr::mutate(
        comment_type = stringr::str_replace_all(comment_type, "comment_2", get_golem_config("comment_2"))
      )
  }

  # rename the column name to be more user friendly
  colnames(comment_data) <- c(
    "Date", "FFT Question", "FFT Score", "Comment Sentiment",
    "FFT Answer", "Sub-Category", "Category"
  )

  cat("Rows in comment table:", nrow(comment_data), " \n") # for debugging

  return(comment_data)
}

#' Internal function for the comment datatable
#'
#' @param data a dataframe
#' @return a formatted datatable
#'
#' @noRd
render_comment_table <- function(data) {
  return(
    DT::datatable(
      data,
      options = list(
        dom = "ipt",
        columnDefs = list(list(width = "500px", targets = c(4))), # ensure the comment column is wider on bigger screen
        initComplete = dt_nhs_header(),
        pageLength = 50,
        scrollX = TRUE,
        selection = "single"
      ),
      filter = "top",
      rownames = FALSE,
      class = "display cell-border compact stripe",
    )
  )
}
