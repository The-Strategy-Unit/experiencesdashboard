
#' Internal function for preparing data for the `comment_table()`function
#'
#' @param comment_data a dataframe
#' @param in_tidy_format boolean if the data was in single labeled (or multilabeled format)
#' @return a formatted datatable
#' @noRd
prep_data_for_comment_table <- function(comment_data, in_tidy_format = TRUE) {
  data <- comment_data
  
  if (in_tidy_format) {
    data <- data %>%
      single_to_multi_label()
  }
  
  stopifnot("values in 'comment ID' should be unique. Did you forget to set `in_tidy_format = TRUE`?" = data$comment_id %>% duplicated() %>% sum() == 0)
  
  data <- data %>%
    dplyr::select(date, comment_type, fft, sentiment, comment_txt, category, super_category) %>%
    dplyr::mutate(
      dplyr::across(c(category, super_category), ~ sapply(.x, paste0, simplify = TRUE, USE.NAMES = FALSE))
    ) %>%
    dplyr::mutate(
      comment_type = stringr::str_replace_all(comment_type, "comment_1", get_golem_config("comment_1"))
    ) %>%
    dplyr::arrange(date)
  
  if (isTruthy(get_golem_config("comment_2"))) {
    data <- data %>%
      dplyr::mutate(
        comment_type = stringr::str_replace_all(comment_type, "comment_2", get_golem_config("comment_2"))
      )
  }
  
  colnames(data) <- c(
    "Date", "FFT Question", "FFT Score", "Comment Sentiment",
    "FFT Answer", "Sub-Category", "Category"
  )
  
  cat("Rows in comment table:", nrow(data), " \n") # for debugging
  
  return(data)
}

#' Internal function for the comment datatable
#'
#' @param data a dataframe
#' @return a formatted datatable
#'
#' @noRd
comment_table <- function(data) {
  # add NHS blue color to the table header
  initcomplete <- DT::JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#005EB8', 'color': '#fff'});",
    "}"
  )
  
  return(
    DT::datatable(
      data,
      extensions = "Buttons", # required to show the download buttons and groups
      options = list(
        dom = "ipt",
        buttons = c("csv", "excel", "pdf"),
        # autoWidth = TRUE,            # required for width option for columns to  work
        # columnDefs = list(list(width = '500px', targets = c(3))),
        initComplete = initcomplete,
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
