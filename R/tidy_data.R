#' Tidy patient experience data
#'
#' @param data dataframe or SQL object with comment column, comment_txt
#' @export
tidy_all_trusts <- function(data) {
  # this line only works if there is data in the table

  if (data %>%
    dplyr::tally() %>%
    dplyr::pull(n) > 0) {
    data %>%
      dplyr::filter(
        !is.na(comment_txt),
        !is.null(comment_txt),
        comment_txt != "NA",
        comment_txt != "NULL",
        comment_txt != "",
        comment_txt != " ",
        hidden == 0
      )
  } else {
    data
  }
}
