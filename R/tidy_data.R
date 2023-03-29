#' Tidy patient experience data
#'
#' @param data dataframe or SQL object, that you can make with get_px_exp()
#'
#' @return
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
        hidden == 0
      )
  } else {
    data
  }
}
