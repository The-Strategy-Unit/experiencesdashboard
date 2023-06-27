#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#'  html decoder fo single string object
#'
#' @param str a string to be decoded
#'
#' @return decoded string
#' @export
#'
#' @examples html_decoder("&amp; &gt;")
html_decoder <- function(str) {
  html <- paste0("<x>", str, "</x>")
  parsed <- xml2::xml_text(xml2::read_html(html))
}

#' Promise aware Pipe operators
#'
#' @name promise_pipe
#' @rdname promise_pipe
#' @importFrom promises %...>% %...!%
#' @keywords internal
#' @noRd
#' @usage lhs %...>% rhs
NULL
