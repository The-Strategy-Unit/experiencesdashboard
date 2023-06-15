#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %||% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- shiny::reactiveValues
rvtl <- shiny::reactiveValuesToList

#' header_links
#' @description A fct to add dropdown icons to dashboard header
#' @noRd
header_links <- function() {
  tags$li(
    tags$li(
      a(
        onclick = "onclick =window.open('https://cdu-data-science-team.github.io/PatientExperience-QDC/')",
        href = NULL,
        icon("book"),
        title = "Go to Project Documentation Website",
        style = "cursor: pointer;"
      ),
      class = "dropdown"
    ),
    tags$li(
      a(
        onclick = "onclick =window.open('mailto:chris.beeley1@nhs.net?cc=oluwasegun.apejoye2@nottshc.nhs.uk')",
        href = NULL,
        icon("envelope", prefer_type = "solid"),
        title = "Contact Project Team",
        style = "cursor: pointer;"
      ),
      class = "dropdown"
    ),
    class = "dropdown"
  )
}

#' get demographic choices
#'
#' @param unique_data dataframe
#' @param demographic_feature string, column name of the demographic feature
#'
#' @return list, unique values in the demographic feature
#' @noRd
get_demographic_choices <- function(unique_data, demographic_feature){
  unique_data %>%
    dplyr::filter(
      !is.na(demographic_feature),
      demographic_feature != "",
      demographic_feature != "NA",
      demographic_feature != "NULL"
    ) %>%
    dplyr::pull(demographic_feature) %>%
    unique() %>%
    sort()
}