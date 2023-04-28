#' nhs_shiny_theme
#'
#' @description Create a shiny theme with NHS blue color
#'
#' @return a string value of the compiled CSS
#'
#' #' @noRd
nhs_shiny_theme <- function() {
  fresh::create_theme(
    fresh::adminlte_color(
      light_blue = "#005EB8"
    ),
    fresh::adminlte_sidebar(
      width = "250px",
      dark_hover_bg = "#41B6E6",
      dark_bg = "#D8DEE9",
    ),
    fresh::adminlte_global(
      content_bg = "#FFF",
    )
  )
}
