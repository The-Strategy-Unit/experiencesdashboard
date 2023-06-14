#' fft UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_fft_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("spc_plot")) %>%
      shinycssloaders::withSpinner()
  )
}

#' fft Server Functions
#'
#' @noRd
mod_fft_server <- function(id, filter_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    graph_data <- reactive({
      split_data_spc(filter_data()$unique_data, variable = "fft", chunks = "monthly")
    }) %>%
      bindCache(filter_data()$unique_data)

    no_group <- reactive({
      graph_data() %>%
        dplyr::pull(1) %>%
        unique() %>%
        length()
    }) %>%
      bindCache(filter_data()$unique_data)

    output$spc_plot <- renderPlot({
      validate(
        need(
          no_group() > 9,
          "There are not enough stable SPC points to plot. Please expand your selection"
        )
      )

      plot_fft_spc(graph_data())
    }) %>%
      bindCache(graph_data())
  })
}
