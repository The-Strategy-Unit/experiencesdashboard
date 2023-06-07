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
    uiOutput(ns("dynamic_fft"))
  )
}

#' fft Server Functions
#'
#' @noRd
mod_fft_server <- function(id, filter_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # confirm if there are at least 10 groups (potential data points) in the data before potting
    output$dynamic_fft <- renderUI({
      tryCatch(
        {
          if (no_group() <= 9) {
            fluidRow(
              tags$br(),
              column(12, tags$strong("There are not enough stable SPC points to plot.
                        Please expand your selection"))
            )
          } else {
            plotOutput(ns("spc_plot")) %>%
              shinycssloaders::withSpinner()
          }
        },
        error = function(e) {
          print(e$message)
          paste("Can't display SPC plot. No enough data")
        }
      )
    })

    graph_data <- reactive({
      split_data_spc(filter_data()$unique_data, variable = "fft", chunks = "monthly")
    }) %>%
      bindCache(filter_data()$unique_data)

    no_group <- reactive({
      graph_data() %>%
        dplyr::pull() %>%
        unique() %>%
        length()
    }) %>%
      bindCache(filter_data()$unique_data)

    memoise_plot_fft_spc <- memoise::memoise(plot_fft_spc, cache = session$cache) # create a session-level cacheable version of plot_fft_spc()
    output$spc_plot <- renderPlot({
      req(no_group() > 9)

      memoise_plot_fft_spc(graph_data())
    })
  })
}
