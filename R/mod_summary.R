#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("dynamic_summary"))
}

#' summary Server Functions
#'
#' @noRd
mod_summary_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$dynamic_summary <- renderUI({
      tagList(
        br(),
        mod_summary_record_ui("summary_record_1"),
        hr(),
        fluidRow(

          # FFT tab (do we have FFT data?)
          if (isTruthy(get_golem_config("question_1"))) {
            column(
              8,
              box(
                mod_fft_ui("fft_ui_1"),
                title = "FFT Score", status = "primary", solidHeader = TRUE, width = NULL
              )
            )
          },

          # Report builder

          column(
            4,
            box(mod_report_builder_ui("report_builder_ui_1"),
              title = "Report builder", status = "primary", solidHeader = TRUE, width = NULL
            )
          )
        ),
      )
    })
  })
}
