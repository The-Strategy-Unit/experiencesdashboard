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
          h1("Overview"),
          mod_summary_record_ui("summary_record_1"),
          
          tags$hr(),
          
          fluidRow(
            
            # Report builder
            
            column(4, mod_report_builder_ui("report_builder_ui_1")),
            
            # FFT tab (do we have FFT data?)
            
            if(isTruthy(get_golem_config("question_1"))){
              column(8, mod_fft_ui("fft_ui_1"))
            } else{
                column(8)
            }
          ),
          
          tags$hr()
        )
      })
  })
}
