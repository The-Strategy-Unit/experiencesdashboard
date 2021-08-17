#' patient_experience UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_patient_experience_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Summary",
        mod_summary_ui("summary_ui_1")
      ),
      tabPanel(
        "Report builder",
        mod_report_builder_ui("report_builder_ui_1")
      ),
      tabPanel(
        "FFT",
        mod_fft_ui("fft_ui_1")
      ),
      tabPanel(
        "Themes/ weighting",
        mod_category_criticality_ui("category_criticality_ui_1")
      ),
      tabPanel(
        "Sentiment",
        mod_sentiment_ui("mod_sentiment_ui_1")
      ),
      tabPanel(
        "Comment search",
        mod_search_text_ui("search_text_ui_1")
      ),
      tabPanel(
        "Demographics",
        mod_demographics_ui("demographics_ui_1")
      )
    )
  )
}
    
#' patient_experience Server Functions
#'
#' @noRd 
mod_patient_experience_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
