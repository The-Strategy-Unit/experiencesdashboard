#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  all_inputs <- reactive(
    list("date_range" = input$date_range,
         "select_division" = input$select_division)
  )
  
  mod_patient_experience_server("patient_experience_ui_1")
  
  mod_sentiment_server("mod_sentiment_ui_1")
  
  mod_category_criticality_server("category_criticality_ui_1", all_inputs = all_inputs)
}
