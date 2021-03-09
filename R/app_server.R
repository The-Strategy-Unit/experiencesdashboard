#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  # render the inputs
  
  output$filter_data <- renderUI({
    
    divisions <- na.omit(unique(tidy_trust_data$division))
    
    max_date <- max(tidy_trust_data$date)
    
    tagList(
      dateRangeInput(
        "date_range",
        label = h5(strong("Select date range:")),
        start = "2020-10-01"
      ),
      selectInput(
        "select_division",
        label = h5(strong("Select divisions:")),
        choices = divisions,
        multiple = TRUE,
        selected = sample(divisions, 1)
      )
    )
  })
  
  all_inputs <- reactive(
    list("date_range" = input$date_range,
         "select_division" = input$select_division)
  )
  
  mod_patient_experience_server("patient_experience_ui_1")
  
  mod_sentiment_server("mod_sentiment_ui_1")
  
  mod_category_criticality_server("category_criticality_ui_1", all_inputs = all_inputs)
}
