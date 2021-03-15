#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  # render the inputs
  
  output$filter_dataUI <- renderUI({
    
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
  
  all_inputs <- reactive({
    list(
      "date_from" = input$date_range[1],
      "date_to" = input$date_range[2],
      "division" = input$select_division
    )
  })
  
  # Create reactive data ----
  filter_data <- reactive({
    
    tidy_trust_data %>%
      dplyr::filter(date > input$date_range[1], 
                    date < input$date_range[2]) %>%
      dplyr::filter(division %in% input$select_division)
  })
  
  filter_sentiment <- reactive({
    
    sentiment_txt_data %>%
      dplyr::filter(date > input$date_range[1], 
                    date < input$date_range[2]) %>%
      dplyr::filter(division %in% input$select_division)
  })

  mod_patient_experience_server("patient_experience_ui_1")
  
  mod_sentiment_server("mod_sentiment_ui_1", filter_sentiment = filter_sentiment)
  
  mod_category_criticality_server("category_criticality_ui_1", 
                                  filter_data = filter_data)
  
  mod_fft_server("fft_ui_1", filter_data = filter_data)
  
  mod_report_builder_server("report_builder_ui_1",
                            filter_sentiment = filter_sentiment,
                            filter_data = filter_data,
                            all_inputs = all_inputs)
  
  mod_click_tables_server("click_tables_ui_1",
                          filter_data = filter_data,
                          comment_type = "improve")
  
  mod_click_tables_server("click_tables_ui_2",
                          filter_data = filter_data,
                          comment_type = "best")
}
