#' plot_sentiment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sentiment_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    wellPanel(
      fluidRow(
        
        column(4,
               dateRangeInput(ns("date_range"),
                              label = h5("Select Date Range:"),
                              start = "2009-04-01", 
                              end = "2019-02-11")
        ),
        column(4,
               selectInput(ns("select_super"), 
                           label = h5("Select Division"),
                           choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                           selected = 1, 
                           multiple = TRUE)
        ),
        column(4,
               selectInput(ns("select_directorate"), 
                           label = h5("Select Division"),
                           choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                           multiple = TRUE,
                           selected = 1)
        )),
        style = "padding: 5px;"),
      
      fluidRow(
        column(12,
               reactable::reactableOutput(ns("table"))
        )
        
      )
    )
  
}

#' plot_sentiment Server Function
#'
#' @noRd 
mod_sentiment_server <- function(id){
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    # Fist tidy entire data for upset plot
    sentiment_txt_data_upset <- sentiment_txt_data %>% 
      dplyr::mutate(date = lubridate::date(date),
                    id = 1:nrow(sentiment_txt_data),
                    all_sentimtents_unnest = all_sentiments) %>% 
      dplyr::select(id, date, super, division2, improve, all_sentiments, polarity, all_sentimtents_unnest) %>% 
      tidyr::unnest(cols = all_sentimtents_unnest) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(value = TRUE) %>% 
      tidyr::pivot_wider(id_cols = c("id", "date", "super", "division2", "improve", "all_sentiments", "polarity"), 
                         names_from = all_sentimtents_unnest, 
                         values_from = value) %>% 
      janitor::clean_names() %>% 
      dplyr::select(-na) %>%
      tidyr::replace_na(list(trust = FALSE, 
                             anticipation = FALSE, 
                             positive = FALSE, 
                             negative = FALSE, 
                             sadness = FALSE, 
                             fear = FALSE, 
                             joy = FALSE,
                             anger = FALSE, 
                             disgust = FALSE, 
                             surprise = FALSE)
      ) %>% 
      dplyr::mutate_if(is.logical, as.numeric)
    
    sentiment_txt_data_r <- reactive( {
      
      sentiment_txt_data_upset %>% 
        dplyr::filter(date > input$date_range[1], date < input$date_range[2])
      
    })
    
    output$table <- reactable::renderReactable({
      
      reactable::reactable(sentiment_txt_data_r()[, c("improve", "super", "division2")],
                           pagination = FALSE,
                           bordered = TRUE,
                           highlight = TRUE, 
                           height = 500,
                           columns = list(
                             improve = reactable::colDef(minWidth = 200),   # 50% width, 200px minimum
                             super = reactable::colDef(minWidth = 50),   # 25% width, 100px minimum
                             division2 = reactable::colDef(minWidth = 50)  # 25% width, 100px minimum
                           ),)
      
    })
    
  })
  
}

## To be copied in the UI
# mod_sentiment_ui("plot_sentiment_ui_1")

## To be copied in the server
# mod_sentiment_server("plot_sentiment_ui_1")

