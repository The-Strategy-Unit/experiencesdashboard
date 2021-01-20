#' mod_sentiment UI Function
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
        
        column(3,
               dateRangeInput(ns("date_range"),
                              label = h5("Date Range:"),
                              start = "2009-04-01", 
                              end = "2019-02-11")
        ),
        
        column(3,
               selectInput(ns("select_division"), 
                           label = h5("Division:"),
                           choices = list("Local partnerships- MH" = "Local partnerships- MH",
                                          "Forensic services" = "Forensic services",
                                          "Local partnerships- CH" = "Local partnerships- CH"),
                           multiple = TRUE,
                           selected = c("Local partnerships- MH", 
                                        "Forensic services",
                                        "Local partnerships- CH"))
        ),
        
        column(3,
               selectInput(ns("select_super"), 
                           label = h5("Tag:"),
                           choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                           selected = 1, 
                           multiple = TRUE)
        ),
        
        column(3,
               selectInput(ns("select_sentiment"), 
                           label = h5("Sentiment:"),
                           choices = list("anger" = "anger",
                                          "anticipation" = "anticipation",
                                          "disgust" = "disgust",
                                          "fear" = "fear",
                                          "joy" = "joy",
                                          "negative" = "negative",
                                          "positive" = "positive",
                                          "sadness" = "sadness",
                                          "surprise" = "surprise",
                                          "trust" = "trust"),
                           multiple = TRUE,
                           selected = "positive")
        )
      ),
      style = "padding: 5px;"),
    
    tabsetPanel(type = "tabs",
                tabPanel("Plot",
                         
                         fluidRow(
                           column(12,
                                  plotOutput(ns("sentiment_plot"))
                           )
                         )
                ),
                
                tabPanel("Comments",
                         
                         fluidRow(
                           column(12,
                                  reactable::reactableOutput(ns("sentiment_table"))
                           )

                         )
                         )
                )
  )
  
}

#' mod_sentiment Server Function
#'
#' @noRd 
mod_sentiment_server <- function(id){
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    # Fist, tidy entire data for upset plot
    sentiment_txt_data_upset <- sentiment_txt_data %>% 
      dplyr::mutate(date = lubridate::date(date),
                    year = lubridate::year(date),
                    id = 1:nrow(sentiment_txt_data),
                    all_sentimtents_unnest = all_sentiments) %>% 
      dplyr::select(id, date, year, super, division2, improve, all_sentiments, polarity, all_sentimtents_unnest) %>% 
      tidyr::unnest(cols = all_sentimtents_unnest) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(value = TRUE) %>% 
      tidyr::pivot_wider(id_cols = c("id", "date", "year", "super", "division2", "improve", "all_sentiments", "polarity"), 
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
    
    
    # Create reactive dataframe ----
    sentiment_txt_data_r <- reactive( {
      
      sentiment_txt_data_upset %>% 
        dplyr::filter(date > input$date_range[1], date < input$date_range[2]) %>% 
        dplyr::filter(division2 %in% input$select_division)
      
    })
    

    
    # Create upset plot ----
    output$sentiment_plot <- renderPlot(
      
        
        UpSetR::upset(data = as.data.frame(sentiment_txt_data_r()[, c("year", "anger", "anticipation", "disgust", "fear", "joy", "negative", "positive", "sadness", "surprise", "trust")]), 
                      nintersects = 25,
                      sets = c("anger", "anticipation", "disgust", "fear", "joy", "negative", "positive", "sadness", "surprise", "trust"),
                      order.by = "freq",
                      text.scale = 1.5,
                      queries = list(list(query = UpSetR::intersects,
                                          params = list(c(input$select_sentiment)),
                                          color = "orange",
                                          active = F)),
                      # attribute.plots = list(gridrows = 50,
                      #                        plots = list(list(plot = UpSetR::histogram,
                      #                                          x = "year",
                      #                                          queries = T)),
                                             # ncols = 1)
                      
        )
      
    )
    
    
    # Create reactive table ----
    output$sentiment_table <- reactable::renderReactable({
      
      reactable::reactable(sentiment_txt_data_r()[, c("improve", "super", "division2")],
                           # pagination = FALSE
                           # height = 500
                           # bordered = TRUE,
                           borderless = TRUE,
                           highlight = TRUE,
                           
                           columns = list(
                             improve = reactable::colDef(minWidth = 200),   # 50% width, 200px minimum
                             super = reactable::colDef(minWidth = 50),   # 25% width, 100px minimum
                             division2 = reactable::colDef(minWidth = 50)  # 25% width, 100px minimum
                           )
      )
      
    })
    
  })
  
}

## To be copied in the UI
# mod_sentiment_ui("mod_sentiment_ui_1")

## To be copied in the server
# mod_sentiment_server("mod_sentiment_ui_1")
