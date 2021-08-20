#' mod_sentiment UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sentiment_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidPage(
      fluidRow(
        uiOutput(ns("superUI")),
        conditionalPanel(
          condition = 'input.tabs != "upset"', 
          uiOutput(ns("sentimentUI"))
        )
      ),
      
      tabsetPanel(
        id = "tabs",
        type = "tabs",
        tabPanel("Comments", value = "comments",
                 br(),
                 fluidRow(
                   column(12,
                          box(
                            width = NULL, 
                            background = "light-blue",
                            textOutput(ns("show_comments_box"))
                          )
                   )
                 ),
                 fluidRow(
                   column(12,
                          reactable::reactableOutput(ns("sentiment_table"))
                   )
                 )
        ),
        tabPanel(
          "Timeline",
          br(),
          fluidRow(
            column(12,
                   box(
                     width = NULL, 
                     background = "light-blue",
                     textOutput(ns("change_time_sentiments_txt"))
                   )
            )
          ),
          fluidRow(
            column(3,
                   uiOutput(ns("divide_plotUI")),
            ),
            column(3,
                   selectInput(ns("select_sentiment_plot_position"), 
                               label = h5(strong("Show proportion or total:")),
                               choices = c("Proportion" = "fill",
                                           "Totals" = "stack"),
                               selected = "stack"
                   )
            )      
          ),
          plotOutput(ns("sentiment_plot_time"))
        ),
        tabPanel("Sentiment combinations",
                 value = "upset",
                 br(),
                 fluidRow(
                   column(12,
                          box(
                            width = NULL, 
                            background = "light-blue",
                            textOutput(ns("combination_sentiments_txt"))
                          )
                   )
                 ),
                 plotOutput(ns("sentiment_plot_upset")
                 )
        )
      )
    )
  )
}

#' mod_sentiment Server Function
#'
#' @noRd 
mod_sentiment_server <- function(id, filter_data, nrc_sentiments){
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    
    output$superUI <- renderUI({
      
      isolate(
        super_choices <- na.omit(
          unique(sentiment_txt_data_tidy_r()$category)
        )
      )
      
      selectInput(
        session$ns("select_super"),
        label = h5(strong("Select categories (defaults to all)")),
        choices = super_choices,
        multiple = TRUE,
        width = "100%"
      )
    })
    
    debounce_select_super <- reactive({
      input$select_super
    }) %>% 
      debounce(3000)
    
    output$sentimentUI <- renderUI({
      
      choices <- all_sentiment_table() %>% 
        dplyr::distinct(all_sentiments_unnest) %>% 
        dplyr::pull()
      
      selectInput(
        session$ns("select_sentiment"),
        label = h5(strong("Select sentiments (defaults to all):")),
        choices = choices,
        multiple = TRUE
      )
    })
    
    output$divide_plotUI <- renderUI({
      
      choices <- list(1, 2, 3) # wait for it...
      
      names(choices) <- c("Category", get_golem_config("location_1"), 
                          paste0(get_golem_config("location_1"), 
                                 " and category"))
      
      selectInput(ns("select_sentiment_plot_facet"), 
                  label = h5(strong("Divide plot by:")), 
                  choices = choices, 
                  selected = 1)
    })
    
    # Create reactive dataframe ----
    sentiment_txt_data_tidy_r <- reactive({
      
      sentiment_txt_data_tidy <- calc_sentiment(filter_data()$filter_data, 
                                                nrc_sentiments) %>% 
        tidy_sentiment_txt()
      
      if(isTruthy(debounce_select_super())){
        
        sentiment_txt_data_tidy <- sentiment_txt_data_tidy %>% 
          dplyr::filter(category %in% debounce_select_super())
      }
      
      return(sentiment_txt_data_tidy)
    })
    
    # return table with all the sentiments in it
    
    all_sentiment_table <- reactive({
      
      sentiment_txt_data_tidy_r() %>% 
        make_sentiment_table(nrc_sentiments)
    })
    
    # Create reactive table ----
    output$sentiment_table <- reactable::renderReactable({
      
      req(nrow(all_sentiment_table()) > 0)
      
      comments <- all_sentiment_table()
      
      if(isTruthy(input$select_sentiment)){
        
        comments <- comments %>% 
          dplyr::filter(all_sentiments_unnest %in% input$select_sentiment)
      }
      
      comments <- comments %>% 
        dplyr::select(comment_txt, all_sentiments) %>% 
        dplyr::distinct()
      
      if(nrow(comments) == 0){
        
        comments <- data.frame("No data found" = "Please expand your selection")
      }
      
      reactable::reactable(
        comments,
        borderless = TRUE,
        highlight = TRUE,
        showSortIcon = FALSE,
        filterable = TRUE,
        showPageSizeOptions = TRUE, 
        pageSizeOptions = c(10, 15, 20, 25, 30), 
        defaultPageSize = 10,
        columns = list(
          comment_txt = reactable::colDef(minWidth = 200, 
                                          sortable = FALSE, 
                                          name = "What could we do better?"),
          all_sentiments = reactable::colDef(sortable = TRUE,
                                             name = "Sentiments")
        )
      )
    })
    
    # Create timeline ----
    output$sentiment_plot_time <- renderPlot({
      
      if(!isTruthy(input$select_sentiment)){
        
        choices <- all_sentiment_table() %>% 
          dplyr::distinct(all_sentiments_unnest) %>% 
          dplyr::pull()
      } else {
        
        choices <- input$select_sentiment
      }
      
      plot_sentiment(sentiment_txt_data_tidy_r(), 
                     sentiment_names = nrc_sentiments,
                     select_sentiment = choices,
                     select_fill_type = input$select_sentiment_plot_position,
                     select_facet = input$select_sentiment_plot_facet)
      
    }
    , height = function() {
      session$clientData$`output_mod_sentiment_ui_1-sentiment_plot_time_width` / 2.3
    }
    )
    
    # Create upset plot ----
    output$sentiment_plot_upset <- renderPlot({
      
      UpSetR::upset(data = as.data.frame(sentiment_txt_data_tidy_r()
                                         [, c("year", "anger", "anticipation",
                                              "disgust", "fear", "joy", "negative",
                                              "positive", "sadness", "surprise",
                                              "trust")]),
                    nintersects = 15,
                    sets = c("anger", "anticipation", "disgust", "fear", "joy",
                             "negative", "positive", "sadness", "surprise", "trust"),
                    order.by = "freq",
                    text.scale = 1.5
      )}
      , height = function() {
        session$clientData$`output_mod_sentiment_ui_1-sentiment_plot_upset_width` / 2.3
      }
    )
  })
  
}
