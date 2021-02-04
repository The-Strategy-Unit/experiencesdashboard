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
    wellPanel(
      fluidRow(
        column(
          3,
          dateRangeInput(
            ns("date_range"),
            label = h5(strong("Select date range:")),
            min = "2013-01-01",
            start = "2013-01-01",
            end = "2018-12-31",
            max = "2019-02-11"
          )
        ),
        column(
          3,
          selectInput(
            ns("select_division"),
            label = h5(strong("Select divisions:")),
            choices = list(
              "Local partnerships- MH" = "Local partnerships- MH",
              "Forensic services" = "Forensic services",
              "Local partnerships- CH" = "Local partnerships- CH"
            ),
            multiple = TRUE,
            selected = c(
              "Local partnerships- MH",
              "Forensic services",
              "Local partnerships- CH"
            )
          )
        ),
        column(
          6,
          selectInput(
            ns("select_super"),
            label = h5(strong("Select categories:")),
            choices = list(
              "Communication" = "Communication",
              "Staff/Staff Attitude" = "Staff/Staff Attitude",
              "Environment/Facilities" = "Environment/Facilities",
              "Access to Services" = "Access to Services",
              "Care/ Treatment" = "Care/ Treatment",
              "Couldn't be improved" = "Couldn't be improved",
              "Service Quality/Outcomes" = "Service Quality/Outcomes",
              "Involvement" = "Involvement",
              "Food" = "Food",
              "Privacy and Dignity" = "Privacy and Dignity",
              "MHA" = "MHA",
              "Equality/Diversity" = "Equality/Diversity",
              "Smoking" = "Smoking",
              "Leave" = "Leave",
              "Safety" = "Safety",
              "Physical Health" = "Physical Health",
              "Record Keeping" = "Record Keeping"
            ),
            selected = c("Staff/Staff Attitude", 
                         "Care/ Treatment", 
                         "Service Quality/Outcomes"),
            multiple = TRUE
          )
        )
      ),
      style = "padding: 5px;"),
    
    tabsetPanel(
      type = "tabs",
      tabPanel("Comments",
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
                 column(6,
                        selectInput(
                          ns("select_sentiment_txt"),
                          label = h5(strong("Select combination of sentiments:")),
                          choices = list(
                            "anger" = "anger",
                            "anticipation" = "anticipation",
                            "disgust" = "disgust",
                            "fear" = "fear",
                            "joy" = "joy",
                            "negative" = "negative",
                            "positive" = "positive",
                            "sadness" = "sadness",
                            "surprise" = "surprise",
                            "trust" = "trust"
                          ),
                          multiple = TRUE,
                          selected = c("negative", "sadness")
                        )
                 )
               ),
               fluidRow(
                 column(12,
                        reactable::reactableOutput(ns("sentiment_table"))
                 )
               )
      ),
      tabPanel("Timeline",
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
                        selectInput(ns("select_sentiment_plot_facet"), 
                                    label = h5(strong("Divide plot by:")), 
                                    choices = list("Category" = 1, 
                                                   "Division" = 2, 
                                                   "Division and category" = 3), 
                                    selected = 1),
                 ),
                 column(3,
                        selectInput(ns("select_sentiment_plot_position"), 
                                    label = h5(strong("Show proportion or total:")),
                                    choices = c("Proportion" = "fill",
                                                "Totals" = "stack"),
                                    selected = "stack"
                                    )
                        ),
                 column(3,
                        selectInput(
                          ns("select_sentiment_plot"),
                          label = h5(strong("Select sentiments:")),
                          choices = list(
                            "anger" = "anger",
                            "anticipation" = "anticipation",
                            "disgust" = "disgust",
                            "fear" = "fear",
                            "joy" = "joy",
                            "negative" = "negative",
                            "positive" = "positive",
                            "sadness" = "sadness",
                            "surprise" = "surprise",
                            "trust" = "trust"
                          ),
                          multiple = TRUE,
                          selected = c("anger", 
                                       "fear", 
                                       "negative",
                                       "sadness")
                        )
                 )
               ),
               plotOutput(ns("sentiment_plot_time"))
      ),
      tabPanel("Sentiment combinations",
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
}

#' mod_sentiment Server Function
#'
#' @noRd 
mod_sentiment_server <- function(id){
  
  moduleServer( id, function(input, output, session){
    
    ns <- session$ns
    

    
    # Fist, tidy entire data for upset plot
    sentiment_txt_data_tidy <- tidy_sentiment_txt(sentiment_txt_data)
    
    # Create reactive dataframe ----
    # Filter data based on user selections
    sentiment_txt_data_tidy_r <- reactive({
      sentiment_txt_data_tidy %>% 
        dplyr::filter(date > input$date_range[1], date < input$date_range[2]) %>% 
        dplyr::filter(division2 %in% input$select_division) %>% 
        dplyr::filter(super %in% input$select_super)
    })
    
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
    
    # Create timeline ----
    output$sentiment_plot_time <- renderPlot({

      sentiments_ordered <- c("negative", "anger", "disgust", "fear", "sadness",
                              "anticipation", "surprise", "joy",  "trust", "positive")
      
      sentiments_ordered_sentence <- stringr::str_to_sentence(sentiments_ordered)
      
      sentiment_plot_time_temp <- sentiment_txt_data_tidy_r() %>% 
        tidyr::unnest(cols = all_sentiments) %>% 
        dplyr::filter(all_sentiments %in% input$select_sentiment_plot) %>% 
        dplyr::select(date, all_sentiments, super, division2) %>% 
        tidyr::drop_na() %>% 
        dplyr::mutate(all_sentiments = factor(x = all_sentiments,
                                              levels = sentiments_ordered,
                                              labels = sentiments_ordered_sentence)) %>%
        ggplot2::ggplot(ggplot2::aes(date, 
                                     fill = all_sentiments,
                                     colour = all_sentiments)) +
        ggplot2::geom_histogram(position = input$select_sentiment_plot_position, 
                                binwidth = 20) +
        ggplot2::scale_x_date() +
        ggplot2::scale_fill_viridis_d(direction = -1) +
        ggplot2::scale_colour_viridis_d(direction = -1) +
        ggplot2::labs(x = "Date", 
                      y = NULL, 
                      fill = "Selected\nsentiments",
                      colour = "Selected\nsentiments") +
        ggplot2::theme(text = ggplot2::element_text(size = 16))
      
      # Add facet ----
      if (input$select_sentiment_plot_facet == 1){
        sentiment_plot_time_temp +
          ggplot2::facet_grid(~super)
      } else if (input$select_sentiment_plot_facet == 2) {
        sentiment_plot_time_temp +
          ggplot2::facet_grid(~division2)
      } else if (input$select_sentiment_plot_facet == 3) {
        sentiment_plot_time_temp +
          ggplot2::facet_grid(division2~super)
        }
      }
      , height = function() {
      session$clientData$`output_mod_sentiment_ui_1-sentiment_plot_time_width` / 2.3
    }
    )
    
    # Create reactive table ----
    output$sentiment_table <- reactable::renderReactable({
      
      filtered_comments <- sentiment_txt_data_tidy_r() %>% 
        dplyr::select(id, all_sentiments, improve) %>% 
        # First get number of total sentiments in all comments
        dplyr::mutate(length = lengths(all_sentiments),
                      all_sentimtents_unnest = all_sentiments) %>%
        # Now filter for number of selected sentiments
        dplyr::filter(length == length(input$select_sentiment_txt)) %>%
        # Unnest to create long version of data
        tidyr::unnest(cols = all_sentimtents_unnest) %>% 
        # Group by comment id so that every computation is now for each comment
        dplyr::group_by(id) %>% 
        dplyr::mutate(test_sentiment = dplyr::case_when(
          all_sentimtents_unnest %in% input$select_sentiment_txt ~ TRUE),
          sum_temp = sum(test_sentiment)) %>% 
        dplyr::ungroup() %>% 
        # Filter comments that match the selected sentiments
        dplyr::filter(is.na(sum_temp) == FALSE) %>% 
        dplyr::select(id, improve) %>%
        dplyr::distinct()
      
      reactable::reactable(filtered_comments[, "improve"],
                           borderless = TRUE,
                           highlight = TRUE,
                           showSortIcon = FALSE,
                           filterable = TRUE,
                           showPageSizeOptions = TRUE, 
                           pageSizeOptions = c(10, 15, 20, 25, 30), 
                           defaultPageSize = 10,
                           columns = list(
                             improve = reactable::colDef(minWidth = 200, 
                                                         sortable = FALSE, 
                                                         name = "What could we do better?")
                           )
      )
    })
    
    # Write output text for text boxes ----
    output$combination_sentiments_txt <- renderText({
      paste0("TODO NOTE: ADD INFORMATION TO GUIDE INTERPRETATION OF UPSET PLOT. EXPLAIN DIFFERENCE BETWEEN SET SIZE AND INTERSECTION SIZE.")
    })
    
    output$change_time_sentiments_txt <- renderText({
      paste0("TODO NOTE: ADD INFORMATION TO GUIDE INTERPRETATION OF CHANGE IN SENTIMENT OVER TIME. ADD INFORMATION EXPLAINING THE DIFFERENCE BETWEEEN TOTALS AND PROPORTIONS.")
    })
    
    output$show_comments_box <- renderText({
      paste0("TODO NOTE: ADD INFORMATION EXPLAINING HOW TO FILTER FOR SENTIMENTS IN THE COMMENTS.")
    })
    
  })
  
}

## To be copied in the UI
# mod_sentiment_ui("mod_sentiment_ui_1")

## To be copied in the server
# mod_sentiment_server("mod_sentiment_ui_1")
