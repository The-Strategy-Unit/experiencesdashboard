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
    wellPanel(fluidRow(
      column(
        3,
        dateRangeInput(
          ns("date_range"),
          label = h5("Date Range:"),
          min = "2013-01-01",
          start = "2013-01-01",
          end = "2019-02-11"
        )
      ),
      
      column(
        3,
        selectInput(
          ns("select_division"),
          label = h5("Division:"),
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
        3,
        selectInput(
          ns("select_super"),
          label = h5("Category:"),
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
          selected = c("Communication", 
                       "Staff/Staff Attitude", 
                       "Environment/Facilities", 
                       "Access to Services", 
                       "Care/ Treatment", 
                       "Couldn't be improved", 
                       "Service Quality/Outcomes", 
                       "Involvement", 
                       "Food", 
                       "Privacy and Dignity", 
                       "MHA", 
                       "Equality/Diversity", 
                       "Smoking", 
                       "Leave", 
                       "Safety", 
                       "Physical Health", 
                       "Record Keeping"),
          multiple = TRUE
        )
      ),
      
      column(
        3,
        selectInput(
          ns("select_sentiment"),
          label = h5("Sentiment:"),
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
          selected = "positive"
        )
      )
    ),
    style = "padding: 5px;"),
    
    tabsetPanel(
      type = "tabs",
      tabPanel("Sentiment intersections",
               plotOutput(ns("sentiment_plot_upset"))),
      
      tabPanel("Change in sentiment over time",
               checkboxInput(ns("proportion"), 
                             "Show proportions of sentiments"),
               plotOutput(ns("sentiment_plot_time"))
               
      ),
      
      tabPanel("Feedback comments",
               fluidRow(
                 column(12,
                        reactable::reactableOutput(ns(
                          "sentiment_table"
                        )
                        )
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
    
    sentiments_ordered <- c("positive", "trust", "joy", "anticipation", 
                            "surprise", "fear", "sadness", "disgust", "anger", 
                            "negative")
    
    sentiments_ordered_sentence <- stringr::str_to_sentence(sentiments_ordered)
        
    # Fist, tidy entire data for upset plot
    sentiment_txt_data_upset <- sentiment_txt_data %>% 
      dplyr::mutate(date = lubridate::date(date),
                    year = lubridate::year(date),
                    id = 1:nrow(sentiment_txt_data),
                    all_sentimtents_unnest = all_sentiments) %>% 
      dplyr::select(id, date, year, super, division2, improve, all_sentiments, 
                    all_sentimtents_unnest) %>% 
      tidyr::unnest(cols = all_sentimtents_unnest) %>% 
      dplyr::distinct() %>% 
      dplyr::mutate(value = TRUE) %>% 
      tidyr::pivot_wider(id_cols = c("id", "date", "year", "super", "division2", 
                                     "improve", "all_sentiments"), 
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
        dplyr::filter(division2 %in% input$select_division) %>% 
        dplyr::filter(super %in% input$select_super)
      
    })
    
    # Create upset plot ----
    output$sentiment_plot_upset <- renderPlot({
      
      # PLOT START
      UpSetR::upset(data = as.data.frame(
        sentiment_txt_data_r()[, c("year", "anger", "anticipation", "disgust",
                                   "fear", "joy", "negative", "positive",
                                   "sadness", "surprise", "trust")]),
        # MAYBE LET USER PICK NUMBER OF INTERSECTIONS
        nintersects = 15,
        sets = c("anger", "anticipation", "disgust", "fear", "joy",
                 "negative", "positive", "sadness", "surprise", "trust"),
        order.by = "freq",
        text.scale = 1.5,
        # ADD THE QUERY AT A LATER POINT
        # queries = list(list(query = UpSetR::intersects,
        #                     params = list(c(input$select_sentiment)),
        #                     color = "orange",
        #                     active = F))
        # COMMENTING OUT AttRIBUTE PLOT BECAUSE OF THE SCALING ISSUE
        # ALSO I'm CURRENTLUY NOT HAPPY WITH THE DESIGN OF IT!
        # , attribute.plots = list(gridrows = 50,
        #                        plots = list(list(plot = UpSetR::histogram,
        #                                          x = "year",
        #                                          queries = T)),
        # ncols = 1)
      )}
      # PLOT END

      # TRYING TO CHANGE PLOT SIZE HERE (see https://github.com/rstudio/shiny/issues/650)
      , height = function() {
        session$clientData$`output_mod_sentiment_ui_1-sentiment_plot_upset_width` / 3
      }
    )
    
    output$sentiment_plot_time <- renderPlot({
      
      p <- sentiment_txt_data_r() %>% 
        tidyr::unnest(cols = all_sentiments) %>% 
        dplyr::mutate(all_sentiments = factor(x = all_sentiments, 
                                              levels = sentiments_ordered,
                                              labels = sentiments_ordered_sentence)) %>% 
        ggplot2::ggplot(ggplot2::aes(date, fill = all_sentiments)) +
        ggplot2::scale_x_date() +
        ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
        ggplot2::scale_fill_viridis_d(direction = -1) +
        ggplot2::labs(x = "Date", 
                      y = "Density", 
                      fill = "Sentiments") +
        ggplot2::theme(text = ggplot2::element_text(size = 16))
      
      if(input$proportion){
        
        return(p + ggplot2::geom_histogram(position = "fill", binwidth = 5))
      } else {
        
        return(p + ggplot2::geom_histogram(binwidth = 5))
      }
      
    })

    # Create reactive table ----
    output$sentiment_table <- reactable::renderReactable({
      
      filtered_comments <- sentiment_txt_data_r() %>% 
        dplyr::select(id, all_sentiments, improve) %>% 
        dplyr::mutate(length = lengths(all_sentiments),
                      all_sentimtents_unnest = all_sentiments) %>%
        dplyr::filter(length == length(input$select_sentiment)) %>%
        tidyr::unnest(cols = all_sentimtents_unnest) %>% 
        dplyr::group_by(id) %>% 
        dplyr::mutate(test_sentiment = dplyr::case_when(
          all_sentimtents_unnest %in% input$select_sentiment ~ TRUE),
          sum_temp = sum(test_sentiment)) %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(is.na(sum_temp) == FALSE) %>% 
        dplyr::select(improve) %>%
        dplyr::distinct()
      
      reactable::reactable(
        filtered_comments,
        # pagination = FALSE
        # height = 500
        # bordered = TRUE,
        borderless = TRUE,
        highlight = TRUE,
        showSortIcon = FALSE,
        filterable = TRUE,
        showPageSizeOptions = TRUE, 
        pageSizeOptions = c(10, 15, 20, 25, 30), 
        defaultPageSize = 15,
        columns = list(
          improve = reactable::colDef(minWidth = 200, sortable = FALSE, name = " ")   # 50% width, 200px minimum
          # ,super = reactable::colDef(minWidth = 50)   # 25% width, 100px minimum
          # ,all_sentiments = reactable::colDef(minWidth = 50)  # 25% width, 100px minimum
        )
      )
      
    })
    
  })
  
}

## To be copied in the UI
# mod_sentiment_ui("mod_sentiment_ui_1")

## To be copied in the server
# mod_sentiment_server("mod_sentiment_ui_1")
