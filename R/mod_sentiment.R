#' sentiment UI Function
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
    uiOutput(ns("dynamic_sentiment_UI"))
  )
}

#' sentiment Server Functions
#'
#' @noRd
mod_sentiment_server <- function(id, filter_data, data_exists) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    memoised_comment_table <- memoise::memoise(comment_table, cache = session$cache) # create a session-level cacheable version of comment_table()
    event_id <- ns("sentiment_plot")

    output$dynamic_sentiment_UI <- renderUI({
      # Only show module contents if the data from the database is not empty
      validate(
        need(data_exists, "sentiment plots will appear here")
      )

      tagList(
        p("6 months or less data is viewed on a weekly level"),
        plotly::plotlyOutput(ns("sentiment_plot")),
        tags$hr(),
        uiOutput(ns("dynamic_UI"))
      )
    })

    output$dynamic_UI <- renderUI({
      validate(
        need(
          plotly::event_data("plotly_click", source = event_id, priority = "event"),
          "Interact with the plot to view the underline comments"
        )
      )

      DT::DTOutput(ns("dynamic_table"))
    })

    # the data ----
    
    # set the timeframe to view to weekly if the data duration is less than 6months (180 days) else monthly
    timeframe <- reactive(
      if (max(filter_data()$filter_data$date) - min(filter_data()$filter_data$date) < 180) {
        "week"
      } else {
        "month"
      }
    )

    clicked_data <- reactive(
      filter_data()$filter_data |>
        dplyr::mutate(date = as.Date(cut(date, timeframe()))) |>
        multigroup_calculated_data("date", "sentiment")
    )

    # the plot ----
    output$sentiment_plot <- plotly::renderPlotly({
      clicked_data() %>%
        plot_sentiment_trend(
          x = ~date,
          y = ~percent,
          color = ~sentiment,
          key = ~sentiment,
          colors = c("#DA291C", "#FAE100", "#009639"),
          event_id = event_id,
          plot_title = "Comment sentiment over time",
          xaxis_title = glue::glue("Date ({timeframe()})"),
          yaxis_title = "% contribution",
        )
    })

    # the input reactive object ----
    return_data <- reactive({
      ret_data <- filter_data()$filter_data

      if (isTruthy(plotly::event_data("plotly_click", source = event_id, priority = "input"))) {
        fiter_output <- plotly::event_data("plotly_click", source = event_id, priority = "input")

        selected_date <- fiter_output$x
        selected_sentiment <- fiter_output$key

        print("-========== sentiment plot selection ============")
        print(selected_date)
        print(selected_sentiment)

        ret_data <- filter_data()$filter_data %>%
          dplyr::mutate(date_group = as.Date(cut(date, timeframe()))) %>%
          dplyr::filter(
            date_group == as.Date(selected_date),
            sentiment == selected_sentiment,
          )
      }
      return(prep_data_for_comment_table(ret_data))
    })

    ## the comments tables ----
    output$dynamic_table <- DT::renderDT({
      memoised_comment_table(return_data())
    })
  })
}
