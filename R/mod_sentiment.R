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

    event_id <- ns("sentiment_plot")

    output$dynamic_sentiment_UI <- renderUI({
      # Only show module contents if the data from the database is not empty
      validate(
        need(data_exists, "sentiment plots will appear here")
      )

      tagList(
        pre("When the filtered data contain less than 7months of data, The sentiment plot below will be plotted as a weekly data rather than a month data. You can interact with the plot to read the underline comments",
          style = "background-color:#005EB8; color:#fff"
        ),
        plotly::plotlyOutput(ns("sentiment_plot")) %>%
          shinycssloaders::withSpinner(),
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

      uiOutput(ns("sentiment_comment"))
    })

    # the data ----

    sentiment_data <- reactive({
      req(filter_data()$filter_data)

      filter_data()$filter_data |>
        dplyr::filter(!is.na(sentiment))
    })

    # set the timeframe to view to weekly if the data duration is less than 6months (180 days) else monthly
    timeframe <- reactive(
      if (max(sentiment_data()$date) - min(sentiment_data()$date) < 180) {
        "week"
      } else {
        "month"
      }
    )

    clicked_data <- reactive(
      sentiment_data() |>
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
          colors = c("#009639", "#FAE100", "#DA291C"),
          event_id = event_id,
          plot_title = glue::glue("Comment sentiment over time ({timeframe()})"),
          xaxis_title = glue::glue("Date ({timeframe()})"),
          yaxis_title = "% contribution",
        )
    })

    # the input reactive object ----
    return_data <- reactive({
      ret_data <- sentiment_data()

      if (isTruthy(plotly::event_data("plotly_click", source = event_id, priority = "input"))) {
        fiter_output <- plotly::event_data("plotly_click", source = event_id, priority = "input")

        selected_date <- fiter_output$x
        selected_sentiment <- fiter_output$key

        cat("Selected date:", selected_date, "\n")
        cat("Selected sentiment:", as.character(selected_sentiment), "\n")

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
    output$sentiment_comment <- renderUI({
      mod_comment_download_server(ns("comment_download_1"), return_data(), filepath = "sentiment-filter-data-")
    })
  })
}
