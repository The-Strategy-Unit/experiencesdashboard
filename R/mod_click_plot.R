#' click_Plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_click_plot_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("dynamic_click_plot_UI"))
}

#' click_tables Server Functions
#'
#' @noRd
mod_click_plot_server <- function(id, filter_data, comment_type, event_id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    clicked_data <- reactive(calculate_table(
      table_data = filter_data()$single_labeled_filter_data,
      count_column = "category",
      comment_type = comment_type
    ))

    global <- reactiveValues(category_selected = NULL)

    output$dynamic_click_plot_UI <- renderUI({
      tagList(
        plotly::plotlyOutput(ns("comment_plot")),
        tags$hr(),
        uiOutput(ns("dynamic_comment_UI"))
      )
    })

    output$dynamic_comment_UI <- renderUI({
      validate(
        need(
          plotly::event_data("plotly_click", source = event_id, priority = "event"),
          "You can select a category to view its comments"
        )
      )

      tagList(
        paste("Selected:", toupper(global$category_selected)),
        hr(),
        htmlOutput(ns("comments_verbatim"))
      )
    })

    # the plot

    output$comment_plot <- plotly::renderPlotly({
      clicked_data() %>%
        plotly::plot_ly(
          x = ~n, y = ~ reorder(Category, n), type = "bar",
          color = I("#005EB8"),
          source = event_id
        ) %>%
        plotly::layout(
          xaxis = list(title = "Number of comments"),
          yaxis = list(title = "")
        ) %>%
        plotly::config(
          displaylogo = FALSE,
          modeBarButtons = list(list("toImage")),
          toImageButtonOptions = list(
            format = "png"
          )
        )
    })

    # the comments

    output$comments_verbatim <- renderText({
      req(plotly::event_data("plotly_click", source = event_id, priority = "event"))

      d <- plotly::event_data("plotly_click", source = event_id, priority = "event")

      global$category_selected <- d$y

      cat("selected category:", global$category_selected, " \n")

      final_text <- show_text(
        data = filter_data()$single_labeled_filter_data,
        filter_by_column = "category",
        filter_by_text = global$category_selected,
        comment_type_filter = comment_type
      )
      return(final_text)
    })
  })
}
