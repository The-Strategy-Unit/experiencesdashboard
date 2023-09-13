#' crosstabs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_crosstabs_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("dynamic_UI"))
  )
}

#' crosstabs Server Functions
#'
#' @noRd
mod_crosstabs_server <- function(id, filter_data, data_exists) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    memoised_comment_table <- memoise::memoise(comment_table, cache = session$cache) # create a session-level cacheable version of comment_table()

    # create dynamic UI that Only show module contents if the data from the database is not empty
    output$dynamic_UI <- renderUI({
      validate(
        need(data_exists, "Cross tab table for sentient, sub-categories, and fft scores will appear here")
      )

      fluidPage(
        fluidRow(
          column(2,
            tagList(
              uiOutput(ns("row_inputs")),
              uiOutput(ns("col_inputs"))
            )
          ),
          column(10, DT::DTOutput(ns("crosstab_table"))) %>%
            shinycssloaders::withSpinner(type = 1, proxy.height = "0px")
        ),
        hr(),
        fluidRow(
          uiOutput(ns("selected_comment"))
        )
      )
    })

    #
    # create additional sidebar for input selection
    # selected row choice determine the choices available for column input 
    output$row_inputs <- renderUI({
      choices <- c("category", "sentiment", "fft")

      tagList(
        selectInput(ns("rows"), "Select row",
          choices = choices,
          selected = choices[1]
        )
      )
    })

    output$col_inputs <- renderUI({
      req(input$rows)

      choices <- setdiff(c("category", "sentiment", "fft"), input$rows)

      tagList(
        selectInput(ns("columns"), "Select columns",
          choices = choices,
          selected = choices[1]
        )
      )
    })

    # show the cross tab
    output$crosstab_table <- DT::renderDT({
      req(input$rows)
      req(input$columns)

      cross_tabulate(
        filter_data()$single_labeled_filter_data,
        input$rows,
        input$columns,
        by_row = FALSE
      ) %>%
        DT::datatable(
          selection = list(
            mode = "single",
            target = "cell"
          ),
          rownames = FALSE,
          filter = "top",
          class = "display cell-border compact",
          options = list(
            pageLength = 10,
            lengthMenu = c(10, 30, 50),
            dom = "lrtip",
            search = list(caseInsensitive = FALSE),
            scrollX = TRUE
          )
        )
    })

    return_data <- reactive({
      req(length(input$crosstab_table_cell_clicked) > 0)

      data <- get_crossway_data(
        filter_data()$single_labeled_filter_data,
        input
      )

      # return empty dataframe if the cell has no value or the row column is selected
      if (nrow(data) < 1) {
        return(data.frame())
      }

      prep_data_for_comment_table(data)
    })

    output$selected_comment <- renderUI({
      mod_comment_download_server(ns("comment_download_1"), return_data(), filepath = "cross-tab-data-")
    })
  })
}
