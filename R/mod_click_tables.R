#' click_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_click_tables_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("dynamic_click_tableUI")) |>
      shinycssloaders::withSpinner()
  )
}

#' click_tables Server Functions
#'
#' @noRd
mod_click_tables_server <- function(id, filter_data, data_exists, comment_type = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$dynamic_click_tableUI <- renderUI({
      validate(
        need(data_exists, "Sub-Category Table will appear here")
      )

      fluidPage(
        p("The boxes below show the volume of comments/responders in the data 
        after applying the filters at the side."),
        mod_summary_record_ui("summary_record_1") |>
          shinycssloaders::withSpinner(),
        # hr(),
        p("This table shows how many comments there are for each 
          sub-category before you drill down into the underlying comments."),
        strong("Please click a row to see comments related to that sub-category"),
        DT::DTOutput(ns("table")) |>
          shinycssloaders::withSpinner(),
        hr(),
        h5("Please select a Sub-category from the table above in other to drill down the table below"),
        uiOutput(ns("comment_table")) |>
          shinycssloaders::withSpinner()
      )
    })

    calculatedTable <- reactive({
      calculate_table(
        table_data = filter_data()$single_labeled_filter_data,
        count_column = "category",
        comment_type = comment_type
      )
    })

    output$table <- DT::renderDT({
      calculated_table <- calculatedTable()

      DT::datatable(calculated_table,
        colnames = c("Sub Category", "No. of comments", "% contribution"),
        selection = "single",
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          pageLength = 10,
          lengthMenu = c(10, 15, 20, 50),
          dom = "Blfrtip",
          buttons = c("copy", "csv", "excel", "pdf", "print"),
          initComplete = dt_nhs_header()
        )
      )
    })

    return_data <- reactive({
      data <- filter_data()$single_labeled_filter_data

      if (isTruthy(input$table_rows_selected)) {
        category_selected <- calculatedTable()$category[input$table_rows_selected]

        print(category_selected)

        data <- filter_data()$single_labeled_filter_data |>
          dplyr::filter(category == category_selected)
      }

      return(prep_data_for_comment_table(data))
    })

    ## the comments tables ----
    output$comment_table <- renderUI({
      mod_comment_download_server(ns("comment_download_1"), return_data(), filepath = "sub-category-data-")
    })

    reactive(
      input$table_rows_selected
    )
  })
}
