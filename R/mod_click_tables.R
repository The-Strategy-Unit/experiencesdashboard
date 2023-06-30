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
    uiOutput(ns("dynamic_click_tableUI"))
  )
}

#' click_tables Server Functions
#'
#' @noRd
mod_click_tables_server <- function(id, filter_data, comment_type = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    memoised_comment_table <- memoise::memoise(comment_table, cache = getShinyOption("cache")) # create a session-level cacheable version of comment_table()

    # add NHS blue color to the Datatable header
    initComplete <- DT::JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#005EB8', 'color': '#fff'});",
      "}"
    )

    output$dynamic_click_tableUI <- renderUI({
      validate(
        need(
          filter_data()$filter_data %>%
            dplyr::tally() %>%
            dplyr::pull(n) > 0,
          "Data Table will appear here"
        )
      )

      fluidPage(
        DT::DTOutput(ns("table")) %>%
          shinycssloaders::withSpinner(),
        hr(),
        h5("Please select a Sub-category from the table above in other to drill down the table below"),
        # add button for editing the table
        downloadButton(ns("click_table_download_data"), "Download data",
          icon = icon("download")
        ),
        DT::DTOutput(ns("comment_table"))
      )
    })

    calculatedTable <- reactive({
      calculate_table(
        table_data = filter_data()$single_labeled_filter_data,
        count_column = "category",
        comment_type = comment_type
      )
    }) %>%
      bindCache(filter_data()$single_labeled_filter_data)

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
          initComplete = initComplete
        )
      )
    })

    return_data <- reactive({
      data <- filter_data()$single_labeled_filter_data

      if (isTruthy(input$table_rows_selected)) {
        category_selected <- calculatedTable()$Category[input$table_rows_selected]

        print(category_selected)

        data <- filter_data()$single_labeled_filter_data %>%
          dplyr::filter(category == category_selected)
      }

      return(prep_data_for_comment_table(data))
    })

    output$comment_table <- DT::renderDT({
      memoised_comment_table(return_data())
    })

    # Download the data ####
    output$click_table_download_data <- downloadHandler(
      filename = paste0("sub-category-", Sys.Date(), ".xlsx"),
      content = function(file) {
        withProgress(message = "Downloading...", value = 0, {
          writexl::write_xlsx(return_data(), file)
          incProgress(1)
        })
      }
    )

    reactive(
      input$table_rows_selected
    )
  })
}
