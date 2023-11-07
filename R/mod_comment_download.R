#' comment_download UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_comment_download_ui <- function(id) {
  ns <- NS(id)
}

#' comment_download Server Functions
#'
#' @noRd
mod_comment_download_server <- function(id, return_data, filepath) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    memoised_render_comment_table <- memoise::memoise(render_comment_table, cache = session$cache) # create a session-level cacheable version of comment_table()

    ## the comments tables ----
    output$dynamic_comment_table <- DT::renderDT({
      memoised_render_comment_table(return_data)
    })

    # Download the data ----
    output$download_comments <- downloadHandler(
      filename = paste0(filepath, Sys.Date(), ".xlsx"),
      content = function(file) {
        withProgress(message = "Downloading...", value = 0, {
          writexl::write_xlsx(return_data, file)
          incProgress(1)
        })
      }
    )

    # if the data is empty then don't show the UI
    if (nrow(return_data) < 1) {
      return(tagList(
        pre(strong("No data to show"))
      ))
    }

    # return the UI (the download button and comment table)
    return(
      tagList(
        fluidRow(
          downloadButton(ns("download_comments"), "Download data from table",
            icon = icon("download")
          ),
          DT::DTOutput(ns("dynamic_comment_table"))
        )
      )
    )
  })
}
