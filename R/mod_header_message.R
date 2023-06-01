#' header_message UI Function
#' #' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_header_message_ui <- function(id) {
  ns <- NS(id)
  dropdownMenuOutput(ns("dynamic_messageMenu"))
}

#' header_message Server Functions
#'
#' @noRd
mod_header_message_server <- function(id, filter_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$dynamic_messageMenu <- renderMenu({
      req(nrow(filter_data()$filter_data) > 0)

      isolate({
        last_upload_date <- unique(filter_data()$filter_data$last_upload_date) %>% na.omit()
        last_upload_date <- if (length(last_upload_date) < 1) "No edit yet" else strptime(max(last_upload_date), format = "%Y-%m-%d %H:%M")

        last_date_edit <- unique(filter_data()$filter_data$last_edit_date) %>% na.omit()
        last_date_edit <- if (length(last_date_edit) < 1) "No edit yet" else strptime(max(last_date_edit), format = "%Y-%m-%d %H:%M")

        total_users <- filter_data()$unique_data$pt_id %>%
          unique() %>%
          length()
      })

      dropdownMenu(
        type = "messages",
        headerText = strong("Dashboard Status", style = "color: #005EB8;"),
        icon = icon("circle-info"),
        messageItem(
          from = strong(total_users, style = "color: #005EB8;"),
          message = p("Total number of responders"),
          icon = icon("users", style = "color: #005EB8;"),
          time = Sys.Date()
        ),
        messageItem(
          from = strong(paste(last_upload_date), style = "color: #005EB8;"),
          message = p("Date Data was Last Upload"),
          icon = icon("file-pen", style = "color: #005EB8;"),
          time = Sys.Date()
        ),
        messageItem(
          from = strong(paste(last_date_edit), style = "color: #005EB8;"),
          message = p("Date Data was Last Editted"),
          icon("calendar", style = "color: #005EB8;")
        )
      )
    })
  })
}
