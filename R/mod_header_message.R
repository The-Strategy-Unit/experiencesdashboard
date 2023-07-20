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
mod_header_message_server <- function(id, pool, db_data, data_exists) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$dynamic_messageMenu <- renderMenu({
      req(data_exists)

      isolate({
        last_upload_date <- DBI::dbGetQuery(pool, paste0("SELECT MAX(last_upload_date) FROM ",
                                                         get_golem_config('trust_name')))$`MAX(last_upload_date)`
        last_upload_date <- if (is.na(last_upload_date)) "No edit yet" else paste(strptime(last_upload_date, format = "%Y-%m-%d %H:%M"), "GMT")
        
        last_date_edit <- DBI::dbGetQuery(pool, paste0("SELECT MAX(last_edit_date) FROM ", 
                                                       get_golem_config('trust_name')))$`MAX(last_edit_date)`
        last_date_edit <- if (is.na(last_date_edit)) "No edit yet" else paste(strptime(last_date_edit, format = "%Y-%m-%d %H:%M"), "GMT")
        
        total_users <- db_data %>%
          dplyr::pull(pt_id) %>%
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
          icon = icon("users", style = "color: #005EB8;")
        ),
        messageItem(
          from = strong(last_upload_date, style = "color: #005EB8;"),
          message = p("Date data was last uploaded"),
          icon = icon("file-pen", style = "color: #005EB8;")
        ),
        messageItem(
          from = strong(last_date_edit, style = "color: #005EB8;"),
          message = p("Date data was last editted"),
          icon("calendar", style = "color: #005EB8;")
        )
      )
    })
  })
}
