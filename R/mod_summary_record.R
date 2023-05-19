#' summary_record UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_summary_record_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("dynamic_summary_record"))
}

#' summary_record Server Functions
#'
#' @noRd
mod_summary_record_server <- function(id, db_data, filter_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # define global variables
    width <- 3

    global <- reactiveValues(
      n_responses = NULL, n_individuals = NULL,
      current_responses = NULL, current_individuals = NULL
    )

    output$dynamic_summary_record <- renderUI({
      global$n_responses <- db_data %>%
        dplyr::filter(!is.na(comment_txt)) %>%
        dplyr::tally() %>%
        dplyr::pull(n)

      global$n_individuals <- db_data %>%
        dplyr::filter(!is.na(comment_txt)) %>%
        dplyr::distinct(pt_id) %>%
        dplyr::tally() %>%
        dplyr::pull(n)

      global$current_responses <- filter_data()$filter_data %>%
        dplyr::filter(!is.na(comment_txt)) %>%
        dplyr::tally() %>%
        dplyr::pull(n)

      global$current_individuals <- filter_data()$filter_data %>%
        dplyr::filter(!is.na(comment_txt)) %>%
        dplyr::distinct(pt_id) %>%
        dplyr::tally() %>%
        dplyr::pull(n)

      tagList(
        fluidRow(
          column(width, valueBoxOutput (ns("commentBox"), width = NULL)),
          column(width, valueBoxOutput (ns("individualBox"), width = NULL)),
          column(width, valueBoxOutput (ns("current_commentBox"), width = NULL)),
          column(width, valueBoxOutput (ns("current_individualBox"), width = NULL))
        )
      )
    })

    # value boxes
    output$commentBox <- renderValueBox({
      shinydashboard::valueBox(
        format(global$n_responses, format = "d", big.mark = ","),
        p("Comments in Database", style = "font-size: 90%"),
        icon = icon("comment"), 
        color = "light-blue"
      )
    })
    output$individualBox <- renderValueBox({
      shinydashboard::valueBox(
        format(global$n_individuals, format = "d", big.mark = ","),
        p("Individuals in Database", style = "font-size: 90%"),
        icon = icon("users"), 
        color = "light-blue"
      )
    })
    output$current_commentBox <- renderValueBox({
      shinydashboard::valueBox(
        format(global$current_responses, format = "d", big.mark = ","),
        p("Comments in Current Selection", style = "font-size: 90%"),
        icon = icon("comment-dots"), 
        color = "light-blue"
      )
    })
    output$current_individualBox <- renderValueBox({
      shinydashboard::valueBox(
        format(global$current_individuals, format = "d", big.mark = ","),
        p("Individuals in Current Selection", style = "font-size: 90%"),
        icon = icon("users"), 
        color = "light-blue"
      )
    })
  })
}
