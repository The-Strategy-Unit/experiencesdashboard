#' demographics_selection UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_demographics_selection_ui <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("dynamic_demographics_selection"))
  )
}

#' demographics_selection Server Functions
#'
#' @noRd
mod_demographics_selection_server <- function(id, filter_data, data_exists) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # the UI render
    output$dynamic_demographics_selection <- renderUI({
      req(data_exists) # only run if data exist in the database

      # check which demographic variables are present before displaying its associated output
      tagList(
        if (isTruthy(get_golem_config("demography_1"))) {
          uiOutput(ns("demography_1_UI"))
        },
        if (isTruthy(get_golem_config("demography_2"))) {
          uiOutput(ns("demography_2_UI"))
        },
        if (isTruthy(get_golem_config("demography_3"))) {
          uiOutput(ns("demography_3_UI"))
        }
      )
    })

    # demography selection----

    output$demography_1_UI <- renderUI({
      isolate(
        choices <- get_demographic_choices(
          filter_data()$unique_data,
          get_golem_config("demography_1")
        )
      )

      selectInput(session$ns("select_demography_1"),
        label = paste("Select", get_golem_config("demography_1"), "(defaults to all)"),
        choices = na.omit(choices),
        selected = NULL, multiple = TRUE
      )
    })

    output$demography_2_UI <- renderUI({
      isolate(
        choices <- get_demographic_choices(
          filter_data()$unique_data,
          get_golem_config("demography_2")
        )
      )

      selectInput(session$ns("select_demography_2"),
        label = paste("Select", get_golem_config("demography_2"), "(defaults to all)"),
        choices = na.omit(choices),
        selected = NULL, multiple = TRUE
      )
    })

    output$demography_3_UI <- renderUI({
      isolate(
        choices <- get_demographic_choices(
          filter_data()$unique_data,
          get_golem_config("demography_3")
        )
      )

      selectInput(session$ns("select_demography_3"),
        label = paste("Select", get_golem_config("demography_3"), "(defaults to all)"),
        choices = na.omit(choices),
        selected = NULL, multiple = TRUE
      )
    })

    reactive(
      list(
        "select_demography_1" = input$select_demography_1,
        "select_demography_2" = input$select_demography_2,
        "select_demography_3" = input$select_demography_3
      )
    )
  })
}
