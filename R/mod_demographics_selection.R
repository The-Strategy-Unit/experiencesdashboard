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
mod_demographics_selection_server <- function(id, filter_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # the UI render

    output$dynamic_demographics_selection <- renderUI({
      # check which demographic variables are present

      isolate({
        has_age <- "age" %in% colnames(filter_data()$unique_data)
        has_gender <- "gender" %in% colnames(filter_data()$unique_data)
        has_ethnicity <- "ethnicity" %in% colnames(filter_data()$unique_data)
      })

      tagList(
        if (has_age) {
          uiOutput(ns("age_UI"))
        },
        if (has_gender) {
          uiOutput(ns("gender_UI"))
        },
        if (has_ethnicity) {
          uiOutput(ns("ethnicity_UI"))
        }
      )
    })

    # demography selection----

    output$age_UI <- renderUI({
      isolate(
        data <- filter_data()$unique_data %>%
          dplyr::arrange(age) %>%
          dplyr::distinct(age, .keep_all = TRUE) %>%
          dplyr::filter(!is.na(age))
      )

      choices <- factor(data$age,
        exclude = NULL
      )

      selectInput(session$ns("select_age"),
        label = "Select age (defaults to all)",
        choices = na.omit(choices),
        selected = NULL, multiple = TRUE
      )
    })

    output$gender_UI <- renderUI({
      isolate(
        choices <- filter_data()$unique_data %>%
          dplyr::arrange(gender) %>%
          dplyr::distinct(gender) %>%
          dplyr::filter(gender != "")
      )

      selectInput(session$ns("select_gender"),
        label = "Select gender (defaults to all)",
        choices = dplyr::pull(na.omit(choices), gender),
        selected = NULL, multiple = TRUE
      )
    })

    output$ethnicity_UI <- renderUI({
      isolate(
        choices <- filter_data()$unique_data %>%
          dplyr::arrange(ethnicity) %>%
          dplyr::distinct(ethnicity) %>%
          dplyr::filter(ethnicity != "")
      )

      selectInput(session$ns("select_ethnicity"),
        label = "Select ethnicity (defaults to all)",
        choices = dplyr::pull(na.omit(choices), ethnicity),
        selected = NULL, multiple = TRUE
      )
    })

    reactive(
      list(
        "select_age" = input$select_age,
        "select_gender" = input$select_gender,
        "select_ethnicity" = input$select_ethnicity
      )
    )
  })
}
