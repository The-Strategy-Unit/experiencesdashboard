#' demographics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_demographics_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("dynamic_demo_UI")) |>
    shinycssloaders::withSpinner()
}

#' demographics Server Functions
#'
#' @noRd
mod_demographics_server <- function(id, filter_data, data_exists) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # the UI render

    output$dynamic_demo_UI <- renderUI({
      validate(
        need(data_exists, "Demography plots will appear here")
      )

      # check which demographic variables are present
      isolate({
        has_demography_1 <- isTruthy(get_golem_config("demography_1"))
        has_demography_2 <- isTruthy(get_golem_config("demography_2"))
        has_demography_3 <- isTruthy(get_golem_config("demography_3"))
      })

      # determine the column width base on the number of demographic variables present

      demography_cond <- sum(has_demography_1, has_demography_2, has_demography_3)

      width <- dplyr::case_when(
        demography_cond == 3 ~ 4,
        demography_cond == 2 ~ 6,
        TRUE ~ 12
      )

      tagList(
        br(),
        fluidRow(
          if (has_demography_1) {
            column(
              width,
              plotly::plotlyOutput(ns("demography_1_graph")) |>
                shinycssloaders::withSpinner()
            )
          },
          if (has_demography_2) {
            column(
              width,
              plotly::plotlyOutput(ns("demography_2_graph")) |>
                shinycssloaders::withSpinner()
            )
          },
          if (has_demography_3) {
            column(
              width,
              plotly::plotlyOutput(ns("demography_3_graph")) |>
                shinycssloaders::withSpinner()
            )
          }
        ),
        hr(),
        pre("The below chart shows the average percentage of FFT score for each group in the demographic feature.",
          "Note: Only 1-5 point FFT scores are considered and categories with fewer than 10 individuals are excluded",
          style = "background-color:#005EB8; color:#fff"
        ),
        fluidRow(
          if (has_demography_1) {
            column(
              width,
              plotly::plotlyOutput(ns("compare_demography_1")) |>
                shinycssloaders::withSpinner()
            )
          },
          if (has_demography_2) {
            column(
              width,
              plotly::plotlyOutput(ns("compare_demography_2")) |>
                shinycssloaders::withSpinner()
            )
          },
          if (has_demography_3) {
            column(
              width,
              plotly::plotlyOutput(ns("compare_demography_3")) |>
                shinycssloaders::withSpinner()
            )
          }
        )
      )
    })

    # distribution----

    output$demography_1_graph <- plotly::renderPlotly({
      demo_data <- filter_data()$unique_data |>
        dplyr::arrange(get_golem_config("demography_1"))
      demo_data[, get_golem_config("demography_1")] <- demo_data[, get_golem_config("demography_1")] |>
        unlist(use.names = F) |>
        factor()

      demo_data |>
        demographic_distribution(variable = get_golem_config("demography_1"))
    })

    output$demography_2_graph <- plotly::renderPlotly({
      filter_data()$unique_data |>
        demographic_distribution(variable = get_golem_config("demography_2"))
    })

    output$demography_3_graph <- plotly::renderPlotly({
      filter_data()$unique_data |>
        demographic_distribution(variable = get_golem_config("demography_3"))
    })

    # compare scores----

    output$compare_demography_1 <- plotly::renderPlotly({
      compare_demographics(filter_data()$unique_data, get_golem_config("demography_1"))
    })

    output$compare_demography_2 <- plotly::renderPlotly({
      compare_demographics(filter_data()$unique_data, get_golem_config("demography_2"))
    })

    output$compare_demography_3 <- plotly::renderPlotly({
      compare_demographics(filter_data()$unique_data, get_golem_config("demography_3"))
    })
  })
}
