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

  uiOutput(ns("dynamic_demo_UI"))
}

#' demographics Server Functions
#'
#' @noRd
mod_demographics_server <- function(id, filter_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # the UI render

    output$dynamic_demo_UI <- renderUI({
      # check which demographic variables are present

      isolate({
        has_age <- "age" %in% colnames(filter_data()$unique_data)
        has_gender <- "gender" %in% colnames(filter_data()$unique_data)
        has_ethnicity <- "ethnicity" %in% colnames(filter_data()$unique_data)
      })

      # determine the column width base on the number of demographic variables present

      demography_cond <- sum(has_age, has_gender, has_ethnicity)

      width <- dplyr::case_when(
        demography_cond == 3 ~ 4,
        demography_cond == 2 ~ 6,
        TRUE ~ 12
      )

      tagList(
        fluidRow(
          column(12, h3(textOutput(ns("total_responses"))))
        ),
        hr(),
        fluidRow(
          if (has_age) {
            column(width, plotOutput(ns("age_graph")))
          },
          if (has_gender) {
            column(width, plotOutput(ns("gender_graph")))
          },
          if (has_ethnicity) {
            column(width, plotOutput(ns("ethnicity_graph")))
          }
        ),
        hr(),
        h3("Categories with fewer than 10 individuals are excluded"),
        p("The below chart shows the average percentage of maximum FFT score for each category."),
        fluidRow(
          if (has_age) {
            column(width, plotly::plotlyOutput(ns("compare_age")))
          },
          if (has_gender) {
            column(width, plotly::plotlyOutput(ns("compare_gender")))
          },
          if (has_ethnicity) {
            column(width, plotly::plotlyOutput(ns("compare_ethnicity")))
          }
        )
      )
    })

    # top row (explore adding this as a pop-up warning)

    output$total_responses <- renderText({
      no_responders <- filter_data()$demography_number

      if (no_responders < 20) {
        return(paste0("There are only ", no_responders, " responders in your
                      selection. Filtering below 20 responders with demographic
                      selections is disabled for reasons of confidentiality.
                      Please widen your selection by clinical area or
                      demography"))
      } else {
        return(paste0(
          "There is a total of ",
          no_responders, " responders in your selection"
        ))
      }
    })

    # distribution----

    output$age_graph <- renderPlot({
      filter_data()$unique_data %>%
        dplyr::arrange(age) %>%
        dplyr::mutate(age = factor(age)) %>%
        demographic_distribution(variable = "age")
    })

    output$gender_graph <- renderPlot({
      filter_data()$unique_data %>%
        demographic_distribution(variable = "gender")
    })

    output$ethnicity_graph <- renderPlot({
      filter_data()$unique_data %>%
        demographic_distribution(variable = "ethnicity")
    })

    # compare scores----

    output$compare_age <- plotly::renderPlotly({
      compare_demographics(filter_data()$unique_data, "age")
    })

    output$compare_gender <- plotly::renderPlotly({
      compare_demographics(filter_data()$unique_data, "gender")
    })

    output$compare_ethnicity <- plotly::renderPlotly({
      compare_demographics(filter_data()$unique_data, "ethnicity")
    })
  })
}
