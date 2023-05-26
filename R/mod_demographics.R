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
      
      validate(
        need(
          filter_data()$filter_data %>%
            dplyr::tally() %>%
            dplyr::pull(n) > 0,
          "Demography plots will appear here"
        )
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
        fluidRow(
          column(12, h3(textOutput(ns("total_responses"))))
        ),
        hr(),
        fluidRow(
          if (has_demography_1) {
            column(width, plotOutput(ns("demography_1_graph")))
          },
          if (has_demography_2) {
            column(width, plotOutput(ns("demography_2_graph")))
          },
          if (has_demography_3) {
            column(width, plotOutput(ns("demography_3_graph")))
          }
        ),
        hr(),
        h3("Categories with fewer than 10 individuals are excluded"),
        p("The below chart shows the average percentage of maximum FFT score for each category."),
        fluidRow(
          if (has_demography_1) {
            column(width, plotly::plotlyOutput(ns("compare_demography_1")))
          },
          if (has_demography_2) {
            column(width, plotly::plotlyOutput(ns("compare_demography_2")))
          },
          if (has_demography_3) {
            column(width, plotly::plotlyOutput(ns("compare_demography_3")))
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

    output$demography_1_graph <- renderPlot({
      
      demo_data <- filter_data()$unique_data %>%
        dplyr::arrange(get_golem_config("demography_1")) 
      demo_data[,get_golem_config("demography_1")] = demo_data[,get_golem_config("demography_1")] %>%
        unlist(use.names=F) %>% factor()
      demo_data %>%
        demographic_distribution(variable = get_golem_config("demography_1"))
    })

    output$demography_2_graph <- renderPlot({
      filter_data()$unique_data %>%
        demographic_distribution(variable = get_golem_config("demography_2"))
    })

    output$demography_3_graph <- renderPlot({
      filter_data()$unique_data %>%
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
