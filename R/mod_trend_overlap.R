#' trend_overlap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_trend_overlap_ui <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("dynamic_trend_overlap"))
  )
}

#' trend_overlap Server Functions
#'
#' @noRd
mod_trend_overlap_server <- function(id, filter_data, data_exists) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Super UI ----
    output$dynamic_trend_overlap <- renderUI({
      validate(
        need(data_exists, "Sub-category inter-relationship plots will appear here")
      )

      fluidPage(
        fluidRow(
          uiOutput(ns("trendUI")),
          column(
            6,
            numericInput(
              ns("min_size"),
              label = h5(strong("Select minimum number of
                              comments in groups (defaults to 2):")),
              value = 2,
              min = 1,
              max = 3000
            )
          ),
          br()
        ),
        tabsetPanel(
          id = ns("tabset_overlap"),
          type = "tabs",

          # Within Categories UI ----
          tabPanel("Within a category",
            value = "tab_within_categories",
            uiOutput(ns("within_categories_ui"))
          ),
          # Across Categories UI ----
          tabPanel("Across all categories",
            value = "tab_across_categories",
            uiOutput(ns("across_categories_ui"))
          )
        )
      )
    })

    # dynamic ui part ----
    output$trendUI <- renderUI({
      req(input$tabset_overlap != "tab_across_categories")

      choices <- get_unique_value(filter_data()$single_labeled_filter_data, "super_category")

      column(
        6,
        selectInput(
          session$ns("select_super_category"),
          label = h5(strong("Select a Category to see its Sub-categories
                              relationships (defaults to first category):")),
          choices = choices,
          selected = choices[1]
        )
      )
    }) %>%
      bindCache(
        filter_data()$single_labeled_filter_data$super_category,
        input$tabset_overlap
      )

    output$within_categories_ui <- renderUI({
      input_select_super_category <- input$select_super_category
      input_min_size <- input$min_size

      mod_overlap_1_server(
        ns("overlap_UI"), filter_data,
        input_select_super_category, input_min_size
      )
    }) %>%
      bindCache(
        filter_data()$single_labeled_filter_data,
        input$select_super_category,
        input$min_size
      )

    output$across_categories_ui <- renderUI({
      input_min_size <- input$min_size

      mod_overlap_1_server(
        ns("overlap_1_UI"), filter_data,
        NULL, input_min_size
      )
    })  %>%
      bindCache(
        filter_data()$single_labeled_filter_data,
        input$min_size
      )
  })
}
