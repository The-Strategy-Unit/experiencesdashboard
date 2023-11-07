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
    uiOutput(ns("dynamic_trend_overlap")) |>
      shinycssloaders::withSpinner()
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
          p(HTML(paste0("
          This page provides an optional, more advanced data exploration tool,
          best utilised by analysts who want to move beyond looking at
          comments for different sub-categories in isolation and want to explore
          the relationships between the sub-categories. To get further
          detail about how to use the upset plot, please see the ",
          enurl(
            "https://cdu-data-science-team.github.io/PatientExperience-QDC/dashboard/#inter-relationship-between-sub-categories",
            "Patient Experience – QDC documentation page.")
          ))),
          p(with_red_stars(strong("
          Please note that this plot is not currently interractive. Please 
          scroll down to select the sub-categories and view the comments.
          "))),
          HTML(paste0(with_red_stars(strong("For users on small screen:  ")), 
          "  This plots may not fit the screen, a quick fix is to Zoom out to 
          75% or 80% on your browser (access your browser setting from the three
          dots (…) at the top right)")),
          hr(),
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
            uiOutput(ns("within_categories_ui")) |>
              shinycssloaders::withSpinner()
          ),
          # Across Categories UI ----
          tabPanel("Across all categories",
            value = "tab_across_categories",
            uiOutput(ns("across_categories_ui")) |>
              shinycssloaders::withSpinner()
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
    })

    output$within_categories_ui <- renderUI({
      input_select_super_category <- input$select_super_category
      input_min_size <- input$min_size

      mod_overlap_1_server(
        ns("overlap_UI"), filter_data,
        input_select_super_category, input_min_size
      )
    })

    output$across_categories_ui <- renderUI({
      input_min_size <- input$min_size

      mod_overlap_1_server(
        ns("overlap_1_UI"), filter_data,
        NULL, input_min_size
      )
    })
  })
}
