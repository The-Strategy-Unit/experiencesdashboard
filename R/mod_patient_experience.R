#' patient_experience UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_patient_experience_ui <- function(id) {
  ns <- NS(id)

  uiOutput(ns("dynamicUI"))
}

#' patient_experience Server Functions
#'
#' @noRd
mod_patient_experience_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$dynamicUI <- renderUI({
      # Summary and Report tab (key tabs to show)

      ui_list <- list(

        # documentation tab
        tabPanel(
          "Data categorisation framework",
          mod_documentation_page_ui("documentation_page")
        ),

        # Data management tab

        tabPanel(
          "Data upload and management",
          mod_data_management_ui("data_management_1")
        ),

        # summary tab
        tabPanel(
          "Distribution of comments over time",
          mod_trend_ui("trend_ui_1")
        )
      )

      # Theme Trend and overlap tab
      
      ui_list <- c(
        ui_list,
        list(
          tabPanel(
            "Inter-relationship between sub-categories",
            mod_trend_overlap_ui("trend_overlap_ui")
          )
        )
      )

      # Theme categories

      ui_list <- c(
        ui_list,
        list(
          tabPanel(
            "What people are telling us about",
            mod_click_tables_ui("click_tables_ui")
          )
        )
      )
      
      # Comment search tab (key tab to show)

      ui_list <- c(
        ui_list,
        list(
          tabPanel(
            "Comment search",
            mod_search_text_ui("search_text_ui_1")
          )
        )
      )

      # Demographics tab

      if (isTruthy(get_golem_config("demography_1")) |
        isTruthy(get_golem_config("demography_2")) |
        isTruthy(get_golem_config("demography_3"))) {
        ui_list <- c(
          ui_list,
          list(
            tabPanel(
              "Who we are hearing from",
              mod_demographics_ui("demographics_ui_1")
            )
          )
        )
      }


      # summary tab
      ui_list <- c(
        ui_list,
        list(
          tabPanel(
            "Summary/Report builder",
            mod_summary_ui("summary_ui_1")
          )
        )
      )

      do.call(tabsetPanel, ui_list)
    })
  })
}
