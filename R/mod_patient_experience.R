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
mod_patient_experience_server <- function(id, admin_user) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$dynamicUI <- renderUI({
      
      # key tabs to show

      ui_list <- list(

        # documentation tab
        tabPanel(
          "Understanding the categories",
          mod_documentation_page_ui("documentation_page")
        ),

        # summary tab
        tabPanel(
          "Categorised comments over time",
          mod_trend_ui("trend_ui_1")
        ),
        
        # Theme categories
        tabPanel(
          "Sub-categories people are telling us about",
          mod_click_tables_ui("click_tables_ui")
        )
      )
      
      # complex comment tab
      ui_list <- c(
        ui_list,
        list(
          tabPanel(
            "Complex comments",
            mod_complex_comments_ui("complex_comments_1")
          )
        )
      )
      
      # Comment search tab

      ui_list <- c(
        ui_list,
        list(
          tabPanel(
            "Comment search",
            mod_search_text_ui("search_text_ui_1")
          )
        )
      )

      # Theme Trend and overlap tab
      
      if (admin_user) {
        ui_list <- c(
          ui_list,
          list(
            tabPanel(
              "Inter-relationship between sub-categories",
              mod_trend_overlap_ui("trend_overlap_ui")
            )
          )
        )
      }

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
      
      # Data management tab
      
      if (admin_user) {
        ui_list <- c(
          ui_list,
          list(
            tabPanel(
              "Data upload and management",
              mod_data_management_ui("data_management_1")
            )
          )
        )
      }

      do.call(tabsetPanel, ui_list)
    })
  })
}
