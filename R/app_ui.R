#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # List the first level UI elements here
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(
        # title = "Qualitative Data Categorisation",
        titleWidth = 1000,
        # mod_header_message_ui("messageMenu"), # TODO: reinstate
        header_links()
      ),
      shinydashboard::dashboardSidebar(
        width = 300,
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Qualitative Data Categorisation",
            tabName = "experiences-user",
            icon = shiny::icon("comment"),
            selected = TRUE
          ),
          uiOutput("filter_location_1"),
          uiOutput("filter_location_2"),
          uiOutput("filter_location_3"),
          mod_demographics_selection_ui("demographics_selection_1"),
          uiOutput("date_filter_ui"),
          style = "color: black;" # ensure all text are black
        )
      ),
      shinydashboard::dashboardBody(
        ### Changing theme ----
        fresh::use_theme(nhs_shiny_theme()), # use fresh object theme to style the whole dashboard
        # Leave this function for adding external resources
        golem_add_external_resources(),
        shinydashboard::tabItems(
          shinydashboard::tabItem(
            tabName = "experiences-user",
            mod_patient_experience_ui("patient_experience_ui_1") |>
              shinycssloaders::withSpinner()
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "experiencesdashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
