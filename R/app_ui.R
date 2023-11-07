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
    dashboardPage(
      dashboardHeader(
        # title = "Qualitative Data Categorisation",
        titleWidth = 1000,
        mod_header_message_ui("messageMenu"),
        header_links()
      ),
      dashboardSidebar(
        width = 300,
        sidebarMenu(
          menuItem("Qualitative Data Categorisation",
            tabName = "experiences-user",
            icon = shiny::icon("comment"),
            selected = TRUE
          ),
          uiOutput("filter_location_1") |>
            shinycssloaders::withSpinner(),
          uiOutput("filter_location_2") |>
            shinycssloaders::withSpinner(),
          uiOutput("filter_location_3") |>
            shinycssloaders::withSpinner(),
          mod_demographics_selection_ui("demographics_selection_1") |>
            shinycssloaders::withSpinner(),
          uiOutput("date_filter_ui") |>
            shinycssloaders::withSpinner(),
          style = "color: black;" # ensure all text are black
        )
      ),
      dashboardBody(
        ### Changing theme ----
        fresh::use_theme(nhs_shiny_theme()), # use fresh object theme to style the whole dashboard
        # Leave this function for adding external resources
        golem_add_external_resources(),
        tabItems(
          tabItem(
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
  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "experiencesdashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
