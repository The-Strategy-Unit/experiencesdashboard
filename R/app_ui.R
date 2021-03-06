#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    tagList(
      # Leave this function for adding external resources
      golem_add_external_resources(),
      # List the first level UI elements here
      dashboardPage(
        
        dashboardHeader(title = "Patient experience",
                        titleWidth = 300),
        dashboardSidebar(width = 300,
                         sidebarMenu(
                           
                           menuItem("Summary", 
                                    tabName = "overview",
                                    icon = shiny::icon("dashboard")
                                    # , badgeLabel = "planned", badgeColor = "orange"
                           ),
                           
                           menuItem("Patient experience", 
                                    tabName = "experiences-user",
                                    icon = shiny::icon("comment"),
                                    selected = TRUE,
                                    badgeLabel = "dev", badgeColor = "green"),
                           
                           # red, yellow, aqua, blue, light-blue, green, navy, teal, 
                           # olive, lime, orange, fuchsia, purple, maroon, black
                           
                           menuItem("Staff experience", 
                                    tabName = "experiences-staff",
                                    icon = shiny::icon("comment-medical"),
                                    badgeLabel = "planned", badgeColor = "orange"
                           ),
                           
                           menuItem("Info", tabName = "info", 
                                    icon = icon("info-circle"), 
                                    menuItem("Data", tabName = "info-data")
                           ),
                           dateRangeInput(
                             "date_range",
                             label = h5(strong("Select date range:")),
                             min = "2013-01-01",
                             start = "2013-01-01",
                             end = "2018-12-31",
                             max = "2019-02-11"
                           ),
                           selectInput(
                             "select_division",
                             label = h5(strong("Select divisions:")),
                             choices = list(
                               "Local partnerships- MH" = "Local partnerships- MH",
                               "Forensic services" = "Forensic services",
                               "Local partnerships- CH" = "Local partnerships- CH"
                             ),
                             multiple = TRUE,
                             selected = c(
                               "Local partnerships- MH",
                               "Forensic services",
                               "Local partnerships- CH"
                             )
                           )
                         )
        ),
        dashboardBody(
          ### Changing theme
          dashboardthemes::shinyDashboardThemes(
            theme = "grey_light"
          ),
          
          tabItems(
            # First tab content
            tabItem(tabName = "overview",
                    h1("Overview"),
                    h2("NOTE: WORK IN PROGRESS"),
                    p("This tab will contain information that summarises all available data- 
                    staff and patient experience, clinical outcomes, and risk")
                    
            ),
            
            tabItem(tabName = "experiences-user",
                    # h1("Service User Experiences"),
                    mod_category_criticality_ui("category_criticality_ui_1")
                    
            ),
            
            tabItem(tabName = "experiences-staff",
                    h1("Staff experiences"),
                    h2("NOTE: WORK IN PROGRESS")
            ),
            
            tabItem(tabName = "info",
                    h1("Further Information"),
                    h2("NOTE: WORK IN PROGRESS"),
                    p("This tab will contain information about the measures used in the 
                    dashboard, as well as other relevant information")
                    
            )
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
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'experiencesdashboard'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

