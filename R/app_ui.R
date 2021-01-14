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
        
        
        
        dashboardHeader(title = "Service User Experiences",
                        titleWidth = 300),
        dashboardSidebar(width = 300,
                         sidebarMenu(
                           
                           menuItem("Service User Experiences", 
                                    tabName = "dashboard-aggregated-experiences-user",
                                    icon = shiny::icon("comment"),
                                    badgeLabel = "dev", badgeColor = "green"),
                           
                           # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black
                           
                           menuItem("Service User Sentiment Analysis", 
                                    tabName = "dashboard-aggregated-experiences-sentiment",
                                    icon = shiny::icon("smile"),
                                    badgeLabel = "dev", badgeColor = "green"), 
                          
                           menuItem("Staff Experiences", 
                                    tabName = "dashboard-aggregated-experiences-staff",
                                    icon = shiny::icon("comment-medical"),
                                    badgeLabel = "planned", badgeColor = "orange"),
                           
                           menuItem("Complaints and Compliments", 
                                    tabName = "dashboard-aggregated-complaints-compliments",
                                    icon = shiny::icon("exclamation-triangle"),
                                    badgeLabel = "planned", badgeColor = "orange"),
                           
                           menuItem("Generate Report",
                                    tabName = "dashboard-aggregated-report",
                                    icon = shiny::icon("file-alt"),
                                    badgeLabel = "planned", badgeColor = "orange"),
                           
                           menuItem("Info", tabName = "info", 
                                    icon = icon("info-circle")))
                         ),
        dashboardBody(
          ### Changing theme
          dashboardthemes::shinyDashboardThemes(
            theme = "grey_light"
          ),
          
          tabItems(
            # First tab content
            tabItem(tabName = "dashboard-aggregated-overview",
                    h1("Aggregated Overview"),
                    
                    dateRangeInput("dateRange",
                                   label = h5("Select Date Range:"),
                                   start = Sys.Date() - 2, end = Sys.Date() + 2),
                    
                    selectInput("select", label = h5("Select Division"),
                                choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3),
                                selected = 1)
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

