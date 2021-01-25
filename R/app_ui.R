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
        
        
        
        dashboardHeader(title = "Service Experiences",
                        titleWidth = 300),
        dashboardSidebar(width = 300,
                         sidebarMenu(
                           
                           menuItem("Overview", 
                                    tabName = "overview",
                                    icon = shiny::icon("dashboard"),
                                    badgeLabel = "planned", badgeColor = "orange"),
                           
                           menuItem("Service user experiences", 
                                    tabName = "experiences-user",
                                    icon = shiny::icon("comment"),
                                    badgeLabel = "dev", badgeColor = "green"),
                           
                           # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black
                           
                           menuItem("Service user sentiment analysis", 
                                    tabName = "experiences-user-sentiment",
                                    icon = shiny::icon("smile"),
                                    badgeLabel = "dev", badgeColor = "green"), 
                          
                           menuItem("Staff experiences", 
                                    tabName = "experiences-staff",
                                    icon = shiny::icon("comment-medical"),
                                    badgeLabel = "planned", badgeColor = "orange"),
                           
                           menuItem("Complaints and compliments", 
                                    tabName = "complaints-compliments",
                                    icon = shiny::icon("exclamation-triangle"),
                                    badgeLabel = "planned", badgeColor = "orange"),
                           
                           menuItem("Generate report",
                                    tabName = "generate-report",
                                    icon = shiny::icon("file-alt"),
                                    badgeLabel = "planned", badgeColor = "orange"),
                           
                           menuItem("Info", tabName = "info", 
                                    icon = icon("info-circle")
                                    
                                    # , menuItem("Data", tabName = "info-data")
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
                    
                    
            ),
            
            tabItem(tabName = "experiences-user",
                    h1("Service User Experiences"),
                    mod_category_criticality_ui("category_criticality_ui_1")

            ),
            
            tabItem(tabName = "experiences-user-sentiment",
                    # h1("Sentiment Analysis of Service User Experiences"),
                    mod_sentiment_ui("mod_sentiment_ui_1")
                    ),
            
            tabItem(tabName = "experiences-staff",
                    h1("Staff Experiences")
            ),
            
            tabItem(tabName = "complaints-compliments",
                    h1("Complaints and Compliments")
            ),
            
            tabItem(tabName = "generate-report",
                    h1("Generate Report of Service Experiences")
            ),
            
            tabItem(tabName = "info",
                    h1("Further Information")
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

