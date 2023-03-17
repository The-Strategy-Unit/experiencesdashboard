#' category_criticality UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_category_criticality_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      
      # Add css file for table ----
      includeCSS(system.file("app/www/", "crit-table.css", 
                             package = "experiencesdashboard")),
      
      fluidRow(
        column(6, h2(get_golem_config("comment_1")),
               p("Click a bar to see comments related to that category"),
               mod_click_plot_ui("click_plot_ui_1")),
        
        if(isTruthy(get_golem_config("comment_2"))){
          
          column(6, h2(get_golem_config("comment_2")),
                 p("Click a bar to see comments related to that category"),
                 mod_click_plot_ui("click_plot_ui_2"))
        }
      )
    )
  )
}

#' category_criticality Server Functions
#'
#' @noRd 
mod_category_criticality_server <- function(id, filter_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
  })
}
