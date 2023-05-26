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
        h5("Click a row to see comments related to that sub-category"),
        mod_click_tables_ui("click_tables_ui")
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
