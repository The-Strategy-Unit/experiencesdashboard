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
 
  )
}
    
#' category_criticality Server Functions
#'
#' @noRd 
mod_category_criticality_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_category_criticality_ui("category_criticality_ui_1")
    
## To be copied in the server
# mod_category_criticality_server("category_criticality_ui_1")
