#' complex_comments UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_complex_comments_ui <- function(id){
  ns <- NS(id)
  tagList(
    colored_h4(strong('Criterial for selecting complex comments'))
 
  )
}
    
#' complex_comments Server Functions
#'
#' @noRd 
mod_complex_comments_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_complex_comments_ui("complex_comments_1")
    
## To be copied in the server
# mod_complex_comments_server("complex_comments_1")
