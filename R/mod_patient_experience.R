#' patient_experience UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_patient_experience_ui <- function(id){
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel(
        "Summary",
        p("This module will contain a high level summary of PX data"),
      ),
      tabPanel(
        "Report builder",
        p("This is the report builder tab")
      ),
      tabPanel(
        "FFT",
        p("This module will summarise scores- FFT and scores +ve/ -ve assigned to comments")
      ),
      tabPanel(
        "Themes",
        p("This module will summarise the themes"),
        mod_category_criticality_ui("category_criticality_ui_1")
      ),
      tabPanel(
        "Weighting",
        p("This module will summarise sentiment/ weighting")
      ),
      tabPanel(
        "Comment search",
        p("This module will show and search raw text")
      )
    )
  )
}
    
#' patient_experience Server Functions
#'
#' @noRd 
mod_patient_experience_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# 
    
## To be copied in the server
# 
