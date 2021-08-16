#' fft UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_fft_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    plotOutput(ns("spc_plot"))
  )
}
    
#' fft Server Functions
#'
#' @noRd 
mod_fft_server <- function(id, filter_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$spc_plot <- renderPlot({
      
      graph_data <- split_data_spc(filter_data()$filter_data, variable = "fft", 
                                   chunks = 15)
      
      plot_fft_spc(graph_data)
    })
  })
}
    