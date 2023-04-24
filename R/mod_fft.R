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
    
    uiOutput(ns("dynamic_fft"))
  )
}
    
#' fft Server Functions
#'
#' @noRd 
mod_fft_server <- function(id, filter_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # confirm if there are at least 10 groups (potential data points) in the data before potting 
    output$dynamic_fft <- renderUI({
      
      if (no_group() < 9){
        
        fluidRow(
          tags$br(),
          column(12, tags$strong('There are not enough stable SPC points to plot.
                      Please expand your selection') 
          )
        )
        
      } else{
        
        plotOutput(ns("spc_plot")) %>% 
          shinycssloaders::withSpinner()
      }
    })
      
    graph_data <- reactive({
      
      tryCatch({
        
        split_data_spc(filter_data()$unique_data, variable = "fft", chunks = 'monthly')
        
      }, error = function(e){
        paste(e)
      })
      
    })
    
    no_group <- reactive({
      
       graph_data() %>% 
        dplyr::pull() %>% 
        unique() %>% 
        length()
    })
      
    output$spc_plot <- renderPlot({
      
      req(no_group() > 9) 
      
      plot_fft_spc(graph_data())
      
    })
  })
}
    