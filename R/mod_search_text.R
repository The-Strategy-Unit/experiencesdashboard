#' search_text UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_search_text_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      p("Text searches are combined with OR- searching AND will be in a 
      future release"),
      p("Add multiple search terms with comma"),
      fluidRow(
        textInput(ns("text_search"), "Search term(s)",
                  placeholder = "e.g. staff, doctor, nurse")
      ),
      fluidRow(
        column(6,
               h2("What could we improve?"),
               htmlOutput(ns("imp_text"))
        ),
        column(6,
               h2("What did we do well?"),
               htmlOutput(ns("best_text"))
        )
      )
    )
  )
}

#' search_text Server Functions
#'
#' @noRd 
mod_search_text_server <- function(id, filter_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$imp_text <- renderText({
      
      validate(
        need(input$text_search, "Please enter a search term")
      )
      
      returnSearchText(text_data = filter_data(), 
                       filter_text = input$text_search, 
                       comment_type_filter = "improve")
    })
    
    output$best_text <- renderText({
      
      validate(
        need(input$text_search, "Please enter a search term")
      )
      
      returnSearchText(text_data = filter_data(), 
                       filter_text = input$text_search, 
                       comment_type_filter = "best")
    })
  })
}
