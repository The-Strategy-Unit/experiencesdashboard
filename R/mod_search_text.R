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
        uiOutput(ns("comment_1_UI")),
        uiOutput(ns("comment_2_UI"))
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
    
    output$comment_1_UI <- renderUI({
      
      tagList(
        column(6,
               h2(get_golem_config("comment_1")),
               htmlOutput(ns("comment_1_output"))
        )
      )
    })
    
    output$comment_2_UI <- renderUI({
      
      if(isTruthy(get_golem_config("comment_2"))){
        tagList(
          column(6,
                 h2(get_golem_config("comment_2")),
                 htmlOutput(ns("comment_2_output"))
          )
        )
      } else {
        return()
      }
    })
    
    output$comment_1_output <- renderText({
      
      validate(
        need(input$text_search, "Please enter a search term")
      )
      
      returnSearchText(text_data = filter_data(), 
                       filter_text = input$text_search, 
                       comment_type_filter = "comment_1")
    })
    
    output$comment_2_output <- renderText({
      
      validate(
        need(input$text_search, "Please enter a search term")
      )
      
      returnSearchText(text_data = filter_data(), 
                       filter_text = input$text_search, 
                       comment_type_filter = "comment_2")
    })
  })
}
