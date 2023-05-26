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
      p("Add multiple search terms with comma"),
        textInput(ns("text_search"), "Search term(s)",
                  placeholder = "e.g. staff, doctor, nurse"),
      hr(),
      DT::DTOutput(ns("comment_output"))
    )
  )
}

#' search_text Server Functions
#'
#' @noRd 
mod_search_text_server <- function(id, filter_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    memoised_comment_table <- memoise::memoise(comment_table, cache = session$cache) # create a session-level cacheable version of comment_table()
    output$comment_output <- DT::renderDT({
      
      validate(
        need(input$text_search, "Please enter a search term")
      )
      return_search_text(text_data = filter_data()$filter_data, 
                       filter_text = input$text_search, 
                       comment_type_filter = NULL, search_type='and') %>% 
        memoised_comment_table(no_super_category=TRUE)
    })
  })
}
