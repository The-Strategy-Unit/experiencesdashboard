#' click_tables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_click_tables_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    DT::DTOutput(ns("table")),
    textOutput(ns("comments"))
  )
}

#' click_tables Server Functions
#'
#' @noRd 
mod_click_tables_server <- function(id, data, count_column, click, filter_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$table <- DT::renderDT({
      
      calculated_table <- calculate_table(
        table_data = tidy_trust_data, 
        count_column = "super_category",
        comment_type = "improve", 
        click_column = NULL)

      DT::datatable(calculated_table,
                    selection = 'single', rownames = FALSE, extensions = 'Buttons', 
                    options = list(pageLength = 10, lengthMenu = c(10, 15, 20, 50),
                                   dom = 'Blfrtip',
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    })
    
    reactive(
      input$table_rows_selected
    )
  })
}
