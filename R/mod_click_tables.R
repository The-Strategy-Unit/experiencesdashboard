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
    
    DT::DTOutput(ns("table")) %>% shinycssloaders::withSpinner(),
    htmlOutput(ns("comments"))
  )
}

#' click_tables Server Functions
#'
#' @noRd 
mod_click_tables_server <- function(id, filter_data, comment_type){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    calculatedTable <- reactive({
      
      calculate_table(
        table_data = filter_data()$filter_data, 
        count_column = "category",
        comment_type = comment_type,
        click_column = NULL
      )
    })
    
    output$table <- DT::renderDT({
      
      calculated_table <- calculatedTable()
      
      DT::datatable(calculated_table,
                    selection = 'single', rownames = FALSE, 
                    extensions = 'Buttons', 
                    options = list(
                      pageLength = 10, lengthMenu = c(10, 15, 20, 50),
                      dom = 'Blfrtip',
                      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
                    ))
    })
    
    output$comments <- renderText({
      
      req(input$table_rows_selected)
      
      category_selected <- calculatedTable()$Category[input$table_rows_selected]
      
      final_text <- show_text(data = filter_data()$filter_data, 
                filter_by_column = "category", 
                filter_by_text = category_selected, 
                comment_type_filter = comment_type)
      
      return(final_text)
    })
    
    reactive(
      input$table_rows_selected
    )
  })
}
