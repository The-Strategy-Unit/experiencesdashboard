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
      
      # if(count_column == "Category"){
      #   
      #   req(click())
      #   
      #   first_table <- calculate_table(table_data = filter_data(),
      #                                  code_column = "Code",
      #                                  category_table = data()$categories,
      #                                  join_lookup = c("Code" = "Number"),
      #                                  count_column = "Super", 
      #                                  click_column = NULL)
      #   
      #   row_selected <- first_table$Category[click()]
      #   
      #   calculated_table <- calculate_table(table_data = filter_data(),
      #                                       code_column = "Code",
      #                                       category_table = data()$categories,
      #                                       join_lookup = c("Code" = "Number"),
      #                                       count_column = count_column, 
      #                                       click_column = row_selected)
      # } else {
      
      calculated_table <- calculate_table(table_data = filter_data(),
                                          code_column = "Code",
                                          category_table = data()$categories,
                                          join_lookup = c("Code" = "Number"),
                                          count_column = count_column, 
                                          click_column = click)
      # }
      
      DT::datatable(calculated_table,
                    selection = 'single', rownames = FALSE, extensions = 'Buttons', 
                    options = list(pageLength = 5, lengthMenu = c(5, 10),
                                   dom = 'Blfrtip',
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
    })
    
    reactive(
      input$themes_rows_selected
    )
  })
}
