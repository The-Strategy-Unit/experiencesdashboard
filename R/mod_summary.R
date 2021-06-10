#' summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_summary_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      
      fluidRow(
        actionButton(ns("launch_modal"), "Launch modal window")
      ),
      
      fluidRow(
        column(
          width = 8,
          tags$b("Imported data:"),
          DT::DTOutput(ns("show_data"))
        )
      )
    )
  )
}

#' summary Server Functions
#'
#' @noRd 
mod_summary_server <- function(id, db_conn){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    observeEvent(input$launch_modal, {
      datamods::import_modal(
        id = session$ns("myid"),
        from = "file",
        title = "Import data to be used in application"
      )
    })
    
    imported <- datamods::import_server("myid", return_class = "tbl_df")
    
    # output$show_data <- DT::renderDT({
    #   imported$data()
    # })
    
    observe({
      
      req(imported$data())
      
      raw_df <- imported$data()
      
      DBI::dbWriteTable(db_conn, "trust_d", raw_df, append = TRUE)
    })
  })
}
