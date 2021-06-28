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
        actionButton(ns("launch_modal"), "Upload new data")
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
    
    observe({
      
      req(imported$data())
      
      raw_df <- imported$data()
      
      raw_df <- raw_df %>% 
        dplyr::filter(!is.na(date))
      
      success <- upload_data(data = raw_df, conn = db_conn, 
                             trust_id = get_golem_config("trust_name"))
      
      if(success){
        
        showModal(modalDialog(
          title = "Success!",
          paste0(nrow(raw_df), " records successfully imported. Please refresh 
               your browser to access the new data"),
          easyClose = TRUE
        ))
      } else {
        
        showModal(modalDialog(
          title = "Error!",
          "There was a problem importing your data. Try reuploading and check 
          in the 'View' section to ensure that the data is well formatted",
          easyClose = TRUE
        ))
      }
    })
  })
}
