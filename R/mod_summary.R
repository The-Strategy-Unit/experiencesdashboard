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
      
      h1("Overview"),
      
      uiOutput(ns("summary_text")),

      # conditionalPanel(
      #   get_golem_config("trust_name") == "demo_trust",
      #   uiOutput(ns("open_panel"))
      # ),
      
      # fluidRow(
      actionButton(ns("launch_modal"), "Upload new data")
      # )
    )
  )
}

#' summary Server Functions
#'
#' @noRd 
mod_summary_server <- function(id, db_conn, db_data, filter_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # summary
    
    output$summary_text <- renderUI({
      
      n_responses <- db_data %>% 
        dplyr::filter(!is.na(comment_txt)) %>% 
        dplyr::tally() %>% 
        dplyr::pull(n)
      
      n_individuals <- db_data %>% 
        dplyr::distinct(pt_id) %>% 
        dplyr::tally() %>% 
        dplyr::pull(n)
      
      current_responses <- filter_data()$filter_data %>% 
        dplyr::filter(!is.na(comment_txt)) %>% 
        dplyr::tally() %>% 
        dplyr::pull(n)
      
      current_individuals <- filter_data()$filter_data %>% 
        dplyr::distinct(pt_id) %>% 
        dplyr::tally() %>% 
        dplyr::pull(n)
      
      tagList(

      p(glue::glue("There are {n_responses} responses in the database from 
                 {n_individuals} individuals.")),
      
      p(glue::glue("The current selected data comprises {current_responses} 
                   responses in the database from {current_individuals} 
                   individuals."))
      )
    })
    
    # UI
    
    output$open_panel <- renderUI({
      
      # if(get_golem_config("trust_name") != "demo_trust"){
      #   
      #   return()
      # }
      
      tagList(
        h3("Download the spreadsheet template below and add your data to it"),
        
        downloadButton(session$ns("open_spreadsheet"), "Download template"),
        
        h3("Then click upload data below")
      )
    })
    
    # download spreadsheet
    
    output$open_spreadsheet <- downloadHandler(
      
      filename = "template.xlsx",
      content = function(file) {
        file.copy("text_mining_template_open.xlsx", file)
      }
    )
    
    # data module
    
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
      
      withProgress(message = 'Processing data. This may take a while. 
                   Please wait...', value = 0, {

        success <- upload_data(data = raw_df, conn = db_conn, 
                               trust_id = get_golem_config("trust_name"))
        
        incProgress(1)
      })

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
