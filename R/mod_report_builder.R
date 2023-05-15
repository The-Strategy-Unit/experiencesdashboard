#' report_builder UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_report_builder_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidRow(
      column(6,
             selectInput(ns("time_period"), "Reporting period",
                         choices = c("Previous quarter" = "quarter",
                                     "Previous 12 months" = "year",
                                     "Current selection" = "custom")),
             
             uiOutput(ns("report_componentsUI")),
             
             downloadButton(ns("download_report"),
                            "Download report")
      )
    )
  )
}

#' report_builder Server Functions
#'
#' @noRd 
mod_report_builder_server <- function(id, filter_data,
                                      all_inputs){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$report_componentsUI <- renderUI({
      
      choices = c("% categories table" = "category_table",
                  "Verbatim comments" = "verbatim_comments")
      
      # do we have demographic data?
      
      demographic_ui <- isTruthy(get_golem_config("gender")) | 
        isTruthy(get_golem_config("age")) | 
        isTruthy(get_golem_config("ethnicity"))
      
      if(demographic_ui){
        
        choices = c(choices, "Sample demographics" = "sample_demographics")
      }
      
      # do we have FFT data?
      
      if(isTruthy(get_golem_config("question_1"))){
        
        choices = c(choices, "FFT graph" = "fft_graph")
      }
      
      # draw final choices

      checkboxGroupInput(
        session$ns("report_components"), "Report features",
        choices = choices,
        selected = c("category_table", "fft_graph"))
    })
    
    output$download_report <- downloadHandler(
      filename = paste0("CustomReport_", Sys.Date(), ".docx"),
      content = function(file){
        
        # check they asked for something
        
        if(is.null(input$report_components)){
          
          showModal(
            modalDialog(
              title = "Error!",
              HTML("Please select something to report on!"),
              easyClose = TRUE
            )
          )
        }
        
        # check there is enough data
        
        else if(nrow(filter_data()$filter_data) < 10){
          
          showModal(
            modalDialog(
              title = "Error!",
              HTML("Not enough data. Please expand your selection"),
              easyClose = TRUE
            )
          )
        }
        
        # calculate parameters
        else{
          
          withProgress(message = "Preparing report, Please wait...", value = 0, {
            
            
            dates <- switch(input$time_period,
                            quarter = previous_quarter(Sys.Date()),
                            year = c(Sys.Date(), Sys.Date() - 365),
                            custom = c(all_inputs()$date_from[1],
                                       all_inputs()$date_to[2])
            )
            
            params <- list(dates = dates,
                           inputs = all_inputs(),
                           data = filter_data()$filter_data,
                           single_label_data = filter_data()$single_labeled_filter_data,
                           options = input$report_components,
                           comment_1 = get_golem_config("comment_1"),
                           comment_2 = get_golem_config("comment_2")
            )
            
            rmarkdown::render(
              system.file("app", "www", "report.Rmd",
                          package = "experiencesdashboard"), 
              output_format = "word_document",
              output_file = here::here(app_sys(), "app/www", "report.docx"),
              # output_dir =
              quiet = TRUE, params = params,
              envir = new.env(parent = globalenv())
            )
            
            # copy docx to 'file'
            file.copy(here::here(app_sys(), "app/www", "report.docx"),
                      file, overwrite = TRUE)
            
            incProgress(1)
            })
          }
        }
      )
  })
}
