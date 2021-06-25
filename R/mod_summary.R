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
      
      preds <- experienceAnalysis::calc_predict_unlabelled_text(
        x = raw_df,
        python_setup = FALSE,
        text_col_name = 'comment',
        preds_column = NULL,
        column_names = "all_cols",
        pipe_path = 'fitted_pipeline.sav'
      ) %>% 
        dplyr::select(code = comment_preds)
      
      criticality <- experienceAnalysis::calc_predict_unlabelled_text(
        x = raw_df,
        python_setup = FALSE,
        text_col_name = 'comment',
        preds_column = NULL,
        column_names = "all_cols",
        pipe_path = 'pipeline_criticality.sav'
      ) %>% 
        dplyr::select(criticality = comment_preds)
      
      final_df <- dplyr::bind_cols(
        raw_df, 
        preds,
        criticality)
      
      final_df <- final_df %>% 
        dplyr::mutate(criticality = dplyr::case_when(
          code == "Couldn't be improved" ~ "3",
          TRUE ~ criticality
        ))

      DBI::dbWriteTable(db_conn, get_golem_config("trust_name"),
                        final_df, append = TRUE)
      
      showModal(modalDialog(
        title = "Success!",
        paste0(nrow(final_df), " records successfully imported"),
        easyClose = TRUE
      ))
    })
  })
}
