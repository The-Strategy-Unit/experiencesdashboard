#' patient_experience UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_patient_experience_ui <- function(id){
  ns <- NS(id)
  
  uiOutput(ns("dynamicUI"))
  
}

#' patient_experience Server Functions
#'
#' @noRd 
mod_patient_experience_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$dynamicUI <- renderUI({
      
      # Summary and Report tab (key tabs to show)
      
      ui_list <- list(
        
        # documentation tab
        tabPanel(
          "Data Categorisation Framework",
          mod_decumentation_page_ui("decumentation_page_1")
        ),
        
        # summary tab
        tabPanel(
          "Summary",
          mod_summary_ui("summary_ui_1")
        ),
        
        # Data management tab
        
        tabPanel(
          "Data Management",
          mod_data_management_ui("data_management_1")
        )
      )
      
      # Theme categories 
      
      ui_list <- c(ui_list,
                   list(
                     tabPanel(
                       "Themes/weighting",
                       mod_category_criticality_ui("category_criticality_ui_1")
                     )
                   )
      )
      
      # Theme Trend and overlap tab      
      
      ui_list <- c(ui_list, 
                   list(
                     tabPanel(
                       "Trend/Overlap",
                       mod_trend_overlap_ui("trend_overlap_ui")
                     )
                   ))
      
      # Comment search tab (key tab to show)
      
      ui_list <- c(ui_list, 
                   list(
                     tabPanel(
                       "Comment search",
                       mod_search_text_ui("search_text_ui_1")
                     )
                   ))
      
      # Demographics tab
      
      if(isTruthy(get_golem_config("demography_1")) | 
         isTruthy(get_golem_config("demography_2")) | 
         isTruthy(get_golem_config("demography_3"))){
        
        ui_list <- c(ui_list,
                     list(
                       tabPanel(
                         "Demographics",
                         mod_demographics_ui("demographics_ui_1")
                       ))
        )
      }
      
      do.call(tabsetPanel, ui_list)
      
    })
  })
}
