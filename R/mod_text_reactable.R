#' text_reactable UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_text_reactable_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    reactable::reactableOutput(ns("table"))
  )
}

#' text_reactable Server Functions
#'
#' @noRd 
mod_text_reactable_server <- function(id, filter_data, filter_category,
                                      comment_select){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Create reactive data ----
    tidy_trust_data_r <- reactive({
      
      return_data <- filter_data()$filter_data
      
      if(isTruthy(filter_category())){
        
        return_data <- return_data %>% 
          dplyr::filter(category %in% filter_category())
      }
      
      return_data
    })
    
    # Create reactive table ----
    output$table <- reactable::renderReactable({
      
      table_comments <- tidy_trust_data_r() %>% 
        tidyr::drop_na(crit) %>% 
        dplyr::filter(comment_type == comment_select) %>% 
        dplyr::select(comment_txt, crit)
      
      reactable::reactable(
        table_comments, # %>% dplyr::sample_n(n_table_best),
        borderless = TRUE,
        highlight = TRUE,
        showSortIcon = FALSE,
        showPageSizeOptions = TRUE,
        pageSizeOptions = c(10, 15, 20, 25, 30),
        defaultPageSize = 10,
        columns = list(
          comment_txt = reactable::colDef(minWidth = 5.5, 
                                          sortable = FALSE, 
                                          filterable = TRUE,
                                          name = ""),
          crit = reactable::colDef(minWidth = 1, 
                                   filterable = TRUE,
                                   name = "Criticality",
                                   cell = function(value) {
                                     class <- paste0("tag crit_", value)
                                     htmltools::div(class = class, value)
                                   }
          )
        )
      )
    })
  })
}
