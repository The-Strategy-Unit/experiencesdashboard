#' category_criticality UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_category_criticality_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    includeCSS("www/crit-table.css"), 
    
    fluidRow(
      column(6,
             reactable::reactableOutput(ns("best_table"))
      ),
      column(6,
             reactable::reactableOutput(ns("improve_table"))
             )
      )
 
  )
}
    
#' category_criticality Server Functions
#'
#' @noRd 
mod_category_criticality_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    
    tidy_trust_data_r <- reactive( {
      
      tidy_trust_data
      
      # %>% 
        # dplyr::filter(date > input$date_range[1], date < input$date_range[2]) %>% 
        # dplyr::filter(division2 %in% input$select_division) %>% 
        # dplyr::filter(super %in% input$select_super)
      
    })
    
    # Create reactive table ----
    output$best_table <- reactable::renderReactable({
      
      best_comments <- tidy_trust_data_r() %>% 
        tidyr::drop_na(crit) %>% 
        dplyr::filter(comment_type == "best") %>% 
        dplyr::select(comment_txt, crit) %>% 
        dplyr::sample_n(1000)
      
      reactable::reactable(best_comments,
                           borderless = TRUE,
                           highlight = TRUE,
                           showSortIcon = FALSE,
                           filterable = TRUE,
                           showPageSizeOptions = TRUE, 
                           pageSizeOptions = c(10, 15, 20, 25, 30), 
                           defaultPageSize = 10,
                           columns = list(
                             comment_txt = reactable::colDef(minWidth = 5.5, 
                                                             sortable = FALSE, 
                                                             name = "What was good?"),
                             crit = reactable::colDef(minWidth = 1, 
                                                      name = "Criticality",
                                                      cell = function(value) {
                                                        class <- paste0("tag crit-best-", value)
                                                        htmltools::div(class = class, value)
                                                      }
                             )
                           ))
      
    })
    
    
    output$improve_table <- reactable::renderReactable({
      
      improve_comments <- tidy_trust_data_r() %>% 
        tidyr::drop_na(crit) %>%
        dplyr::filter(comment_type == "improve") %>% 
        dplyr::select(comment_txt, crit) %>% 
        dplyr::sample_n(1000)
      
      reactable::reactable(improve_comments,
                           borderless = TRUE,
                           highlight = TRUE,
                           showSortIcon = FALSE,
                           filterable = TRUE,
                           showPageSizeOptions = TRUE, 
                           pageSizeOptions = c(10, 15, 20, 25, 30), 
                           defaultPageSize = 10,
                           columns = list(
                             comment_txt = reactable::colDef(minWidth = 5.5, 
                                                             sortable = FALSE, 
                                                             name = "What could we do better?"),
                             crit = reactable::colDef(minWidth = 1, 
                                                      name = "Criticality",
                                                      cell = function(value) {
                                                        class <- paste0("tag crit-imp-", value)
                                                        htmltools::div(class = class, value)
                                                      }
                                                      )
                           )
      )
      
    })
 
  })
}
    
## To be copied in the UI
# mod_category_criticality_ui("category_criticality_ui_1")
    
## To be copied in the server
# mod_category_criticality_server("category_criticality_ui_1")
