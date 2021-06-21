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
                                      comment_type){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Create reactive data ----
    tidy_trust_data_r <- reactive({
      
      filter_data()$filter_data %>%
        dplyr::filter(category %in% filter_category())
    })
    
    # Create reactive table ----
    output$table <- reactable::renderReactable({
      
      table_comments <- tidy_trust_data_r() %>% 
        tidyr::drop_na(crit) %>% 
        dplyr::filter(comment_type == comment_type) %>% 
        dplyr::select(comment_txt, crit)
      
      # # Trick so table is max 1000 rows, otherwise takes ages to load
      # if (nrow(best_comments) >= 1000) {
      #   n_table_best <- 1000
      # } else if (nrow(best_comments) < 1000) {
      #   n_table_best <- nrow(best_comments)
      # }
      
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
