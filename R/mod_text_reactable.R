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
    
    fluidRow(
      column(12,
             box(
               width = NULL,
               background = "light-blue",
               textOutput(ns("category_crit_table_txt"))
             )
      )
    ),
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
    
#' text_reactable Server Functions
#'
#' @noRd 
mod_text_reactable_server <- function(id, filter_data, filter_category){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # Create reactive data ----
    tidy_trust_data_r <- reactive({
      
      filter_data()$filter_data %>%
        dplyr::filter(category %in% filter_category())
    })
    
    # Create reactive table (best) ----
    output$best_table <- reactable::renderReactable({
      
      best_comments <- tidy_trust_data_r() %>% 
        tidyr::drop_na(crit) %>% 
        dplyr::filter(comment_type == "comment_2") %>% 
        dplyr::select(comment_txt, crit)
      
      # Trick so table is max 1000 rows, otherwise takes ages to load
      if (nrow(best_comments) >= 1000) {
        n_table_best <- 1000
      } else if (nrow(best_comments) < 1000) {
        n_table_best <- nrow(best_comments)
      }
      
      reactable::reactable(
        dplyr::sample_n(best_comments, n_table_best),
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
                                          name = "What was good?"),
          crit = reactable::colDef(minWidth = 1, 
                                   filterable = TRUE,
                                   name = "Criticality",
                                   cell = function(value) {
                                     class <- paste0("tag crit-best-", value)
                                     htmltools::div(class = class, value)
                                   }
          )
        )
      )
      
    })
    
    # Create reactive table (improve) ----
    output$improve_table <- reactable::renderReactable({
      
      improve_comments <- tidy_trust_data_r() %>% 
        tidyr::drop_na(crit) %>% 
        dplyr::filter(comment_type == "comment_1") %>% 
        dplyr::select(comment_txt, crit)
      
      # Trick so table is max 1000 rows, otherwise takes ages to load
      if (nrow(improve_comments) >= 1000) {
        n_table_imp <- 1000
      } else if (nrow(improve_comments) < 1000) {
        n_table_imp <- nrow(improve_comments)
      }
      
      reactable::reactable(
        dplyr::sample_n(improve_comments, n_table_imp),
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
    
    # Write output text for text boxes ----
    output$category_crit_time_plot_txt <- renderText({
      paste0("TODO NOTE: ADD INFORMATION TO GUIDE INTERPRETATION OF CHANGE IN SENTIMENT OVER TIME. EXPLAIN CRITICALITY. ADD INFORMATION EXPLAINING THE DIFFERENCE BETWEEEN TOTALS AND PROPORTIONS.")
    })
    
    output$category_crit_table_txt <- renderText({
      paste0("TODO NOTE: ADD INFORMATION TO GUIDE INTERPRETATION OF FEEDBACK COMMENTS. EXPLAIN CRITICALITY.")
    })
  })
}
