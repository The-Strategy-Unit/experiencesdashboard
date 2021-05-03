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
    
    fluidPage(
      
      # Add css file for table ----
      includeCSS(system.file("app/www/", "crit-table.css", 
                             package = "experiencesdashboard")),
      
      fluidRow(
        uiOutput(ns("categoryUI"))
      ),
      
      tabsetPanel(id = ns("tabset"),
                  type = "tabs",
                  tabPanel("Summary", value = "summary",
                           fluidRow(
                             column(6, h2("What could we improve?"),
                                    p("Click a row to see comments related to that category"),
                                    mod_click_tables_ui("click_tables_ui_1")),
                             column(6, h2("What did we do well?"),
                                    p("Click a row to see comments related to that category"),
                                    mod_click_tables_ui("click_tables_ui_2"))
                           )
                  ),
                  tabPanel("Comments", value = "comments",
                           br(),
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
                  ),
                  tabPanel("Timeline", value = "timeline",
                           br(),
                           fluidRow(
                             column(12,
                                    box(
                                      width = NULL,
                                      background = "light-blue",
                                      textOutput(ns("category_crit_time_plot_txt"))
                                    )
                             )
                           ),
                           fluidRow(
                             column(3,
                                    selectInput(ns("category_crit_time_facet"), 
                                                label = h5(strong("Divide plot by:")), 
                                                choices = list("Comment and category" = 1, 
                                                               "Comment and division" = 2), 
                                                selected = 1)
                             ),
                             column(3,
                                    selectInput(ns("category_crit_time_geom_histogram"), 
                                                label = h5(strong("Show proportion or total:")), 
                                                choices = list("Proportion" = "fill",
                                                               "Total" = "stack"), 
                                                selected = "stack")
                             )
                           ),
                           plotOutput(ns("category_crit_time_plot"))
                  )
      )
    )
  )
}

#' category_criticality Server Functions
#'
#' @noRd 
mod_category_criticality_server <- function(id, filter_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # create UI
    
    output$categoryUI <- renderUI({
      
      # don't show on the summary tab- doesn't do anything
      
      req(input$tabset != "summary")
      
      choices <- na.omit(unique(filter_data()$category))
      
      selectInput(
        session$ns("select_super"),
        label = h5(strong("Select categories:")),
        choices = choices,
        selected = sample(choices, 2),
        multiple = TRUE
      )
    })
    
    # Create reactive data ----
    tidy_trust_data_r <- reactive({
      
      filter_data() %>%
        dplyr::filter(category %in% input$select_super)
    })
    
    # Create sentiment plot over time ----
    output$category_crit_time_plot <- renderPlot({
      category_crit_time_plot <- tidy_trust_data_r() %>% 
        tidyr::drop_na(crit) %>% 
        tidyr::drop_na(category) %>% 
        ggplot2::ggplot(ggplot2::aes(x = date, 
                                     fill = factor(crit, exclude = NA))) +
        ggplot2::geom_histogram(position = input$category_crit_time_geom_histogram,
                                binwidth = 20) +
        ggplot2::scale_fill_viridis_d() +
        ggplot2::theme(text = ggplot2::element_text(size = 16))
      
      ## Add labels ----
      if (input$category_crit_time_geom_histogram == "stack") {
        category_crit_time_plot <- category_crit_time_plot + 
          ggplot2::labs(x = "Date", 
                        y = "Total number of responses", 
                        fill = "Criticality")
      } else if (input$category_crit_time_geom_histogram == "fill") {
        category_crit_time_plot <- category_crit_time_plot + 
          ggplot2::scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
          ggplot2::labs(x = "Date", 
                        y = "Proportion of responses", 
                        fill = "Criticality")
      }
      
      # Add facet ----
      if (input$category_crit_time_facet == 1) {
        category_crit_time_plot +
          ggplot2::facet_grid(category ~ factor(comment_type, 
                                                levels = c("best", 
                                                           "imp"),
                                                labels = c("What was good?", 
                                                           "What could we do better?"))
          )
      } else if (input$category_crit_time_facet == 2) {
        category_crit_time_plot +
          ggplot2::facet_grid(location_1 ~ factor(comment_type, 
                                                levels = c("best", 
                                                           "imp"),
                                                labels = c("What was good?", 
                                                           "What could we do better?"))
          )
      }}
      , height = function() {
        session$clientData$`output_category_criticality_ui_1-category_crit_time_plot_width` / 2.3
      }
    )
    
    # Create reactive table (best) ----
    output$best_table <- reactable::renderReactable({
      
      best_comments <- tidy_trust_data_r() %>% 
        tidyr::drop_na(crit) %>% 
        dplyr::filter(comment_type == "best") %>% 
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
        dplyr::filter(comment_type == "imp") %>% 
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

## To be copied in the UI
# mod_category_criticality_ui("category_criticality_ui_1")

## To be copied in the server
# mod_category_criticality_server("category_criticality_ui_1")
