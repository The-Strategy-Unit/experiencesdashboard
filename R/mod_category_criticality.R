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
    
    wellPanel(
      fluidRow(
        column(
          3,
          dateRangeInput(
            ns("date_range"),
            label = h5("Select date range:"),
            min = "2013-01-01",
            start = "2013-01-01",
            end = "2018-12-31",
            max = "2019-02-11"
          )
        ),
        
        column(
          3,
          selectInput(
            ns("select_division"),
            label = h5("Select divisions:"),
            choices = list(
              "Local partnerships- MH" = "Local partnerships- MH",
              "Forensic services" = "Forensic services",
              "Local partnerships- CH" = "Local partnerships- CH"
            ),
            multiple = TRUE,
            selected = c(
              "Local partnerships- MH",
              "Forensic services",
              "Local partnerships- CH"
            )
          )
        ),
        
        column(
          6,
          selectInput(
            ns("select_super"),
            label = h5("Select categories:"),
            choices = list(
              "Communication" = "Communication",
              "Staff/Staff Attitude" = "Staff/Staff Attitude",
              "Environment/Facilities" = "Environment/Facilities",
              "Access to Services" = "Access to Services",
              "Care/ Treatment" = "Care/ Treatment",
              "Couldn't be improved" = "Couldn't be improved",
              "Service Quality/Outcomes" = "Service Quality/Outcomes",
              "Involvement" = "Involvement",
              "Food" = "Food",
              "Privacy and Dignity" = "Privacy and Dignity",
              "MHA" = "MHA",
              "Equality/Diversity" = "Equality/Diversity",
              "Smoking" = "Smoking",
              "Leave" = "Leave",
              "Safety" = "Safety",
              "Physical Health" = "Physical Health",
              "Record Keeping" = "Record Keeping"
            ),
            selected = c("Staff/Staff Attitude", 
                         "Care/ Treatment"),
            multiple = TRUE
          )
        )
      ),
      style = "padding: 5px;"),
    
    tabsetPanel(
      type = "tabs",
      
      
      
      tabPanel("Distribution over time",
               
               br(),
               
               radioButtons(ns("category_crit_time_geom_histogram"), 
                            label = NULL,
                            choices = list("Show total number of responses" = "stack", 
                                           "Show percentage of respondes" = "fill"), 
                            selected = "stack"),
               
               plotOutput(ns("category_crit_time_plot"))
               ), 
      
      tabPanel("Show comments",
    
    fluidRow(
      column(6,
             reactable::reactableOutput(ns("best_table"))
      ),
      column(6,
             reactable::reactableOutput(ns("improve_table"))
             )
      )
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
      
      tidy_trust_data %>%
      dplyr::filter(date > input$date_range[1], date < input$date_range[2]) %>%
      dplyr::filter(division2 %in% input$select_division) %>%
      dplyr::filter(super_category %in% input$select_super)
      
    })
    
    
    
    # Create sentiment plot over time ----
    output$category_crit_time_plot <- renderPlot({
      
      tidy_trust_data_r() %>% 
        tidyr::drop_na(crit) %>% 
        ggplot2::ggplot(ggplot2::aes(x = date, fill = factor(crit))) +
        ggplot2::geom_histogram(position = input$category_crit_time_geom_histogram) +
        ggplot2::facet_grid(super_category ~ factor(comment_type, 
                                           levels = c("best", "improve"),
                                           labels = c("What was good?", "What could we do better?"))) +
        ggplot2::scale_fill_viridis_d() +
        ggplot2::labs(x = "Date", 
             y = "Number of responses", 
             fill = "Criticality") +
        ggplot2::theme(text = ggplot2::element_text(size = 16))
      
      
    
    }, height = function() {
      session$clientData$`output_category_criticality_ui_1-category_crit_time_plot_width` / 2.3
    })
    
    

    
    
    
    # Create reactive table ----
    output$best_table <- reactable::renderReactable({
      

      

      best_comments <- tidy_trust_data_r() %>% 
        tidyr::drop_na(crit) %>% 
        dplyr::filter(comment_type == "best") %>% 
        dplyr::select(comment_txt, crit)
      
      
      if (nrow(best_comments) >= 1000) {
        n_table_best <- 1000
      } else if (nrow(best_comments) < 1000) {
        n_table_best <- nrow(best_comments)
      }
      
      
      reactable::reactable(dplyr::sample_n(best_comments, n_table_best),
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
        dplyr::select(comment_txt, crit)
      
      
      if (nrow(improve_comments) >= 1000) {
        n_table_imp <- 1000
      } else if (nrow(improve_comments) < 1000) {
        n_table_imp <- nrow(improve_comments)
      }
    
      
      reactable::reactable(dplyr::sample_n(improve_comments, n_table_imp),
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
