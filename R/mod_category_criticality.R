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
      
      tabsetPanel(
        id = ns("tabset"),
        type = "tabs",
        tabPanel("Summary", value = "summary",
                 fluidRow(
                   column(6, h2(get_golem_config("comment_1")),
                          p("Click a row to see comments related to that category"),
                          mod_click_tables_ui("click_tables_ui_1")),
                   
                   if(isTruthy(get_golem_config("comment_2"))){
                     
                     column(6, h2(get_golem_config("comment_2")),
                            p("Click a row to see comments related to that category"),
                            mod_click_tables_ui("click_tables_ui_2"))
                   }
                 )
        ),
        tabPanel("Comments", value = "comments",
                 fluidRow(
                   column(6, h2(get_golem_config("comment_1")),
                          mod_text_reactable_ui("text_reactable_ui_1")),
                   
                   if(isTruthy(get_golem_config("comment_2"))){
                     
                     column(6, h2(get_golem_config("comment_2")),
                            mod_text_reactable_ui("text_reactable_ui_2"))
                   }
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
                          uiOutput(ns("dividePlotUI")),
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
      
      choices <- na.omit(unique(filter_data()$filter_data$category))
      
      selectInput(
        session$ns("select_super"),
        label = h5(strong("Select categories (defaults to all):")),
        choices = choices,
        multiple = TRUE
      )
    })
    
    output$dividePlotUI <- renderUI({
      
      choices <- list(1, 2) # wait for it...
      
      names(choices) <- c("Comment and category",  
                          paste0("Comment and ", 
                                 get_golem_config("location_1")))
      
      selectInput(
        ns("category_crit_time_facet"), 
        label = h5(strong("Divide plot by:")), 
        choices = choices, 
        selected = 1)
    })
    
    # Create reactive data ----
    tidy_trust_data_r <- reactive({
      
      return_data <- filter_data()$filter_data
      
      if(isTruthy(input$select_super)){
        
        return_data <- return_data %>%
          dplyr::filter(category %in% input$select_super)
      }
      
      return_data
      
    })
    
    # Create sentiment plot over time ----
    output$category_crit_time_plot <- renderPlot({
      
      category_crit_time_plot <- tidy_trust_data_r() %>% 
        tidyr::drop_na(crit) %>% 
        tidyr::drop_na(category) %>% 
        dplyr::mutate(
          comment_type = factor(comment_type, levels = unique(comment_type),
                                labels = c(get_golem_config("comment_1"),
                                           get_golem_config("comment_2"))))
      
      category_crit_time_plot <- category_crit_time_plot %>% 
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
          ggplot2::facet_grid(category ~ comment_type)
        
      } else if (input$category_crit_time_facet == 2) {
        category_crit_time_plot +
          ggplot2::facet_grid(location_1 ~ comment_type)
      }}
      , height = function() {
        session$clientData$`output_category_criticality_ui_1-category_crit_time_plot_width` / 2.3
      })
    
    reactive(
      input$select_super
    )
  })
}
