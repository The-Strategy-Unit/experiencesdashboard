#' trend_overlap UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_trend_overlap_ui <- function(id){
  ns <- NS(id)
  tagList(
    
    fluidPage(
      
      fluidRow(
        uiOutput(ns("trendUI"))
      ),
      
      tabsetPanel(
        id = ns("tabset_trend"),
        type = "tabs",
        
        # A Sub-tab
        
        tabPanel("Trend", value = "trend",
                 br(),
                 plotOutput(ns("category_trend_plot")) %>% shinycssloaders::withSpinner()
        ),
        
        # A Sub-tab
        
        tabPanel("Theme Overlap", value = "overlap",
                 br(),
                 br(),
                 fluidRow(
                   column(2),
                   column(8, 
                          plotOutput(ns("category_overlap_plot")) %>% shinycssloaders::withSpinner()),
                   column(2)
                   )
                 )
        )
    )
  )
}
    
#' trend_overlap Server Functions
#'
#' @noRd 
mod_trend_overlap_server <- function(id, filter_data, overlap_plot_type =c('count', 'correlation')){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$trendUI <- renderUI({
      
      # show only on the trend tab
      
      req(input$tabset_trend == "trend")
      
      choices <- na.omit(unique(filter_data()$filter_data$category))
      
      selectInput(
        session$ns("select_super_category"),
        label = h5(strong("Select categories (defaults to all):")),
        choices = choices,
        multiple = TRUE
      )
      
    })
    
    # Create category trend
    
    trend_data <- reactive({
      
      return_data <- filter_data()$filter_data %>% 
        dplyr::mutate(
          comment_type = factor(comment_type, levels = unique(comment_type),
                                labels = c(get_golem_config("comment_1"),
                                           get_golem_config("comment_2")))) %>%
        dplyr::mutate(date = as.Date(cut(date, "month"))) %>%
        dplyr::group_by(comment_type, date, category) %>%
        dplyr::summarize(cat = dplyr::n()) %>%
        dplyr::group_by(comment_type, date) %>%
        dplyr::mutate(prop = round(cat/sum(cat), 2)) %>%
        dplyr::filter(!is.na(category)) 
      
      if(isTruthy(input$select_super_category)){
        
        return_data <- return_data %>%
          dplyr::filter(category %in% input$select_super_category)
      }
      
      return_data
      
    })
    
    output$category_trend_plot <- renderPlot({
      
      trend_data() %>% 
        ggplot2::ggplot(ggplot2::aes(x=date, y=prop, color=category, group=category)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::scale_x_date(date_breaks = "4 months", date_labels = "%b %y") +
        ggplot2::scale_y_continuous(label = scales::label_percent(accuracy = 1)) +
        ggplot2::scale_colour_viridis_d() +
        ggplot2::facet_grid(. ~ comment_type) +
        ggplot2::labs(x= NULL, y = '% contribution', color = "Category") +
        ggplot2::theme(text = ggplot2::element_text(size = 16)) 
      
    })
    
    # Add overlapping plot
    output$category_overlap_plot <- renderPlot({
      
      
      set.seed(2017)
      
      filter_data()$filter_data %>% 
        make_sample_multilabeled_data() %>% 
        multi_to_single_label(column_name = 'labels') %>% 
        make_overlap_theme(group_type = overlap_plot_type) %>% 
        reshape_overlapping_theme() %>%
        overlap_heatmap_plot(legend = overlap_plot_type)
      
    })
    
    reactive(
      input$select_super_category
    )
 
  })
}
