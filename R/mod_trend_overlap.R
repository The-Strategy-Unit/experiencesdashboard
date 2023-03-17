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
                 plotly::plotlyOutput(ns("category_trend_plot")) %>% 
                   shinycssloaders::withSpinner()
        ),
        
        # A Sub-tab
        
        tabPanel("Theme Overlap", value = "overlap",
                 br(),
                 fluidRow(
                   column(12,
                          plotly::plotlyOutput(ns("category_overlap_plot")) %>% 
                            shinycssloaders::withSpinner(),
                          hr(),
                          uiOutput(ns("dynamic_overlap_text"))
                          )
                   )
                 )
        )
    )
  )
}
    
#' trend_overlap Server Functions
#'
#' @noRd 
mod_trend_overlap_server <- function(id, filter_data, 
                                     overlap_plot_type =c('count', 'correlation')){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    global <- reactiveValues(selected_cat1 = NULL, selected_cat2 = NULL)
    
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
    
    # Verbatim output
    
    output$dynamic_overlap_text <- renderUI({
      
      validate(
        need(plotly::event_data("plotly_click", source = 'overlap_plot', 
                                priority = 'event'), 
             "Please select the categories to view")
      )
      
      tagList(
        paste(toupper(global$selected_cat2),'<==>',toupper(global$selected_cat1)),
        hr(),
        htmlOutput(ns("overlap_text"))
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
    
    # trend plot
    
    output$category_trend_plot <- plotly::renderPlotly({
      
      p <- trend_data() %>% 
        ggplot2::ggplot(ggplot2::aes(x = date, y = prop, 
                                     color = category, group = category)) +
        ggplot2::geom_line() +
        ggplot2::geom_point() +
        ggplot2::scale_x_date(date_breaks = "4 months", date_labels = "%b %y") +
        ggplot2::scale_y_continuous(label = scales::label_percent(accuracy = 1)) +
        ggplot2::scale_colour_viridis_d() +
        ggplot2::facet_grid(. ~ comment_type) +
        ggplot2::labs(x= NULL, y = '% contribution', color = "Category") #+
        # ggplot2::theme(text = ggplot2::element_text(size = 16)) 
        
      return(
        p %>%
          plotly::ggplotly(tooltip = c('colour', 'text')) %>%
          plotly::config(displayModeBar = FALSE)
      )
      
    })
    
    # Create tidy data for the overlapping plot
    
    tidy_data <- reactive({
      
       filter_data()$filter_data %>%
        make_sample_multilabeled_data() %>% 
        multi_to_single_label(column_name = 'labels')
    })
    
    output$category_overlap_plot <- plotly::renderPlotly({
      
      clicked_data <- tidy_data()  %>% 
        make_overlap_theme(group_type = overlap_plot_type) %>% 
        reshape_overlapping_theme()
      
      clicked_data %>%
        interactive_heatmap(overlap_plot_type, source = 'overlap_plot')
    })
    
    
    output$overlap_text <- renderText({
      
      req(plotly::event_data("plotly_click", source = 'overlap_plot', priority = 'event'))
          
      clickData <- plotly::event_data("plotly_click", source = 'overlap_plot', priority = 'event')
      
      global$selected_cat1 <- clickData$x
      global$selected_cat2 <- clickData$y 
                                           
      final_text <- show_multilabeled_text(tidy_data(), 'value', 
                                           c(global$selected_cat1, global$selected_cat2))
      
      cat(paste(global$selected_cat1),'<==>', toupper(global$selected_cat2), ' :: ', 
          length(final_text), ' \n')
      
      return(final_text)
      
    })
  })
}
