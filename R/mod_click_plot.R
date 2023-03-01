#' click_Plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_click_plot_ui <- function(id){
  ns <- NS(id)
  
  uiOutput(ns('dynamic_click_plot_UI'))
  
}

#' click_tables Server Functions
#'
#' @noRd 
mod_click_plot_server <- function(id, filter_data, comment_type){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    clicked_data <- reactive(calculate_table(
      table_data = filter_data()$filter_data,
      count_column = "category",
      comment_type = comment_type
    ) %>% 
      dplyr::mutate(Category = reorder(Category, n))
    )
    
    global <- reactiveValues(
      to_highlight = NULL, 
      category_selected = NULL)
    
    output$dynamic_click_plot_UI <- renderUI({
      
      # code to track and highlight the selected bar in the plot
      global$to_highlight <- rep(FALSE, length(clicked_data()$Category))
      
      tagList(
        
        plotOutput(ns("comment_plot"), click = ns("comment_plot_click")),
        tags$br(),
        tags$hr(),
        htmlOutput(ns("comments_verbatim"))
        
        )
      
      })
    
    # the plot
    
    output$comment_plot <- renderPlot({
      
      clicked_data() %>% 
        ggplot2::ggplot(ggplot2::aes(x = n, y = Category, 
                   fill = rev(ifelse(global$to_highlight, yes = "yes", no = "no"))
                   )) +
        ggplot2::geom_bar(stat="identity", show.legend = FALSE) +
        ggplot2::scale_fill_manual(values = c("yes" = "darkblue", "no" = "grey" ), guide = FALSE ) + 
        ggplot2::theme(axis.text.x = ggplot2::element_text(face="bold", size=12),
              axis.text.y = ggplot2::element_text(face="bold", size=12)) +
        ggplot2::labs(x = 'No. of comments', y = NULL) 
      
    })
    
    observeEvent(input$comment_plot_click, {
      
      global$category_selected <- levels(clicked_data()$Category)[round(as.double(input$comment_plot_click$y))]
      global$to_highlight <- levels(clicked_data()$Category) %in% global$category_selected
  
      cat('selection index', round(as.double(input$comment_plot_click$y)), ' \n')   # for debugging 
      })
    
    # the comments
    
    output$comments_verbatim <- renderText({
      
      req(global$category_selected)
      
      cat("selected category:", global$category_selected, " \n")    # for debugging
      
      final_text <- show_text(data = filter_data()$filter_data, 
                filter_by_column = "category", 
                filter_by_text = global$category_selected, 
                comment_type_filter = comment_type)
      
      return(final_text)
    })
  })
}
