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
  tagList(
    
    plotOutput(ns("comment_plot"), click = ns("comment_plot_click")),
    tags$hr(),
    htmlOutput(ns("comments_verbatim"))
    
  )
}

#' click_tables Server Functions
#'
#' @noRd 
mod_click_plot_server <- function(id, filter_data, comment_type){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
      clicked_data <- reactive(
        calculate_table(
        table_data = filter_data,
        count_column = "category",
        comment_type = comment_type) %>% 
        dplyr::mutate(Category = reorder(Category, n))
        )
    
    # code to track and highlight the selected bar in the plot
    
    isolate(
      global <- reactiveValues(
      to_highlight = rep(FALSE, length(clicked_data()$Category)), 
      category_selected = NULL)
    )
    
    observeEvent(input$comment_plot_click, {
      
      global$category_selected <- levels(clicked_data()$Category)[round(as.double(input$comment_plot_click$y))]
      global$to_highlight <- levels(clicked_data()$Category) %in% global$category_selected
  
      cat('selection index', round(as.double(input$comment_plot_click$y)), ' \n')   # for debugging 
      })
    
    # the plot
    
    output$comment_plot <- renderPlot({
      
      clicked_data() %>% 
        ggplot(aes(x = n, y = Category, 
                   fill = rev(ifelse(global$to_highlight, yes = "yes", no = "no"))
                   )) +
        geom_bar(stat="identity", show.legend = FALSE) +
        scale_fill_manual(values = c("yes" = "darkblue", "no" = "grey" ), guide = FALSE ) +
        ggplot2::labs(x = 'No. of comments', y = NULL) 
      
    })
    
    # the comments
    
    output$comments_verbatim <- renderText({
      
      req(global$category_selected)
      
      cat("selected category:", global$category_selected, " \n")    # for debugging
      
      final_text <- show_text(data = filter_data, 
                filter_by_column = "category", 
                filter_by_text = global$category_selected, 
                comment_type_filter = comment_type)
      
      return(final_text)
    })
    
    # reactive(
    #   input$comment_plot_click
    # )
  })
}


# App ###########
library(shiny)
library(tidyverse, warn.conflicts=F)
library(DT)
library(shinydashboard)

# UI
ui <- function(){
  tagList(
    dashboardPage(
      # header (title)
      dashboardHeader(title = "PAT table",
                      titleWidth = 300),

      # sidebar
      shinydashboard::dashboardSidebar(
        # shinyFeedback::useShinyFeedback(),
        # shinyjs::useShinyjs(),
        sidebarMenu(
          menuItem('maininput',
                   tabName = 'tabs',
                   icon = shiny::icon("comment"),
                   selected = TRUE,
                   badgeLabel = "dev", badgeColor = "blue")
        )

      ),

      # body
      shinydashboard::dashboardBody(
        tabsetPanel(
          tabPanel('tab1',
                   mod_click_plot_ui('click_1')
          ),
        )
      ),
      skin = 'red'
    )
  )
}

# SERVER
server <- function( input, output, session
){
  df_1 <- readr::read_csv(here::here('tests/test_data.csv'), show_col_types = FALSE)
  df_1 <- df_1 %>% dplyr::rename(age = age, genders = gender)
  # cat(colnames(df))
  filter_data = df_1
  mod_click_plot_server('click_1', filter_data, 'comment_1')
}

shinyApp(ui, server)




