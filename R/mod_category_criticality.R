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
        
        # A Sub-tab
        
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
        
        # A Sub-tab
        
        tabPanel("Summary Plot", value = "summary2",
                 fluidRow(
                   column(6, h2(get_golem_config("comment_1")),
                          p("Click a bar to see comments related to that category"),
                          mod_click_plot_ui("click_plot_ui_1")),
                   
                   if(isTruthy(get_golem_config("comment_2"))){
                     
                     column(6, h2(get_golem_config("comment_2")),
                            p("Click a bar to see comments related to that category"),
                            mod_click_plot_ui("click_plot_ui_2"))
                   }
                 )
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
    
    reactive(
      input$select_super
    )
  })
}
