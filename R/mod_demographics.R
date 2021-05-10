#' demographics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_demographics_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4, uiOutput(ns("age_UI"))),
      column(4, uiOutput(ns("gender_UI"))),
      column(4, uiOutput(ns("ethnicity_UI")))
    ),
    fluidRow(
      column(4, plotOutput(ns("age_graph"))),
      column(4, plotOutput(ns("gender_graph"))),
      column(4, plotOutput(ns("ethnicity_graph")))
    )
  )
}

#' demographics Server Functions
#'
#' @noRd 
mod_demographics_server <- function(id, filter_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # UI----
    
    output$age_UI <- renderUI({
      
      choices <- filter_data() %>% 
        dplyr::arrange(age) %>% 
        dplyr::distinct(age_label, .keep_all = TRUE) %>%
        dplyr::filter(!is.na(age_label))
      
      choices <- factor(choices$age, 
                        levels = choices$age, 
                        labels = choices$age_label, 
                        exclude = NULL)
      
      selectInput("select_age", label = "Select age (defaults to all)",
                  choices = na.omit(choices),
                  selected = NULL, multiple = TRUE)
    })
    
    output$gender_UI <- renderUI({
      
      choices <- filter_data() %>% 
        dplyr::arrange(gender) %>% 
        dplyr::distinct(gender)
      
      selectInput("select_gender", label = "Select gender (defaults to all)",
                  choices = na.omit(choices),
                  selected = NULL, multiple = TRUE)
    })
    
    output$ethnicity_UI <- renderUI({
      
      choices <- filter_data() %>% 
        dplyr::arrange(ethnicity) %>% 
        dplyr::distinct(ethnicity)
      
      selectInput("select_ethnicity", label = "Select ethnicity (defaults to all)",
                  choices = na.omit(choices),
                  selected = NULL, multiple = TRUE)
    })
    
  })
}
