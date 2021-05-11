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
    ),
    hr(),
    fluidRow(
      column(4, plotOutput(ns("compare_age")))
    )
  )
}

#' demographics Server Functions
#'
#' @noRd 
mod_demographics_server <- function(id, filter_data, store_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # UI----
    
    output$age_UI <- renderUI({
      
      choices <- store_data %>% 
        dplyr::arrange(age) %>% 
        dplyr::distinct(age_label, .keep_all = TRUE) %>%
        dplyr::filter(!is.na(age_label))
      
      choices <- factor(choices$age, 
                        levels = choices$age, 
                        labels = choices$age_label, 
                        exclude = NULL)
      
      selectInput(session$ns("select_age"), 
                  label = "Select age (defaults to all)",
                  choices = na.omit(choices),
                  selected = NULL, multiple = TRUE)
    })
    
    output$gender_UI <- renderUI({
      
      choices <- store_data %>% 
        dplyr::arrange(gender) %>% 
        dplyr::distinct(gender)
      
      selectInput(session$ns("select_gender"), 
                  label = "Select gender (defaults to all)",
                  choices = na.omit(choices),
                  selected = NULL, multiple = TRUE)
    })
    
    output$ethnicity_UI <- renderUI({
      
      choices <- store_data %>% 
        dplyr::arrange(ethnicity) %>% 
        dplyr::distinct(ethnicity)
      
      selectInput(session$ns("select_ethnicity"), 
                  label = "Select ethnicity (defaults to all)",
                  choices = na.omit(choices),
                  selected = NULL, multiple = TRUE)
    })
    
    # distribution----
    
    output$age_graph <- renderPlot({
      
      filter_data() %>% 
        dplyr::arrange(age) %>% 
        dplyr::mutate(age = factor(age, 
                                   levels = age, 
                                   labels = age_label, 
                                   exclude = NULL)) %>% 
        dplyr::count(age) %>% 
        tidyr::replace_na(list(age = "Unknown")) %>% 
        ggplot2::ggplot(ggplot2::aes(x = age, y = n)) + 
        ggplot2::geom_col() + 
        ggplot2::xlab("Age") + 
        nottshcMethods::theme_nottshc()
    })
    
    output$gender_graph <- renderPlot({
      
      filter_data() %>% 
        dplyr::arrange(gender) %>% 
        dplyr::count(gender) %>% 
        tidyr::replace_na(list(gender = "Unknown")) %>% 
        ggplot2::ggplot(ggplot2::aes(x = gender, y = n)) + 
        ggplot2::geom_col() + 
        ggplot2::xlab("Gender") + 
        nottshcMethods::theme_nottshc()
    })
    
    # compare scores----
    
    output$compare_age <- renderPlot({
      
      filter_data() %>% 
        dplyr::filter(!is.na(age_label)) %>% 
        dplyr::group_by(age_label) %>% 
        dplyr::summarise(fft = mean(fft, na.rm = TRUE),
                         listening = mean(listening, na.rm = TRUE),
                         communication = mean(communication, na.rm = TRUE),
                         respect = mean(respect, na.rm = TRUE),
                         inv_care = mean(inv_care, na.rm = TRUE),
                         positive_q = mean(positive_q, na.rm = TRUE),
                         n = dplyr::n()) %>% 
        dplyr::filter(n > 10) %>% 
        dplyr::mutate(dplyr::across(where(is.numeric), ~ round(. * 20, 1))) %>% 
        dplyr::select(-n) %>% 
        tidyr::pivot_longer(-age_label) %>% 
        ggplot2::ggplot(ggplot2::aes(x = age_label, y = value, 
                                     group = name, fill = name)) + 
        ggplot2::geom_col(position = "dodge") + nottshcMethods::theme_nottshc() +
        ggplot2::ylab("%") + ggplot2::ylim(0, 100)
    })
    
    reactive(
      list("select_age" = input$select_age,
           "select_gender" = input$select_gender,
           "select_ethnicity" = input$select_ethnicity)
    )
  })
}
