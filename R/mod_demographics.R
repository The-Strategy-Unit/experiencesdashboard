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

  uiOutput(ns("dynamic_demo_UI"))
}

#' demographics Server Functions
#'
#' @noRd 
mod_demographics_server <- function(id, filter_data, store_data){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # the UI render
    
    output$dynamic_demo_UI <- renderUI({
      
      # check which demographic variables are present
      
      isolate({
        has_age <- 'age' %in% colnames(filter_data()$unique_data)
        has_gender <- 'gender' %in% colnames(filter_data()$unique_data)
        has_ethnicity <- 'ethnicity' %in% colnames(filter_data()$unique_data)
      })
      
      # determine the column width base on the number of demographic variables present
      
      demography_cond <- sum(has_age,has_gender,has_ethnicity)
      
      width <- dplyr::case_when(
        demography_cond == 3 ~ 4,
        demography_cond == 2 ~ 6,
        TRUE ~ 12
      )
      
      tagList(
        fluidRow(
          column(12, h3(textOutput(ns("total_responses"))))
        ),
        hr(),
        fluidRow(
          if(has_age){column(width, uiOutput(ns("age_UI")))},
          if(has_gender){column(width, uiOutput(ns("gender_UI")))},
          if(has_ethnicity){column(width, uiOutput(ns("ethnicity_UI")))}
        ),
        fluidRow(
          if(has_age){column(width, plotOutput(ns("age_graph")))},
          if(has_gender){column(width, plotOutput(ns("gender_graph")))},
          if(has_ethnicity){column(width, plotOutput(ns("ethnicity_graph")))}
        ),
        hr(),
        h3("Categories with fewer than 10 individuals are excluded"),
        fluidRow(
          if(has_age){column(width, plotOutput(ns("compare_age")))},
          if(has_gender){column(width, plotOutput(ns("compare_gender")))},
          if(has_ethnicity){column(width, plotOutput(ns("compare_ethnicity")))}
        )
      )
    })
    
    # top row
    
    output$total_responses <- renderText({
      
      no_responses <- filter_data()$demography_number
      
      if(no_responses < 20){
        
        return(paste0("There are only " , no_responses, " responses in your 
                      selection. Filtering below 20 responses with demographic 
                      selections is disabled for reasons of confidentiality. 
                      Please widen your selection by clinical area or 
                      demography"))
      } else {
        
        return(paste0("There is a total of ", 
                      no_responses, " responses in your selection"))
      }
    })
    
    # demography selection----
    
    output$age_UI <- renderUI({
      
      isolate(
        choices <- filter_data()$unique_data %>% 
          dplyr::arrange(age) %>% 
          dplyr::distinct(age, .keep_all = TRUE) %>%
          dplyr::filter(!is.na(age))
      )
      
      choices <- factor(choices$age, 
                        exclude = NULL)
      
      selectInput(session$ns("select_age"), 
                  label = "Select age (defaults to all)",
                  choices = na.omit(choices),
                  selected = NULL, multiple = TRUE)
    })
    
    output$gender_UI <- renderUI({
      
      isolate(
        choices <- filter_data()$unique_data %>% 
          dplyr::arrange(gender) %>% 
          dplyr::distinct(gender)
      )
      
      selectInput(session$ns("select_gender"), 
                  label = "Select gender (defaults to all)",
                  choices = na.omit(choices),
                  selected = NULL, multiple = TRUE)
    })
    
    output$ethnicity_UI <- renderUI({
      
      isolate(
        choices <- filter_data()$unique_data %>% 
          dplyr::arrange(ethnicity) %>% 
          dplyr::distinct(ethnicity)
      )
      
      selectInput(session$ns("select_ethnicity"), 
                  label = "Select ethnicity (defaults to all)",
                  choices = na.omit(choices),
                  selected = NULL, multiple = TRUE)
    })
    
    # distribution----
    
    output$age_graph <- renderPlot({
      
      filter_data()$unique_data %>% 
        dplyr::arrange(age) %>% 
        dplyr::mutate(age = factor(age, 
                                   exclude = NULL)) %>% 
        demographic_distribution(variable = "age")
    })
    
    output$gender_graph <- renderPlot({
      
      filter_data()$unique_data %>% 
        demographic_distribution(variable = "gender")
    })
    
    output$ethnicity_graph <- renderPlot({
      
      filter_data()$unique_data %>% 
        demographic_distribution(variable = "ethnicity")
    })
    
    # compare scores----
    
    output$compare_age <- renderPlot({
      
      compare_demographics(filter_data()$unique_data, "age")
    })
    
    output$compare_gender <- renderPlot({
      
      compare_demographics(filter_data()$unique_data, "gender")
    })
    
    output$compare_ethnicity <- renderPlot({
      
      compare_demographics(filter_data()$unique_data, "ethnicity")
    })
    
    reactive(
      list("select_age" = input$select_age,
           "select_gender" = input$select_gender,
           "select_ethnicity" = input$select_ethnicity)
    )
  })
}
