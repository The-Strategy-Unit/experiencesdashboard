#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  # fetch data
  
  pool <- odbc::dbConnect(drv = odbc::odbc(),
                       driver = "Maria DB",
                       server = Sys.getenv("HOST_NAME"),
                       UID = Sys.getenv("DB_USER"),
                       PWD = Sys.getenv("MYSQL_PASSWORD"),
                       database = "TEXT_MINING",
                       Port = 3306)
  
  db_data <- dplyr::tbl(pool,
                        dbplyr::in_schema("TEXT_MINING",
                                          get_golem_config("trust_name"))) %>%
    tidy_all_trusts(conn = pool, trust_id = get_golem_config("trust_name"))
  
  # vector of sentiment names
  
  nrc_sentiments <- sentiment_nrc %>%
    dplyr::select(sentiment) %>%
    dplyr::distinct() %>%
    dplyr::pull() %>%
    sort()
  
  # store values of demographics and location_1 from last 3 years
  
  interpolate_date <- Sys.Date()
  
  if(get_golem_config("trust_name") != "demo_trust"){
    
    store_data <- db_data %>% 
      dplyr::filter(date > interpolate_date - 3 * 365) %>%
      dplyr::select(dplyr::any_of(c("location_1", "age", "age_label", 
                                    "gender", "ethnicity"))) %>%
      dplyr::collect()
  }
  
  # render UI---
  
  output$filter_location_1 <- renderUI({
    
    if(get_golem_config("trust_name") == "demo_trust"){
      
      return()
    }
    
    location_1_choices <- date_filter() %>%
      dplyr::distinct(location_1) %>%
      dplyr::mutate(location_1 = dplyr::na_if(location_1, "Unknown")) %>%
      dplyr::filter(!is.na(location_1))
    
    selectInput(
      "select_location_1",
      label = h5(strong(paste0("Select ", get_golem_config("location_1"),
                               " (defaults to all) :"))),
      choices = sort(location_1_choices %>% dplyr::pull(location_1)),
      multiple = TRUE,
      selected = NULL
    )
  })
  
  output$filter_location_2 <- renderUI({
    
    if(get_golem_config("trust_name") == "demo_trust"){
      
      return()
    }
    
    location_2_choices <- date_filter()
    
    if(isTruthy(input$select_location_1)){ # filter by location_1 if exists
      
      location_2_choices <- location_2_choices %>% 
        dplyr::filter(location_1 %in% !!input$select_location_1)
    }
    
    location_2_choices <- location_2_choices %>%
      dplyr::distinct(location_2) %>%
      dplyr::mutate(location_2 = dplyr::na_if(location_2, "Unknown")) %>%
      dplyr::filter(!is.na(location_2))
    
    selectInput(
      "select_location_2",
      label = h5(strong(paste0("Select ", get_golem_config("location_2"), " :"))),
      choices = sort(location_2_choices %>% dplyr::pull(location_2)),
      multiple = TRUE,
      selected = NULL
    )
  })
  
  output$filter_location_3 <- renderUI({
    
    if(get_golem_config("trust_name") == "demo_trust"){
      
      return()
    }
    
    location_3_choices <- date_filter()
    
    if(isTruthy(input$select_location_1)){ # filter by location_1 if exists
      
      location_3_choices <- location_3_choices %>%
        dplyr::filter(location_1 %in% !!input$select_location_1)
    }
    
    if(isTruthy(input$select_location_2)){ # filter by location_2 if exists
      
      location_3_choices <- location_3_choices %>%
        dplyr::filter(location_2 %in% !!input$select_location_2)
    }
    
    location_3_choices <- location_3_choices %>%
      dplyr::distinct(location_3) %>%
      dplyr::mutate(location_3 = dplyr::na_if(location_3, "Unknown")) %>%
      dplyr::filter(!is.na(location_3))
    
    selectInput(
      "select_location_3",
      label = h5(strong(paste0("Select ", get_golem_config("location_3"), " :"))),
      choices = sort(location_3_choices %>% dplyr::pull(location_3)),
      multiple = TRUE,
      selected = NULL
    )
  })
  
  all_inputs <- reactive({
    list(
      "date_from" = input$date_range[1],
      "date_to" = input$date_range[2],
      "location_1" = input$select_location_1
    )
  })
  
  # Create reactive data ----
  
  date_filter <- reactive({
    
    db_data %>% 
      dplyr::filter(date > !!input$date_range[1],
                    date < !!input$date_range[2])
  })
  filter_data <- reactive({
    
    if(get_golem_config("trust_name") == "demo_trust"){
      
      return(list("filter_data" = db_data %>% dplyr::collect(), 
                  "demography_number" = NULL))
    }
    
    return_data <- date_filter()
    
    # filter location
    
    if(isTruthy(input$select_location_1)){
      
      return_data <- return_data %>% 
        dplyr::filter(location_1 %in% !!input$select_location_1)
    }
    
    if(isTruthy(input$select_location_2)){
      
      return_data <- return_data %>% 
        dplyr::filter(location_2 %in% !!input$select_location_2)
    }
    
    if(isTruthy(input$select_location_3)){
      
      return_data <- return_data %>% 
        dplyr::filter(location_3 %in% !!input$select_location_3)
    }
    
    # filter demographics
    
    demography_data <- return_data
    
    if(isTruthy(demographic_filters()$select_age)){
      
      demography_data <- demography_data %>% 
        dplyr::filter(age_label %in% !!demographic_filters()$select_age)
    }
    
    if(isTruthy(demographic_filters()$select_gender)){
      
      demography_data <- demography_data %>% 
        dplyr::filter(gender %in% !!demographic_filters()$select_gender)
    }
    
    if(isTruthy(demographic_filters()$select_ethnicity)){
      
      demography_data <- demography_data %>% 
        dplyr::filter(gender %in% !!demographic_filters()$select_gender)
    }
    
    no_responses <- demography_data %>% 
      dplyr::tally() %>% 
      dplyr::pull(n)
    
    if(no_responses < 20){
      
      return_data <- return_data %>%
        dplyr::collect() %>% 
        dplyr::arrange(date)
    } else {
      
      return_data <- demography_data %>% 
        dplyr::collect() %>% 
        dplyr::arrange(date)
    }
    
    demography_number <- demography_data %>% 
      dplyr::tally() %>% 
      dplyr::pull(n)
    
    return(list("filter_data" = return_data, 
                "demography_number" = demography_number))
  })
  
  filter_sentiment <- reactive({
    
    calc_sentiment(filter_data()$filter_data, nrc_sentiments)
    
  })
  
  # modules----
  
  mod_summary_server("summary_ui_1", db_conn = pool)
  
  # patient experience modules----
  
  mod_patient_experience_server("patient_experience_ui_1")
  
  mod_sentiment_server("mod_sentiment_ui_1", 
                       filter_sentiment = filter_sentiment,
                       nrc_sentiments = nrc_sentiments)
  
  filter_category <- mod_category_criticality_server("category_criticality_ui_1", 
                                                     filter_data = filter_data)
  
  mod_fft_server("fft_ui_1", filter_data = filter_data)
  
  mod_report_builder_server("report_builder_ui_1",
                            filter_sentiment = filter_sentiment,
                            filter_data = filter_data,
                            all_inputs = all_inputs)
  
  mod_click_tables_server("click_tables_ui_1",
                          filter_data = filter_data,
                          comment_type = "comment_1")
  
  mod_click_tables_server("click_tables_ui_2",
                          filter_data = filter_data,
                          comment_type = "comment_2")
  
  mod_text_reactable_server("text_reactable_ui_1", 
                            filter_data = filter_data,
                            filter_category = filter_category,
                            comment_type = "comment_1")
  
  mod_search_text_server("search_text_ui_1",
                         filter_data = filter_data)
  
  demographic_filters <- mod_demographics_server("demographics_ui_1",
                                                 filter_data = filter_data,
                                                 store_data = store_data)
}
