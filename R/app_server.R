#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  # fetch data
  
  pool <- pool::dbPool(drv = odbc::odbc(),
                       driver = "Maria DB",
                       server = Sys.getenv("HOST_NAME"),
                       UID = Sys.getenv("DB_USER"),
                       PWD = Sys.getenv("MYSQL_PASSWORD"),
                       database = "TEXT_MINING",
                       Port = 3306)
  
  db_data <- dplyr::tbl(pool, 
                        dbplyr::in_schema("TEXT_MINING", get_golem_config("trust_name"))) %>% 
    tidy_px_exp(conn = pool, trust_id = get_golem_config("trust_name"))
  
  # render ALL the inputs
  
  output$filter_dataUI <- renderUI({

    dates <- db_data %>%
      dplyr::summarise(min_date = min(date),
                       max_date = max(date)) %>%
      dplyr::collect()

    location_1_choices <- db_data %>%
      dplyr::distinct(location_1) %>%
      dplyr::mutate(location_1 = dplyr::na_if(location_1, "Unknown")) %>%
      dplyr::filter(!is.na(location_1)) %>%
      dplyr::pull(location_1)

    tagList(
      selectInput(
        "select_division",
        label = h5(strong("Select divisions:")),
        choices = location_1_choices,
        multiple = TRUE,
        selected = location_1_choices
      ),
      dateRangeInput(
        "date_range",
        label = h5(strong("Select date range:")),
        start = dates$min_date,
        end = dates$max_date
      )
    )
  })
  
  all_inputs <- reactive({
    list(
      "date_from" = input$date_range[1],
      "date_to" = input$date_range[2],
      "division" = input$select_division
    )
  })
  
  # Create reactive data ----
  filter_data <- reactive({
    
    db_data %>%
      dplyr::filter(date > !!input$date_range[1],
                    date < !!input$date_range[2]) %>%
      dplyr::filter(location_1 %in% !!input$select_division) %>%
      dplyr::collect() %>% 
      dplyr::arrange(date)
  })
  
  filter_sentiment <- reactive({
    
    sentiment_txt_data %>%
      dplyr::filter(date > input$date_range[1],
                    date < input$date_range[2]) %>%
      dplyr::filter(location_1 %in% input$select_division)
  })
  
  mod_patient_experience_server("patient_experience_ui_1")
  
  mod_sentiment_server("mod_sentiment_ui_1", filter_sentiment = filter_sentiment)
  
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
  
  mod_text_reactable_server("text_reactable_ui_1", filter_data = filter_data,
                            filter_category = filter_category)
  
  mod_search_text_server("search_text_ui_1",
                         filter_data = filter_data)
}
