#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Initialize the database connection

  pool <- odbc::dbConnect(
    drv = odbc::odbc(),
    driver = Sys.getenv("odbc_driver"),
    server = Sys.getenv("HOST_NAME"),
    UID = Sys.getenv("DB_USER"),
    PWD = Sys.getenv("MYSQL_PASSWORD"),
    database = "TEXT_MINING",
    Port = 3306
  )

  # fetch  the data

  db_data <- dplyr::tbl(
    pool,
    dbplyr::in_schema(
      "TEXT_MINING",
      get_golem_config("trust_name")
    )
  ) %>%
    tidy_all_trusts()

  # find out if there is data in the table

  data_exists <- db_data %>%
    dplyr::tally() %>%
    dplyr::pull(n) > 0
  
  if (!data_exists){
    showModal(modalDialog(
      title = "NO DATA!",
      HTML("<strong>Please go to the 'Data Management' tab to upload data.</strong>"),
    ))
  }

  # store values of demographics and location_1 from last 3 years

  interpolate_date <- Sys.Date()

  if (get_golem_config("trust_name") != "demo_trust") {
    store_data <- db_data %>%
      dplyr::filter(date > interpolate_date - 3 * 365) %>%
      dplyr::select(dplyr::any_of(c(
        "location_1", "age",
        "gender", "ethnicity"
      ))) %>%
      dplyr::collect()
  }

  # render UI---

  output$filter_location_1 <- renderUI({
    req(get_golem_config("location_1"))
    req(data_exists)

    location_1_choices <- date_filter() %>%
      dplyr::distinct(location_1) %>%
      dplyr::mutate(location_1 = dplyr::na_if(location_1, "Unknown")) %>%
      dplyr::filter(!is.na(location_1))

    selectInput(
      "select_location_1",
      label = h5(strong(paste0(
        "Select ", get_golem_config("location_1"),
        " (defaults to all) :"
      ))),
      choices = sort(location_1_choices %>% dplyr::pull(location_1)),
      multiple = TRUE,
      selected = NULL
    )
  })

  output$filter_location_2 <- renderUI({
    req(get_golem_config("location_2"))
    req(data_exists)

    location_2_choices <- date_filter()

    if (isTruthy(input$select_location_1)) { # filter by location_1 if exists

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
    req(get_golem_config("location_3"))
    req(data_exists)

    location_3_choices <- date_filter()

    if (isTruthy(input$select_location_1)) { # filter by location_1 if exists

      location_3_choices <- location_3_choices %>%
        dplyr::filter(location_1 %in% !!input$select_location_1)
    }

    if (isTruthy(input$select_location_2)) { # filter by location_2 if exists

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
      "location_1" = input$select_location_1,
      "location_2" = input$select_location_2,
      "location_3" = input$select_location_3
    )
  })

  demographic_filters <- mod_demographics_selection_server("demographics_selection_1",
    filter_data = filter_data
  )

  # filter 1: by selected dates ----

  date_filter <- reactive({
    db_data %>%
      dplyr::filter(
        date > !!input$date_range[1],
        date < !!input$date_range[2]
      )
  })

  filter_data <- reactive({
    if (get_golem_config("trust_name") == "demo_trust") {
      return(list(
        "filter_data" = db_data %>% dplyr::collect(),
        "demography_number" = NULL
      ))
    }

    return_data <- date_filter()

    # filter 2: locations

    if (isTruthy(input$select_location_1)) {
      return_data <- return_data %>%
        dplyr::filter(location_1 %in% !!input$select_location_1)
    }

    if (isTruthy(input$select_location_2)) {
      return_data <- return_data %>%
        dplyr::filter(location_2 %in% !!input$select_location_2)
    }

    if (isTruthy(input$select_location_3)) {
      return_data <- return_data %>%
        dplyr::filter(location_3 %in% !!input$select_location_3)
    }

    # filter 3: demographics

    demography_data <- return_data 

    if (isTruthy(demographic_filters()$select_demography_1)) {
      demography_data <- demography_data %>%
        dplyr::filter(!!rlang::sym(get_golem_config("demography_1")) %in% !!demographic_filters()$select_demography_1)
    }

    if (isTruthy(demographic_filters()$select_demography_2)) {
      demography_data <- demography_data %>%
        dplyr::filter(!!rlang::sym(get_golem_config("demography_2")) %in% !!demographic_filters()$select_demography_2)
    }

    if (isTruthy(demographic_filters()$select_demography_3)) {
      demography_data <- demography_data %>%
        dplyr::filter(!!rlang::sym(get_golem_config("demography_3")) %in% !!demographic_filters()$select_demography_3)
    }

    # get the number of patients in data filtered by demographics

    no_responders <- demography_data %>%
      dplyr::distinct(pt_id) %>%
      dplyr::tally() %>%
      dplyr::pull(n)

    # only return demography filtered data if the number of responders is more than 20

    if (no_responders < 20 & data_exists) {
      return_data <- return_data %>%
        dplyr::collect() %>%
        dplyr::arrange(date)

      # add a pop up warning whenever any of the demographic filter is selected and
      # there are less than 20 responders in the data

      if ((isTruthy(demographic_filters()$select_demography_1)) |
        (isTruthy(demographic_filters()$select_demography_2)) |
        (isTruthy(demographic_filters()$select_demography_3))
      ) {
        showModal(modalDialog(
          title = "Warning!",
          paste0("There are only ", no_responders, " responders in your selection.
                 Filtering below 20 responders with demographic selections is disabled for
                 reasons of confidentiality. Please widen your selection by clinical area or demography"),
          easyClose = TRUE
        ))
      }
    } else {
      return_data <- demography_data %>%
        dplyr::collect() %>%
        dplyr::arrange(date)
    }

    # also return a dataset with unique individuals

    unique_data <- return_data %>%
      dplyr::distinct(pt_id, .keep_all = TRUE)
    
    # return the data in single labelled form 
    
    if (data_exists){
      tidy_filter_data <- return_data %>% 
        multi_to_single_label('category')  %>% 
        dplyr::select(-original_label, -name) %>% 
        dplyr::rename(category = value)
    } else {
      tidy_filter_data <- return_data
    }

    return(list(
      "filter_data" = return_data,
      "single_labeled_filter_data" = tidy_filter_data,
      "unique_data" = unique_data,
      "demography_number" = no_responders
    ))
  })

  # combine UI server ----

  mod_patient_experience_server("patient_experience_ui_1")

  # modules----
  
  mod_decumentation_page_server("decumentation_page_1")
  
  mod_summary_server("summary_ui_1")

  mod_summary_record_server("summary_record_1", db_data, filter_data)

  mod_data_management_server("data_management_1", db_conn = pool, filter_data)

  filter_category <- mod_category_criticality_server("category_criticality_ui_1",
    filter_data = filter_data
  )

  mod_fft_server("fft_ui_1", filter_data = filter_data)

  mod_report_builder_server("report_builder_ui_1",
    filter_data = filter_data,
    all_inputs = all_inputs
  )

  mod_click_tables_server("click_tables_ui_1",
    filter_data = filter_data,
    comment_type = "comment_1"
  )

  mod_click_tables_server("click_tables_ui_2",
    filter_data = filter_data,
    comment_type = "comment_2"
  )

  # mod_click_plot_server("click_plot_ui_1",
  #   filter_data = filter_data,
  #   comment_type = "comment_1",
  #   event_id = "click_plot_event_1"
  # )
  # 
  # mod_click_plot_server("click_plot_ui_2",
  #   filter_data = filter_data,
  #   comment_type = "comment_2",
  #   event_id = "click_plot_event_2"
  # )

  mod_text_reactable_server("text_reactable_ui_1",
    filter_data = filter_data,
    filter_category = filter_category,
    comment_select = "comment_1"
  )

  mod_text_reactable_server("text_reactable_ui_2",
    filter_data = filter_data,
    filter_category = filter_category,
    comment_select = "comment_2"
  )

  mod_search_text_server("search_text_ui_1",
    filter_data = filter_data
  )

  mod_trend_overlap_server("trend_overlap_ui",
    filter_data = filter_data, overlap_plot_type = "count"
  )

  mod_demographics_server("demographics_ui_1",
    filter_data = filter_data
  )
}
