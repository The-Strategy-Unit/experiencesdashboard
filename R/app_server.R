#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # decide which golem active config to use
  # get value from session data if it hasn't been set before calling run app()
  cat("Session group:", session$groups, " \n")
  if (Sys.getenv("R_CONFIG_ACTIVE") == "rsconnect") {
    Sys.setenv("R_CONFIG_ACTIVE" = set_trust_config(session$groups))
  }
  cat("Trust name:", get_golem_config("trust_name"), " \n")

  # Initialize the database connection
  pool <- get_pool()

  # fetch  the data
  db_data <- get_db_data(pool, get_golem_config("trust_name"))

  # find out if there is data in the table
  data_exists <- db_data %>%
    dplyr::tally() %>%
    dplyr::pull(n) > 0

  if (!data_exists) {
    showModal(modalDialog(
      title = strong("WELCOME!!!"),
      HTML(paste(
        h2(strong("Welcome to the Patient Experience Qualitative Data Categorisation Dashboard."),
          style = "text-align:center"
        ),
        h4("To start Using the dashboard, you need to upload your Trust data. After doing this you will get a success message and you can
          refresh your browser to start exploring your data."),
        h4(HTML(paste0("To get started, Please go to the", strong(em(("'Data Management'"))), "tab to upload your data")))
      ))
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

  # add date filter derived from the db data
  output$date_filter_ui <- renderUI({
    dateRangeInput(
      "date_range",
      label = h5(strong("Select date range:")),
      start = min(dplyr::pull(db_data, date) %>% na.omit()),
      end = max(dplyr::pull(db_data, date) %>% na.omit())
    )
  })

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
    filter_data = filter_data, data_exists = data_exists
  )

  # filter 1: by selected dates ----

  date_filter <- reactive({
    req(input$date_range) # ensure input$date_range is available before attempting to run this chunk


    start <- min(dplyr::pull(db_data, date) %>% na.omit())
    end <- max(dplyr::pull(db_data, date) %>% na.omit())

    # ensure start date is less than end date
    if (input$date_range[1] > input$date_range[2]) {
      showModal(modalDialog(
        title = strong("Error!"),
        HTML(paste(
          p("Start date can't be after end date"),
          p("data has default to ", strong(start), " -:- ", strong(end))
        ))
      ))

      db_data
    } else if (input$date_range[1] < start |
      input$date_range[2] > end) {
      showModal(modalDialog(
        title = strong("Error!"),
        HTML(paste(
          p("START date can't be before : ", start),
          p("and"),
          p("END date can't be after : ", end),
          p("DATE filter has default to ", strong(start), " -:- ", strong(end))
        ))
      ))

      db_data
    } else {
      db_data %>%
        dplyr::filter(
          date >= !!input$date_range[1],
          date <= !!input$date_range[2]
        )
    }
  })

  filter_data <- reactive({
    if (get_golem_config("trust_name") == "demo_trust") {
      return(list(
        "filter_data" = db_data %>%
          dplyr::collect(),
        "demography_number" = NULL
      ))
    }



    # filter 2: by selected Locations ----

    return_data <- get_location_data(
      date_filter = date_filter(),
      select_location_1 = input$select_location_1,
      select_location_2 = input$select_location_2,
      select_location_3 = input$select_location_3
    )

    # filter 2: by selected demographics ----

    demography_data <- get_demography_data(
      return_data = return_data,
      select_demography_1 = demographic_filters()$select_demography_1,
      select_demography_2 = demographic_filters()$select_demography_2,
      select_demography_3 = demographic_filters()$select_demography_3
    )

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
    tidy_filter_data <- get_tidy_filter_data(return_data, data_exists)

    return(list(
      "filter_data" = return_data,
      "single_labeled_filter_data" = tidy_filter_data,
      "unique_data" = unique_data,
      "demography_number" = no_responders
    ))
  })

  # modules----
  ## add information to dashboard header ----
  mod_header_message_server("messageMenu", db_data, data_exists)

  ## combine ALL sub-modules----
  mod_patient_experience_server("patient_experience_ui_1")

  ## sub-modules

  mod_documentation_page_server("documentation_page")

  mod_trend_server("trend_ui_1", filter_data, data_exists)

  mod_summary_server("summary_ui_1", data_exists)

  mod_summary_record_server("summary_record_1", db_data, filter_data)

  mod_data_management_server("data_management_1", db_conn = pool, filter_data, data_exists)

  filter_category <- mod_category_criticality_server("category_criticality_ui_1",
    filter_data = filter_data
  )

  mod_fft_server("fft_ui_1", filter_data = filter_data)

  mod_report_builder_server("report_builder_ui_1",
    filter_data = filter_data,
    all_inputs = all_inputs,
    data_exists = data_exists
  )

  mod_click_tables_server("click_tables_ui",
    filter_data = filter_data
  )

  mod_search_text_server("search_text_ui_1",
    filter_data = filter_data
  )

  mod_trend_overlap_server("trend_overlap_ui", filter_data, data_exists)

  mod_demographics_server(
    "demographics_ui_1",
    filter_data, data_exists
  )
}
