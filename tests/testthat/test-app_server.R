test_that("App server create the database data", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_pool", "get_pool")

  m <- mock(phase_2_db_data)

  stub(app_server, "get_db_data", m)

  # add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  ## combine ALL sub-modules----
  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  ## sub-modules

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "get_pool", "phase_2_demo")
    expect_equal(db_data, phase_2_db_data)
  })
})

test_that("all_inputs returns correct value", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "get_db_data", phase_2_db_data)

  # add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  ## combine ALL sub-modules----
  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  ## sub-modules

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")
  #
  testServer(app_server, {
    # expect_called(m, 1)
    # expect_args(m, 1, 'get_pool', 'phase_2_demo')
    expect_equal(db_data, phase_2_db_data)

    min_date <- min(phase_2_db_data$date)
    max_date <- max(phase_2_db_data$date)

    session$setInputs(
      date_range = c(min_date, max_date),
      select_location_1 = "Ambulance Services",
      select_location_2 = NULL,
      select_location_3 = NULL
    )

    expect_equal(all_inputs()$location_1, "Ambulance Services")
    expect_null(all_inputs()$location_2)
    expect_null(all_inputs()$location_2, 2)
    expect_equal(all_inputs()$date_from, min_date)
    expect_equal(all_inputs()$date_to, max_date)
  })
})

test_that("loads mod_header_message_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", m)

  ## combine ALL sub-modules----
  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  ## sub-modules

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "messageMenu", phase_2_db_data, data_exists)
  })
})


test_that("loads mod_documentation_page_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  ## combine ALL sub-modules----
  stub(app_server, "mod_patient_experience_server", m)

  ## sub-modules

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "patient_experience_ui_1")
  })
})

test_that("loads mod_patient_experience_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", m)

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "documentation_page")
  })
})

test_that("loads mod_trend_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", m)

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "trend_ui_1", filter_data, data_exists)
  })
})

test_that("loads mod_summary_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", m)

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "summary_ui_1", data_exists)
  })
})

test_that("loads mod_summary_record_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", m)

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "summary_record_1", db_data, filter_data)
  })
})

test_that("loads mod_data_management_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", m)

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "data_management_1", pool, filter_data, data_exists)
  })
})

test_that("loads mod_category_criticality_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", m)

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "category_criticality_ui_1", filter_data)
  })
})

test_that("loads mod_fft_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", m)

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "fft_ui_1", filter_data)
  })
})

test_that("loads mod_report_builder_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", m)


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "report_builder_ui_1", filter_data, all_inputs, data_exists)
  })
})

test_that("loads mod_click_tables_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", m)

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "click_tables_ui", filter_data)
  })
})

test_that("loads mod_search_text_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", m)

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "search_text_ui_1", filter_data)
  })
})

test_that("loads mod_trend_overlap_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", m)

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "trend_overlap_ui", filter_data, data_exists)
  })
})

test_that("loads mod_demographics_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", m)

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "demographics_ui_1", filter_data, data_exists)
  })
})

test_that("database data is returned as a lazy tibble", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_server", "mod_summary_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_category_criticality_server", "mod_category_criticality_server")

  stub(app_server, "mod_fft_server", "mod_fft_server")

  stub(app_server, "mod_report_builder_server", "mod_report_builder_server")


  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  skip_on_ci()
  testServer(app_server, {
    # test that the database data is returned as a tibble
    inherits(db_data, "tbl_sql") |> expect_true()

    # test that when trust name is "trust_LPT" then db_data is empty
    expect_true(data_exists)
  })
})

test_that("App server does't work with random_configuration", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "random_configuration")
  expect_error(
    testServer(app_server, {})
  )
})
