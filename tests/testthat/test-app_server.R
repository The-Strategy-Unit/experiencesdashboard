test_that("App server create the database data", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  # mock pool::poolClose to always return the arguement its given unmodified
  stub(app_server, "pool::poolClose", identity)

  stub(app_server, "check_api_job", list(latest_time = NULL, estimated_wait_time = NULL))

  stub(app_server, "get_pool", "get_pool")

  m <- mock(phase_2_db_data)

  stub(app_server, "get_db_data", m)

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "get_pool", "phase_2_demo")
    expect_equal(db_data, phase_2_db_data)
  })
})

test_that("all_inputs returns correct value", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  # mock pool::poolClose to always return the arguement its given unmodified
  stub(app_server, "pool::poolClose", identity)

  stub(app_server, "check_api_job", list(latest_time = NULL, estimated_wait_time = NULL))


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



  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_complex_comments_server", "mod_complex_comments_server")



  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "get_pool", "phase_2_demo")
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

  # mock pool::poolClose to always return the arguement its given unmodified
  stub(app_server, "pool::poolClose", identity)

  stub(app_server, "check_api_job", list(latest_time = NULL, estimated_wait_time = NULL))

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



  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_complex_comments_server", "mod_complex_comments_server")



  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "messageMenu", "get_pool", phase_2_db_data, data_exists)
  })
})

test_that("loads mod_documentation_page_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  # mock pool::poolClose to always return the arguement its given unmodified
  stub(app_server, "pool::poolClose", identity)

  stub(app_server, "check_api_job", list(latest_time = NULL, estimated_wait_time = NULL))

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



  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_complex_comments_server", "mod_complex_comments_server")



  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "patient_experience_ui_1", admin_user)
  })
})

test_that("loads mod_patient_experience_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  stub(app_server, "check_api_job", list(latest_time = NULL, estimated_wait_time = NULL))

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  # mock pool::poolClose to always return the arguement its given unmodified
  stub(app_server, "pool::poolClose", identity)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", m)

  stub(app_server, "mod_trend_server", "mod_trend_server")



  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_complex_comments_server", "mod_complex_comments_server")



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

  # mock pool::poolClose to always return the arguement its given unmodified
  stub(app_server, "pool::poolClose", identity)

  stub(app_server, "check_api_job", list(latest_time = NULL, estimated_wait_time = NULL))

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", m)



  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_complex_comments_server", "mod_complex_comments_server")



  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "trend_ui_1", filter_data, data_exists)
  })
})

test_that("loads mod_summary_record_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  # mock pool::poolClose to always return the arguement its given unmodified
  stub(app_server, "pool::poolClose", identity)

  stub(app_server, "check_api_job", list(latest_time = NULL, estimated_wait_time = NULL))

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")



  stub(app_server, "mod_summary_record_server", m)

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_complex_comments_server", "mod_complex_comments_server")



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

  # mock pool::poolClose to always return the arguement its given unmodified
  stub(app_server, "pool::poolClose", identity)

  stub(app_server, "check_api_job", list(latest_time = NULL, estimated_wait_time = NULL))

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")



  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", m)

  stub(app_server, "mod_complex_comments_server", "mod_complex_comments_server")



  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "data_management_1", pool, filter_data, data_exists, user)
  })
})

test_that("loads mod_click_tables_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  # mock pool::poolClose to always return the arguement its given unmodified
  stub(app_server, "pool::poolClose", identity)

  stub(app_server, "check_api_job", list(latest_time = NULL, estimated_wait_time = NULL))

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")



  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_complex_comments_server", "mod_complex_comments_server")



  stub(app_server, "mod_click_tables_server", m)

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "click_tables_ui", filter_data, TRUE)
  })
})

test_that("loads mod_search_text_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  stub(app_server, "check_api_job", list(latest_time = NULL, estimated_wait_time = NULL))

  # mock pool::poolClose to always return the arguement its given unmodified
  stub(app_server, "pool::poolClose", identity)

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_complex_comments_server", "mod_complex_comments_server")

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

  # mock pool::poolClose to always return the arguement its given unmodified
  stub(app_server, "pool::poolClose", identity)

  stub(app_server, "check_api_job", list(latest_time = NULL, estimated_wait_time = NULL))

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_complex_comments_server", "mod_complex_comments_server")

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

  # mock pool::poolClose to always return the arguement its given unmodified
  stub(app_server, "pool::poolClose", identity)

  stub(app_server, "check_api_job", list(latest_time = NULL, estimated_wait_time = NULL))

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_complex_comments_server", "mod_complex_comments_server")

  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", m)

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "demographics_ui_1", filter_data, data_exists)
  })
})


test_that("loads mod_fft_server correctly", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  # mock pool::poolClose to always return the arguement its given unmodified
  stub(app_server, "pool::poolClose", identity)

  stub(app_server, "check_api_job", list(latest_time = NULL, estimated_wait_time = NULL))

  m <- mock()

  stub(app_server, "get_pool", "get_pool")

  stub(app_server, "dplyr::collect", identity)

  stub(app_server, "get_db_data", phase_2_db_data)

  ## add information to dashboard header ----
  stub(app_server, "mod_header_message_server", "mod_header_message_server")

  stub(app_server, "mod_patient_experience_server", "mod_patient_experience_server")

  stub(app_server, "mod_documentation_page_server", "mod_documentation_page_server")

  stub(app_server, "mod_trend_server", "mod_trend_server")

  stub(app_server, "mod_summary_record_server", "mod_summary_record_server")

  stub(app_server, "mod_data_management_server", "mod_data_management_server")

  stub(app_server, "mod_complex_comments_server", m)

  stub(app_server, "mod_click_tables_server", "mod_click_tables_server")

  stub(app_server, "mod_search_text_server", "mod_search_text_server")

  stub(app_server, "mod_trend_overlap_server", "mod_trend_overlap_server")

  stub(app_server, "mod_demographics_server", "mod_demographics_server")

  testServer(app_server, {
    expect_called(m, 1)
    expect_args(m, 1, "complex_comments_1", filter_data, data_exists)
  })
})

test_that("database data is returned as a lazy tibble", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")

  stub(app_server, "check_api_job", list(latest_time = NULL, estimated_wait_time = NULL))

  skip_on_ci()

  testServer(app_server, {
    # test that the database data is returned as a lazy tibble
    inherits(db_data, "tbl_sql") |>
      expect_true()

    expect_true(data_exists)
  })
})

test_that("App server does't work with random_configuration", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "random_configuration")

  skip_on_ci()

  expect_error(
    testServer(app_server, {})
  )
})
