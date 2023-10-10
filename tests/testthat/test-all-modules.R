# mod_click_tables_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## test 1 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

test_that("mod_click_tables_server set up dynamic_click_tableUI correctly", {
  # no data in the database
  testServer(mod_click_tables_server, args = list(reactiveVal(), FALSE), {
    # act/assert
    expect_error(output$dynamic_click_tableUI, "Sub-Category Table will appear here")
  })

  # data exist in the database
  testServer(mod_click_tables_server, args = list(reactiveVal(), TRUE), {
    # act/assert
    expect_no_error(output$dynamic_click_tableUI)
  })
})

## test 2 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_click_tables_server set's up correctly", {
  # arrange
  m <- mock(data.frame())
  stub(mod_click_tables_server, "calculate_table", m)

  stub(mod_click_tables_server, "prep_data_for_comment_table", identity)

  testServer(mod_click_tables_server, args = list(reactiveVal(), TRUE), {
    filter_data(
      list(
        single_labeled_filter_data = single_labeled_filter_data
      )
    )

    # act/assert
    expect_equal(calculatedTable(), data.frame())
    expect_equal(return_data(), single_labeled_filter_data)
    expect_no_error(output$table)
    expect_no_error(output$comment_table)

    # expect calculate_table is called once and with the correct arguements
    expect_called(m, 1)
    expect_args(m, 1, single_labeled_filter_data, "category", comment_type)
  })
})

## test 3 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_click_tables_server works correctly - user input can be accessed", {
  # arrange
  stub(mod_click_tables_server, "prep_data_for_comment_table", identity)

  testServer(mod_click_tables_server, args = list(reactiveVal(), TRUE), {
    filter_data(
      list(
        single_labeled_filter_data = single_labeled_filter_data
      )
    )

    # act/assert
    session$setInputs(table_rows_selected = "Gratitude/ good experience")
    expect_equal(input$table_rows_selected, "Gratitude/ good experience") # user input can be accessed
    expect_no_error(return_data()) # return_data can be accessed
    expect_snapshot(output$comment_table) # comment table output is working correctly
  })
})

# mod_data_management_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_data_management_server work correctly", {
  # no data in the database
  testServer(mod_data_management_server, args = list("db_conn", reactiveVal(), FALSE), {
    # act/assert
    expect_error(output$data_management_UI)
  })

  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")
  testServer(mod_data_management_server, args = list("db_conn", reactiveVal(), TRUE), {
    filter_data(
      list(
        filter_data = phase_2_db_data |> head(100)
      )
    )

    # act/assert
    expect_no_error(output$data_management_UI)
    expect_equal(nrow(dt_out$data), 100)
    expect_equal(ncol(dt_out$data), 19)
    expect_equal(class(dt_out$data$category), "list")
    expect_no_error(output$pat_table)
    expect_equal(class(proxy), "dataTableProxy")
    expect_true(inherits(dt_out$complex_comments, "data.frame"))
  })
})

# mod_demographics_selection_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## test 1 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_demographics_selection_server work correctly: no data in database", {
  # arrange
  testServer(mod_demographics_selection_server, args = list(reactiveVal(), FALSE), {
    # act/assert
    expect_error(output$dynamic_demographics_selection)
    expect_false(data_exists)
  })
})

## test 2 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_demographics_selection_server work correctly: CONFIG with demographic feature", {
  # arrange
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")
  m <- mock("get_demographic_choice")
  stub(mod_demographics_selection_server, "get_demographic_choices", m)

  testServer(mod_demographics_selection_server, args = list(reactiveVal(), TRUE), {
    filter_data(
      list(
        unique_data = "data"
      )
    )

    # act
    session$setInputs()

    # assert
    expect_called(m, 3)
    expect_args(m, 1, "data", demographic_feature = "age")
    expect_args(m, 2, "data", demographic_feature = "sex")
    expect_args(m, 3, "data", demographic_feature = "ethnicity")

    expect_snapshot(output$dynamic_demographics_selection)
  })
})

## test 3 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_demographics_selection_server work correctly: CONFIG with no demographic feature", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "random_config")
  m <- mock("get_demographic_choice")
  stub(mod_demographics_selection_server, "get_demographic_choices", m)

  testServer(mod_demographics_selection_server, args = list(reactiveVal(), TRUE), {
    filter_data(
      list(
        unique_data = "data"
      )
    )

    # act
    session$setInputs()

    # assert
    expect_called(m, 3)
    expect_args(m, 1, "data", demographic_feature = NULL)
    expect_args(m, 2, "data", demographic_feature = NULL)
    expect_args(m, 3, "data", demographic_feature = NULL)

    expect_snapshot(output$dynamic_demographics_selection) # snapshot will have not html output
  })
})


# mod_demographics_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## test 1 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_demographics_server work correctly", {
  # no data in the database
  # arrange
  m <- mock("calculate_table_data")
  stub(mod_demographics_server, "calculate_table", m)
  testServer(mod_demographics_server, args = list(reactiveVal(), FALSE), {
    filter_data(
      list(
        filter_data = "data"
      )
    )
    # act/assert
    expect_error(output$dynamic_demo_UI, "Demography plots will appear here")
  })
})

## test 2 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_demographics_server work correctly", {
  # no data in the database
  # arrange
  m <- mock()
  stub(mod_demographics_server, "compare_demographics", m)

  # there is data in the database
  testServer(mod_demographics_server, args = list(reactiveVal(), TRUE), {
    filter_data(
      list(
        unique_data = "data"
      )
    )
    # act/assert
    expect_no_error(output$dynamic_demo_UI)

    # compare_demographics is called 3 times
    expect_called(m, 3)
  })
})

# mod_documentation_page_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_documentation_page_server work correctly", {
  # no data in the database

  testServer(mod_documentation_page_server, {
    # act/assert
    expect_no_error(output$framework_table)
    expect_no_error(framework)
  })
})

# mod_fft_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("it set's up graph data correctly", {
  # arrange
  m <- mock("spc_data")

  stub(mod_fft_server, "split_data_spc", m)

  testServer(mod_fft_server, args = list(reactiveVal()), {
    filter_data(
      list(
        unique_data = "data"
      )
    )

    #  act/assert
    expect_equal(graph_data(), "spc_data")

    expect_called(m, 1)
    expect_args(m, 1, "data", variable = "fft", chunks = "monthly")
  })
})

test_that("it checks the number of rows", {
  # arrange
  graph_data_expected <- tibble(
    date = c(
      "2020-12-01", "2021-01-01", "2021-02-01",
      "2021-03-01", "2021-04-01", "2021-05-01"
    ),
    n = c(1, 2, 2, 3, 3, 3)
  )

  stub(mod_fft_server, "split_data_spc", graph_data_expected)

  testServer(mod_fft_server, args = list(reactiveVal()), {
    filter_data(
      list(
        unique_data = "data"
      )
    )

    #  act/assert
    expect_equal(no_group(), 6)
  })
})


test_that("it doesn't validates the plot data when group is less than 10", {
  # arrange
  graph_data_expected <- tibble(
    date = c(
      "2020-12-01", "2021-01-01", "2021-02-01",
      "2021-03-01", "2021-04-01", "2021-05-01"
    ),
    fft = c(1, 2, 2, 3, 3, 3)
  )

  m <- mock()

  graph_data_expected <- graph_data_expected |>
    mutate(date = as.Date(date))

  stub(mod_fft_server, "split_data_spc", graph_data_expected)
  stub(mod_fft_server, "plot_fft_spc", m)

  testServer(mod_fft_server, args = list(reactiveVal()), {
    expect_error(output$spc_plot)
    expect_called(m, 0)
  })
})

test_that("it validates the plot data when group is at least 10", {
  # arrange
  graph_data_expected <- tibble(
    date = c(
      "2020-04-01", "2020-05-01", "2020-08-01",
      "2020-09-01", "2020-10-01", "2020-11-01",
      "2020-12-01", "2021-01-01", "2021-02-01",
      "2021-03-01", "2021-04-01", "2021-05-01"
    ),
    fft = c(1, 2, 2, 3, 3, 3, 4, 5, 3, 5, 3, 4)
  )

  m <- mock()

  stub(mod_fft_server, "split_data_spc", graph_data_expected)
  stub(mod_fft_server, "plot_fft_spc", m)

  testServer(mod_fft_server, args = list(reactiveVal()), {
    filter_data(
      list(
        unique_data = "data"
      )
    )

    # act/assert
    expect_no_error(output$spc_plot)
    expect_no_warning(output$spc_plot)

    expect_called(m, 1)
    expect_args(m, 1, graph_data_expected)
  })
})

# mod_header_message_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_header_message_server work correctly", {
  # arrange

  stub(
    mod_header_message_server, "DBI::dbGetQuery",
    tibble::tibble(
      "MAX(last_upload_date)" = Sys.Date(),
      "MAX(last_edit_date)" = Sys.Date()
    )
  )
  data <- phase_2_db_data %>%
    mutate(last_edit_date = NA)

  # no data in the database
  testServer(mod_header_message_server, args = list("pool", data, FALSE), {
    # act/assert
    expect_error(output$dynamic_messageMenu)
  })

  # there is data in the database
  testServer(mod_header_message_server, args = list("pool", data, TRUE), {
    # act/assert
    expect_no_error(output$dynamic_messageMenu)
    expect_identical(db_data, data)
  })
})

# mod_patient_experience_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_patient_experience_server work correctly", {
  # arrange
  m <- mock()
  stub(mod_patient_experience_server, "mod_documentation_page_ui", m)

  m2 <- mock()
  stub(mod_patient_experience_server, "mod_data_management_ui", m2)

  m3 <- mock()
  stub(mod_patient_experience_server, "mod_trend_ui", m3)

  m4 <- mock()
  stub(mod_patient_experience_server, "mod_trend_overlap_ui", m4)

  m5 <- mock()
  stub(mod_patient_experience_server, "mod_click_tables_ui", m5)

  m6 <- mock()
  stub(mod_patient_experience_server, "mod_search_text_ui", m6)

  m7 <- mock()
  stub(mod_patient_experience_server, "mod_summary_ui", m7)

  withr::local_envvar("R_CONFIG_ACTIVE" = "random_config")
  m8 <- mock()
  stub(mod_patient_experience_server, "mod_demographics_ui", m8)

  withr::local_envvar("R_CONFIG_ACTIVE" = "trust_NUH")
  m9 <- mock()
  stub(mod_patient_experience_server, "mod_demographics_ui", m9)

  # there is data in the database
  testServer(mod_patient_experience_server, {
    # act
    session$setInputs()

    # assert
    # modules are called once
    expect_called(m, 1)
    expect_called(m2, 1)
    expect_called(m3, 1)
    expect_called(m4, 1)
    expect_called(m5, 1)
    expect_called(m6, 1)
    expect_called(m7, 1)
    expect_called(m9, 1)

    # modules not called when no demographic feature in config
    expect_called(m8, 0)
  })
})

# mod_summary_record_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
## test1 ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_summary_record_server works correctly", {
  testServer(mod_summary_record_server, args = list(phase_2_db_data, reactiveVal()), {
    filter_data(
      list(
        filter_data = phase_2_db_data |> head(10)
      )
    )

    # assert all global variable are null before a call to output$dynamic_summary_record
    expect_equal(global$n_responses, NULL)
    expect_equal(global$n_individuals, NULL)
    expect_equal(global$current_responses, NULL)
    expect_equal(global$current_individuals, NULL)

    # assert output are accessible
    expect_no_error(output$dynamic_summary_record)
    expect_no_error(output$commentBox)
    expect_no_error(output$individualBox)
    expect_no_error(output$current_commentBox)
    expect_no_error(output$current_individualBox)

    # assert all global variable are expected values after a call to output$dynamic_summary_record
    expect_equal(global$n_responses, 1900)a
    expect_equal(global$n_individuals, 959)
    expect_equal(global$current_responses, 10)
    expect_equal(global$current_individuals, 5)
  })
})

# mod_report_builder_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_report_builder_server work correctly: nothing is selected", {
  # no data in the database
  testServer(mod_report_builder_server, args = list(reactiveVal(), FALSE), {
    # act/assert
    expect_error(output$dynamic_report_UI)
  })

  testServer(mod_report_builder_server, args = list(filter_data = reactiveVal(), all_inputs = reactiveVal(), data_exists = TRUE), {
    all_inputs <- reactive({
      list(
        "date_from" = as.Date("2020-05-26"),
        "date_to" = as.Date("2021-10-18"),
        "location_1" = NULL,
        "location_2" = NULL,
        "location_3" = NULL
      )
    })
    # act/assert
    expect_no_error(output$dynamic_report_UI)
    expect_no_error(output$report_componentsUI)
    expect_null(input$report_components)
    expect_error(output$download_report)
  })
})

test_that("mod_report_builder_server work correctly: filter data is less than 10 rows", {
  # no data in the database
  testServer(mod_report_builder_server, args = list(reactiveVal(), FALSE), {
    # act/assert
    expect_error(output$dynamic_report_UI)
  })

  # there is data in the database
  testServer(mod_report_builder_server, args = list(filter_data = reactiveVal(), all_inputs = reactiveVal(), data_exists = TRUE), {
    filter_data(
      list(
        filter_data = phase_2_db_data |> head(9),
        single_labeled_filter_data = "data"
      )
    )
    all_inputs <- reactive({
      list(
        "date_from" = as.Date("2020-05-26"),
        "date_to" = as.Date("2021-10-18"),
        "location_1" = NULL,
        "location_2" = NULL,
        "location_3" = NULL
      )
    })

    # act
    session$setInputs(report_components = c(
      "% categories table" = "category_table",
      "Verbatim comments" = "verbatim_comments"
    ))


    # assert
    expect_no_error(output$dynamic_report_UI)
    expect_no_error(output$report_componentsUI)

    expect_true(nrow(filter_data()$filter_data) < 10)
    expect_error(output$download_report)
  })
})

test_that("mod_report_builder_server work correctly", {
  # no data in the database
  testServer(mod_report_builder_server, args = list(reactiveVal(), FALSE), {
    # act/assert
    expect_error(output$dynamic_report_UI)
  })

  # There is data in the database
  testServer(mod_report_builder_server, args = list(filter_data = reactiveVal(), all_inputs = reactiveVal(), data_exists = TRUE), {
    filter_data(
      list(
        filter_data = phase_2_db_data |> head(10),
        single_labeled_filter_data = phase_2_db_data |>
          head(10) |>
          get_tidy_filter_data(TRUE)
      )
    )
    all_inputs <- reactive({
      list(
        "date_from" = as.Date("2020-05-26"),
        "date_to" = as.Date("2021-10-18"),
        "location_1" = NULL,
        "location_2" = NULL,
        "location_3" = NULL
      )
    })

    # act
    session$setInputs(report_components = c(
      "% categories table" = "category_table",
      "Verbatim comments" = "verbatim_comments"
    ))
    session$setInputs(time_period = "Previous quarter")


    # assert
    expect_no_error(output$dynamic_report_UI)
    expect_no_error(output$report_componentsUI)
    expect_no_error(output$download_report)
  })
})

# mod_search_text_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_search_text_server work correctly", {
  # no data in the database
  testServer(mod_search_text_server, args = list(reactiveVal()), {
    # act/assert
    expect_error(return_data())
    expect_error(output$comment_output)
    # expect_no_error(output$search_download_data)
  })

  # arrange
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")
  testServer(mod_search_text_server, args = list(reactiveVal()), {
    filter_data(
      list(
        filter_data = phase_2_db_data |> head(10)
      )
    )
    session$setInputs(text_search = "good")

    # act/assert
    expect_no_error(return_data())
    expect_no_error(output$comment_output)
  })
})

# mod_summary_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_summary_server work correctly", {
  # no data in the database
  testServer(mod_summary_server, args = list(FALSE), {
    # act/assert
    expect_error(output$dynamic_summary)
  })

  testServer(mod_summary_server, args = list(TRUE), {
    # act/assert
    expect_no_error(output$dynamic_summary)
  })
})

# mod_trend_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_trend_server work correctly", {
  # no data in the database
  testServer(mod_trend_server, args = list(reactiveVal(), FALSE), {
    # act/assert
    expect_error(output$dynamic_trendUI)
  })

  testServer(mod_trend_server, args = list(reactiveVal(), TRUE), {
    filter_data(
      list(
        single_labeled_filter_data = single_labeled_filter_data
      )
    )

    # act/assert
    expect_no_error(output$dynamic_trendUI)
  })
})

# mod_trend_overlap_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_trend_overlap_server work correctly", {
  # no data in the database
  testServer(mod_trend_overlap_server, args = list(reactiveVal(), FALSE), {
    # act/assert
    expect_error(output$dynamic_trend_overlap)
  })

  # data exist in the database
  testServer(mod_trend_overlap_server, args = list(reactiveVal(), TRUE), {
    # act/assert
    expect_no_error(output$dynamic_trend_overlap)
  })
})

test_that("mod_trend_overlap_server initialise top sub category selector correctly", {
  # arrange
  m <- mock(c("a", "b", "c"))
  stub(mod_trend_overlap_server, "get_unique_value", m)

  testServer(mod_trend_overlap_server, args = list(reactiveVal(), TRUE), {
    # doesn't show in tab_across_categories tab
    session$setInputs(tabset_overlap = "tab_across_categories")
    expect_error(output$trendUI)

    # show in other tab
    session$setInputs(tabset_overlap = "other tab")
    expect_no_error(output$trendUI)

    # other outputs shows as expected
    expect_no_error(output$within_categories_ui)
    expect_no_error(output$across_categories_ui)
  })
})

# mod_overlap_1_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("mod_overlap_1_server work correctly: get_unique_value()", {
  # arrange
  data <- phase_2_db_data |>
    head(10) |>
    get_tidy_filter_data(TRUE)

  m <- mock(c("a", "b", "c"))

  stub(mod_overlap_1_server, "get_unique_value", m)
  testServer(mod_overlap_1_server, args = list(reactiveVal(), "string1", 2), {
    filter_data(
      list(
        single_labeled_filter_data = data
      )
    )

    # act
    session$setInputs()

    # assert
    expect_called(m, 2)
    expect_args(m, 1, data, "category")
    expect_args(m, 2, data[0, ], "category")
  })
})

test_that("mod_overlap_1_server: dynamic_select_category_ui work correctly", {
  # arrange
  data <- phase_2_db_data |>
    head(10) |>
    get_tidy_filter_data(TRUE)
  m <- mock(c("a", "b", "c"))
  # m = mock()
  stub(mod_overlap_1_server, "get_unique_value", m)

  testServer(mod_overlap_1_server, args = list(reactiveVal(), "string1", 2), {
    filter_data(
      list(
        single_labeled_filter_data = data
      )
    )

    session$setInputs(
      select_category1 = NULL,
      select_category2 = "Admission",
      select_category3 = "Admission",
      # min_size = 1
    )
    expect_error(output$dynamic_select_category_ui)
  })
})

test_that("mod_overlap_1_server: dynamic_select_category_ui work correctly", {
  # arrange
  data <- phase_2_db_data |>
    head(10) |>
    get_tidy_filter_data(TRUE)
  m <- mock(c("a", "b", "c"))
  # m = mock()
  stub(mod_overlap_1_server, "get_unique_value", m)

  testServer(mod_overlap_1_server, args = list(reactiveVal(), "string1", 2), {
    filter_data(
      list(
        single_labeled_filter_data = data
      )
    )

    expect_no_error(output$trendUI_2)

    session$setInputs(
      select_category1 = NULL,
      select_category2 = "pack",
      select_category3 = "Admission",
      # min_size = 1
    )
    expect_no_error(output$dynamic_select_category_ui)
  })
})

test_that("mod_overlap_1_server initialise top sub category selectors correctly", {
  m <- mock(c("a", "b", "c"))
  stub(mod_overlap_1_server, "get_unique_value", m)

  testServer(mod_overlap_1_server, args = list(reactiveVal(), "General", 1), {
    expect_no_error(output$trendUI_2)
  })
})

test_that("mod_overlap_1_server works correctly when given some inputs", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")
  testServer(mod_overlap_1_server, args = list(reactiveVal(), "General", 1), {
    # Arrange
    filter_data(
      list(
        single_labeled_filter_data = single_labeled_filter_data
      )
    )

    session$setInputs(
      select_category1 = NULL,
      select_category2 = "Parking",
      select_category3 = "Admission"
    )

    expect_no_error(output$dynamic_select_category_ui)

    # the upset plot contents are working
    expect_equal(nrow(upset_data()), 110)
    expect_equal(length(all_categories()), 15)
    expect_equal(length(filtered_categories()), 3)
    expect_no_error(output$category_upset)

    # the comment table content works well
    expect_no_error(output$category_upset)
    expect_equal(nrow(return_data()), 0)
    expect_error(output$download_data_ui)
    expect_no_error(output$overlap_table)
  })
})

# mod_comment_download_server ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
test_that("module server works well if given corrent arguements", {
  testServer(mod_comment_download_server,
             args = list(head(phase_2_db_data, 100), "test-data-"), {
               ns <- session$ns
               expect_true(
                 inherits(ns, "function")
               )
               expect_true(
                 grepl(id, ns(""))
               )
               expect_true(
                 grepl("test", ns("test"))
               )
               
               # the return data is accessible
               expect_identical(return_data, head(phase_2_db_data, 100))
               
               # shows the comment table
               expect_true(
                 inherits(output$dynamic_comment_table, "json")
               )
               
               # download file is named correctly
               expect_true(grepl("(test-data-.).+(.xlsx)$", output$download_comments))
               
               # returned value is class shiny.tag.list
               golem::expect_shinytaglist(session$returned)
               expect_snapshot(session$returned)
             }
  )
})

test_that("module server works well if passed data is empty", {
  # throw error when there is no data in the database
  testServer(mod_comment_download_server, 
             args = list(data.frame(), "test-data-"), {
               # show expected result
               expect_true(grepl("No data to show", session$returned))
             })
})