# 
# 
# test_that("it loads the reference data and set's the activity_type dropdown", {
#   
#   withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")
#   
#   data <- split_data_spc(unique_data, variable = "fft", chunks = "monthly")
#   
#   # m <- mock(data)
#   # stub(mod_fft_server, 'graph_data',  m)
#   
#   
#   stub(mod_fft_server, 'no_group',  100)
#   
#   testServer(mod_fft_server, args = list(phase_2_db_data), {
#     
#     print(no_group())
#     
#     # print(output$dynamic_fft)
#     # expect_called(m, 1)
#     # expect_args(m, 1, "messageMenu", phase_2_db_data)
#   })
#   
# })

# mod_fft_server ----
## test1 ----
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
    
    #  act    
    actual <- graph_data()
    
    # assert
    expect_equal(actual, "spc_data")
    
    expect_called(m, 1)
    expect_args(m, 1, "data", variable = "fft", chunks = "monthly")
  })
})


test_that("it checks the number of rows", {
  # arrange
  graph_data_expected <- tibble(
    date = c("2020-12-01", "2021-01-01", "2021-02-01",
             "2021-03-01", "2021-04-01", "2021-05-01"),
    n = c(1, 2, 2, 3, 3, 3)
  )
  
  stub(mod_fft_server, "split_data_spc", graph_data_expected)
  
  testServer(mod_fft_server, args = list(reactiveVal()), {
    filter_data(
      list(
        unique_data = "data"
      )
    )
    
    #  act    
    actual <- no_group()
    
    # assert
    expect_equal(actual, 6)
  })
})


test_that("it validates the plot data", {
  # arrange
  graph_data_expected <- tibble(
    date = c("2020-12-01", "2021-01-01", "2021-02-01",
             "2021-03-01", "2021-04-01", "2021-05-01"),
    fft = c(1, 2, 2, 3, 3, 3)
  )
  
  m <- mock()
  
  graph_data_expected <- graph_data_expected |> 
    mutate(date = as.Date(date))
  
  stub(mod_fft_server, "split_data_spc", graph_data_expected)
  stub(mod_fft_server, "plot_fft_spc", m)
  
  testServer(mod_fft_server, args = list(reactiveVal()), {
    
    print(no_group())
    expect_error(output$spc_plot)
    # expect_condition(output$spc_plot, 'There are not enough stable SPC points to plot. Please expand your selection')
    
  })
})


test_that("it validates the plot data", {
  # arrange
  graph_data_expected <- tibble(
    date = c("2020-04-01", "2020-05-01", "2020-08-01", 
             "2020-09-01", "2020-10-01", "2020-11-01",
             "2020-12-01", "2021-01-01", "2021-02-01",
             "2021-03-01", "2021-04-01", "2021-05-01"),
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
    
    print(no_group())
    print(graph_data())
    
    skip('Skip for now')
    output$spc_plot
    # act/assert
    # expect_snapshot(output$spc_plot)
    # expect_no_warning(output$spc_plot)
    # 
    # expect_called(m, 1)
    # expect_args(m, 1, graph_data_expected)
  })
})


# mod_summary_record_server ----
## test1 ----
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
    
    # act
    expect_snapshot(output$dynamic_summary_record)
    expect_snapshot(output$commentBox)
    expect_snapshot(output$individualBox)
    expect_snapshot(output$current_commentBox)
    expect_snapshot(output$current_individualBox)
    
    # assert all global variable are expected values after a call to output$dynamic_summary_record 
    expect_equal(global$n_responses, 1981)
    expect_equal(global$n_individuals, 1000)
    expect_equal(global$current_responses, 10)
    expect_equal(global$current_individuals, 5)
  })
})
