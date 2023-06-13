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
