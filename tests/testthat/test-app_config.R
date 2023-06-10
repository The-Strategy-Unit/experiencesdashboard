test_that("get_golem_config works", {
  Sys.setenv("R_CONFIG_ACTIVE" = "phase_2_demo")

  expect_equal(get_golem_config("trust_name"), "phase_2_demo")
})
