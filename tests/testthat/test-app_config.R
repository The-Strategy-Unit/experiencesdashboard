test_that("get_golem_config works", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")
  expect_equal(get_golem_config("trust_name"), "phase_2_demo")

  withr::local_envvar("R_CONFIG_ACTIVE" = "random_configuration")
  expect_null(get_golem_config("trust_name"))
})

test_that("trust configuration is still the same", {
  # trust_LPT ----
  withr::local_envvar("R_CONFIG_ACTIVE" = "trust_LPT")
  expect_true(isTruthy(get_golem_config("location_1")))
  expect_true(isTruthy(get_golem_config("location_2")))
  expect_false(isTruthy(get_golem_config("location_3")))
  expect_true(isTruthy(get_golem_config("comment_1")))
  expect_false(isTruthy(get_golem_config("comment_2")))
  expect_null(get_golem_config("demography_1"))
  expect_null(get_golem_config("demography_2"))
  expect_null(get_golem_config("demography_3"))
  
  # trust_NUH ----
  withr::local_envvar("R_CONFIG_ACTIVE" = "trust_NUH")
  expect_true(isTruthy(get_golem_config("location_1")))
  expect_true(isTruthy(get_golem_config("location_2")))
  expect_true(isTruthy(get_golem_config("location_3")))
  expect_true(isTruthy(get_golem_config("comment_1")))
  expect_false(isTruthy(get_golem_config("comment_2")))
  expect_equal(get_golem_config("demography_1"), "gender")
  expect_equal(get_golem_config("demography_2"), "age")
  expect_equal(get_golem_config("demography_3"), "ethnicity")

  # trust_GOSH ----
  withr::local_envvar("R_CONFIG_ACTIVE" = "trust_GOSH")
  expect_true(isTruthy(get_golem_config("location_1")))
  expect_true(isTruthy(get_golem_config("location_2")))
  expect_true(isTruthy(get_golem_config("location_3")))
  expect_true(isTruthy(get_golem_config("comment_1")))
  expect_true(isTruthy(get_golem_config("comment_2")))
  expect_equal(get_golem_config("demography_1"), "disability")
  expect_equal(get_golem_config("demography_2"), "religion")
  expect_equal(get_golem_config("demography_3"), "ethnicity")
})
