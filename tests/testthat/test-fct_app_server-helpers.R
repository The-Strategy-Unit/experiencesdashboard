test_that("db connect can be created", {
  skip_on_ci()
  expect_no_error(get_pool())
})

test_that("db data can be accessed", {
  skip_on_ci()
  expect_error(get_db_data(get_pool(), "random_config") %>%
    head(1) %>%
    collect())
  expect_no_error(get_db_data(get_pool(), "trust_NUH") %>%
    head(1) %>%
    collect())
})

test_that("get_tidy_filter_data works", {
  test1 <- get_tidy_filter_data(phase_2_db_data, TRUE)
  expect_false(identical(test1, phase_2_db_data))

  report <- data_validation_report()

  test1 %>%
    data.validator::validate(name = "Verifying get_tidy_filter_data output") %>%
    data.validator::validate_if(has_all_names(
      "date", "location_1",
      "comment_type", "comment_txt", "fft",
      "category", "super_category"
    ), description = "required columns are present") %>%
    validate_rows(col_concat, is_uniq, comment_id, category, description = "comment_id and category combination is unique") %>%
    validate_if(!is.na(comment_id) & comment_id != "", description = "comment_id column is not empty") %>%
    validate_if(!is.na(pt_id) & pt_id != "", description = "pat_id column is not empty") %>%
    validate_if(is.character(category), description = "category column is a list") %>%
    validate_if(inherits(date, "Date"), description = "date column is in date format") %>%
    validate_if(lubridate::year(min(date)) > 2015, description = "Start Date is after 2015") %>%
    add_results(report)

  # get_results(report) %>% View()

  is_validation_success <- all((get_results(report) %>% dplyr::pull(type)) == "success")
  expect_true(is_validation_success)

  test2 <- get_tidy_filter_data(phase_2_db_data, FALSE)
  expect_identical(test2, phase_2_db_data)
})

test_that("get_location_data works", {
  test1 <- get_location_data(
    date_filter = phase_2_db_data,
    select_location_1 = "Community Health Services",
    select_location_2 = "SNAPS",
    select_location_3 = c("Chameleon", "CHAMELEON WARD")
  )
  expect_equal(nrow(test1), 4)

  test2 <- get_location_data(
    date_filter = phase_2_db_data,
    select_location_1 = NULL,
    select_location_2 = NULL,
    select_location_3 = NULL
  )
  expect_identical(test2, phase_2_db_data)

  test3 <- get_location_data(
    date_filter = phase_2_db_data,
    select_location_1 = "NULL",
    select_location_2 = "NULL",
    select_location_3 = "NULL"
  )
  expect_false(identical(test3, phase_2_db_data))
  expect_equal(nrow(test3), 0)
})

test_that("get_demography_data works", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "trust_LPT")
  # > get_golem_config("demography_2")
  # [1] "age"
  # > get_golem_config("demography_1")
  # [1] "sex"
  # > get_golem_config("demography_3")
  # [1] "ethnicity"

  test1 <- get_demography_data(
    return_data = phase_2_db_data,
    select_demography_1 = "Male",
    select_demography_2 = "0 - 11",
    select_demography_3 = "Mixed"
  )
  expect_equal(nrow(test1), 2)

  test2 <- get_demography_data(
    return_data = phase_2_db_data,
    select_demography_1 = NULL,
    select_demography_2 = NULL,
    select_demography_3 = NULL
  )
  expect_identical(test2, phase_2_db_data)
})

test_that("set_trust_config works", {
  # no group
  expect_error(set_trust_config("group"), 'Not a member of any group')
  expect_error(set_trust_config("trust_NUH"), 'Not a member of any group')
  expect_error(set_trust_config("otherdashboard-developers"), "Not a member of any group")
  expect_error(set_trust_config("experiencedashboard-admins"), "Not a member of any group")
  
  # multiple groups
  expect_error(
    set_trust_config(c("experiencedashboard-admins", "experiencedashboard-developers", "experiencedashboard-developer-2")),
    "member of multiple groups"
  )
  
  # one group
  expect_equal(set_trust_config(c("experiencedashboard-admins", "experiencedashboard-developers")), "")
  expect_equal(set_trust_config("experiencedashboard-developers"), "")
})
