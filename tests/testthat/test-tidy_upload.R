test_that("Cleaning of uploaded data works", {
  required_cols <- c(
    "date", "pt_id", "location_1", "location_2", "location_3",
    "comment_type", "comment_text", "fft_score", "sex",
    "gender", "age", "ethnicity", "sexuality", "disability", "religion",
    "extra_variable_1", "extra_variable_2", "extra_variable_3"
  )

  template <- tibble(
    date = structure(c(1625097600, 1625097600, 1625097600),
      class = c("POSIXct", "POSIXt"), tzone = "UTC"
    ),
    location_1 = c("XXX", "YYY", "ZZZ"),
    location_2 = c("xxxxxx", "yyyyyy", "zzzzzz"),
    location_3 = c(
      "abcdef",
      "ghijk",
      "lmonp"
    ),
    fft_score = c(5, 2, 1),
    question_1 = c(
      "Service too slow",
      NA,
      "????"
    ),
    question_2 = c(NA, "NULL", NA),
    gender = c(NA, NA, NA),
    age = c(NA, NA, NA),
    ethnicity = c(NA, NA, NA),
    extra_variable_1 = c("parent", "child", "NA")
  )

  test_template <- template %>%
    dplyr::mutate(pt_id = seq.int(1, nrow(.))) %>%
    tidyr::pivot_longer(
      cols = dplyr::starts_with("question"),
      names_to = "comment_type",
      values_to = "comment_text"
    ) %>%
    dplyr::select(dplyr::any_of(required_cols)) %>%
    dplyr::mutate(comment_id = 1:nrow(.)) %>%
    clean_dataframe("comment_text")

  expect_equal(test_template$comment_text, "Service too slow")
  expect_equal(nrow(test_template), 1)
})

test_that("clean_dataframe works", {
  df <- data.frame(
    comment = c(
      "staff are really friendly and kind, they are happy to help us when we need it.� they take real care with my baby.� i feel so comfortable letting them look after my child. you feel really welcome and like you are living in a second home.� truly wonderful staff everywhere",
      "Staff are very friendly�& informative."
    )
  )

  result <- df |>
    clean_dataframe("comment")

  expect_equal(result$comment[2], "Staff are very friendly & informative.")
})


test_that("tidy_trust_gosh works", {
  data <- data.frame(
    comment_id = 1:4,
    age = c("31", "55", "77", NA)
  )

  result <- tidy_trust_gosh(data)

  expect_true(inherits(result, "data.frame"))
  expect_true(inherits(result$age, "character"))
  expect_equal(result$age, c("26 - 39", "40 - 64", "65 - 79", NA))
})

test_that("tidy_trust_neas works", {
  data <- data.frame(
    comment_id = 1:4,
    fft_score = c("Very good", "Poor", "Don’t know", NA)
  )

  result <- tidy_trust_neas(data)

  expect_true(inherits(result, "data.frame"))
  expect_true(inherits(result$fft_score, "numeric"))
  expect_equal(result$fft_score, c(1, 4, 6, NA))
})

test_that("tidy_trust_nth works", {
  data <- data.frame(
    comment_id = 1:4,
    age = c("31", "55", "77", NA),
    sex = c("2", "1", "3", NA),
    gender = c("2", "1", "3", NA)
  )

  result <- tidy_trust_nth(data)

  expect_true(inherits(result, "data.frame"))
  expect_true(inherits(result$sex, "character"))
  expect_equal(result$gender, c("Female", "Male", "Prefer not to say", "Prefer to self-describe"))
})

test_that("uploaded data works", {
  # test 1- throw error for mismatched trust
  withr::local_envvar("R_CONFIG_ACTIVE" = "trust_LPT")
  expect_error(upload_data("data", "conn", "trust_NUH", "simon", TRUE))

  # test 2 -
  # Create a mock for the database connection (DBI::dbGetQuery and DBI::dbWriteTable)
  stub(upload_data, "DBI::dbGetQuery", list(`MAX(job_id)` = 0, `MAX(comment_id)` = 0, `MAX(pt_id)` = 0)) # return 0
  stub(upload_data, "DBI::dbWriteTable", TRUE) # mock successful database write

  test_pred <- data.frame(comment_id = 1:5, prediction = 1:5)

  # Create a mock for the API functions (`get_pred_from_url` and `batch_predict`)
  m <- mock()
  stub(upload_data, "get_api_pred_url", m) # return a test prediction dataframe
  stub(
    upload_data,
    "batch_predict",
    data.frame(
      comment_id = 1:5,
      comment_text = c("comment1", "comment2", "comment3", "comment4", "comment5"),
      labels = c("l1", "l2", "l3", "l4", NA)
    )
  ) # return a test prediction dataframe


  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")
  test_upload <- upload_data(
    data = head(phase_2_upload_data, 10),
    conn = NULL,
    trust_id = get_golem_config("trust_name"),
    user = "test user",
    write_db = F
  )

  expect_called(m, 1)
  expect_true(inherits(test_upload, "data.frame"))

  expect_no_error(
    upload_data(
      data = head(phase_2_upload_data, 10),
      conn = NULL,
      trust_id = get_golem_config("trust_name"),
      user = "test user",
      write_db = T
    )
  )
})
