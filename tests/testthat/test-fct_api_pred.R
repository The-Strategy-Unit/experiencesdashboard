test_that("api_pred and batch_predict is working...", {
  text_data <- data.frame(
    comment_id = c(1, 2, 3),
    comment_text = c(
      "Food was cold", "Those satffs are just too amazing",
      "I hate the meal"
    ),
    question_type = c("what_good", "could_improve", "nonspecific")
  )

  expect_error(api_pred(text_data |>
    select(-question_type)))

  expect_no_error(api_pred(text_data))

  preds <- text_data |>
    batch_predict()

  expect_equal(nrow(preds), 3)
  expect_true(all(c("comment_id", "comment_text", "labels") %in% names(preds)))
  expect_equal(sum(is.na(preds$labels)), 0)
})

# test_that("assign_highlevel_categories function is working and API vs Framework are in sync", {
#
#   withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")
#
#   text_data <- phase_2_db_data %>%
#     head(100) %>%
#     dplyr::mutate(comment_text = comment_txt,
#            question_type = comment_type) %>%
#     dplyr::select(comment_id, comment_text, question_type) %>%
#     dplyr::mutate(
#       question_type = stringr::str_replace_all(
#         question_type, "comment_1",
#         api_question_code(get_golem_config("comment_1"))
#       )
#     ) |>
#     dplyr::mutate(
#       question_type = stringr::str_replace_all(
#         question_type, "comment_2",
#         api_question_code(get_golem_config("comment_2"))
#       )
#     )
#
#   preds <- text_data |>
#     batch_predict()
#
#   # assign the super category
#   preds <- preds %>%
#     rename(category = labels) %>%
#     tidyr::unnest(category) %>%
#     dplyr::mutate(super_category = assign_highlevel_categories(category))
#
#   not_in_framewk <- preds %>%
#     filter(super_category == "Unknown Category") %>%
#     pull(category) %>%
#     unique()
#
#   # all assigned labels must be in framework
#   expect_true(all(preds$category %in% framework$`Sub-category`))
#   expect_true(length(not_in_framewk) == 0)
# })

test_that("api_question_code works", {
  expect_equal(api_question_code("What did we do well"), "what_good")
  expect_equal(api_question_code("What could be improved"), "could_improve")
  expect_equal(api_question_code("why that answer"), "nonspecific")
})

test_that("get_api_pred_url works and return expected result", {
  comment_id <- c("1", "2", "3")
  comment_text <- c(
    "Nurse was great but very difficult to find parking",
    "The ward was freezing.",
    ""
  )
  question_type <- c("what_good", "could_improve", "nonspecific")
  df <- data.frame(comment_id, comment_text, question_type)

  # emit url link
  stub(get_api_pred_url, "httr::POST", list(status_code = 202))
  stub(get_api_pred_url, "httr::content", "url")

  expect_equal(get_api_pred_url(df, "api_key"), "url")

  # emit data
  stub(get_api_pred_url, "httr::POST", list(status_code = 200))
  stub(get_api_pred_url, "httr::content", "data")
  stub(get_api_pred_url, "jsonlite::fromJSON", identity)

  expect_equal(get_api_pred_url(df, "api_key"), "data")

  # throw expected error
  stub(get_api_pred_url, "httr::POST", list(message = "failed call"))
  stub(get_api_pred_url, "httr::http_status", identity)
  stub(get_api_pred_url, "httr::status_code", identity)

  expect_error(get_api_pred_url(df, "api_key"), "failed call")
})

test_that("get_pred_from_url works and return expected result", {
  comment_id <- c("1", "2", "3")
  sentiment <- c(2, 4, 1)
  df <- data.frame(comment_id, sentiment)

  # emit data
  stub(get_pred_from_url, "httr::GET", list(status_code = 200))
  stub(get_pred_from_url, "httr::content", df)
  stub(get_pred_from_url, "jsonlite::fromJSON", identity)

  expect_equal(get_pred_from_url(df), df)

  # show busy
  stub(get_pred_from_url, "httr::GET", list(status_code = 202))
  expect_equal(get_pred_from_url(df), "Busy")

  # throw expected error
  stub(get_pred_from_url, "httr::GET", list(status_code = 505))
  stub(get_pred_from_url, "httr::http_status", list(message = "can't reach server"))
  expect_error(get_pred_from_url(df), "can't reach server")
})

test_that("transform_prediction_for_database works and return expected result", {
  prediction <- readRDS(here::here("tests/prediction.rds"))

  result <- transform_prediction_for_database(prediction)

  expect_true(nrow(result) == nrow(prediction))
  expect_true(result$comment_id |> duplicated() |> sum() == 0)
  expect_true(inherits(result$super_category, "list"))
  expect_true(inherits(result$category, "list"))

  # throw expected error
  expect_error(transform_prediction_for_database(select(prediction, -sentiment)))
})

test_that("track_api_job correctly handles completed job", {
  test_pred <- readRDS(here::here("tests/prediction.rds"))

  # Create a mock for the `get_pred_from_url` function
  stub(track_api_job, "get_pred_from_url", test_pred) # return a test prediction dataframe

  # Create a mock for the database connection (DBI::dbGetQuery and DBI::dbExecute)
  stub(track_api_job, "DBI::dbGetQuery", data.frame()) # return a test result
  m <- mock(TRUE, cycle = TRUE)
  stub(track_api_job, "DBI::dbExecute", m) # return a success status
  stub(track_api_job, "dplyr::rows_update", TRUE) # return a success status

  # Create a test job
  test_job <- data.frame(job_id = 1, url = "http://example.com", trust_id = 123)

  # Call the function with the mocks - Check if the result is a dataframe
  track_api_job(test_job, conn = NULL, write_db = FALSE) |>
    expect_identical(test_pred)

  # expect DBI::dbExecute is called twice
  expect_called(m, 2)

  # Create a new mock for the database connection (DBI::dbExecute)
  m2 <- mock(TRUE, cycle = TRUE)
  stub(track_api_job, "DBI::dbExecute", m2) # return a success status

  # Call the function with the mocks - Check it completes
  track_api_job(test_job, conn = NULL, write_db = TRUE) |>
    expect_output("Job 1 prediction has been successfully written to database")

  # expect DBI::dbExecute is called twice
  expect_called(m2, 2)
})

test_that("track_api_job correctly handles pending job", {
  test_pred <- "http://example.com"

  # Create a mock for the `get_pred_from_url` function
  stub(track_api_job, "get_pred_from_url", test_pred) # return a url

  # Create a test job
  test_job <- data.frame(job_id = 1, url = "http://example.com", trust_id = 123)

  # Check if still busy
  track_api_job(test_job, conn = NULL, write_db = FALSE) |>
    expect_output("Checking Job 1 \\nJob 1 is still busy")

  track_api_job(test_job, conn = NULL, write_db = TRUE) |>
    expect_output("Checking Job 1 \\nJob 1 is still busy")
})

test_that("track_api_job correctly handles failed job", {
  # Create a test job
  test_job <- data.frame(job_id = 2, url = "http://example.com", trust_id = 123)

  # Mock the behavior of `get_pred_from_url` to simulate failure
  stub(track_api_job, "get_pred_from_url", function() stop("Simulated error"))

  # Create a mock for the database connection DBI::dbGetQuery
  m <- mock(TRUE)
  stub(track_api_job, "DBI::dbGetQuery", m) # return a test result

  # Call the function with the mocks

  track_api_job(test_job, conn = NULL, write_db = FALSE) |>
    expect_output("Job 2 failed")

  expect_called(m, 1)
})


test_that("check_api_job works and return expected result", {
  api_table <- data.frame(
    job_id = 1,
    url = "url",
    date = as.POSIXct("2023-09-15 12:00:00", tz = "UTC"),
    no_comments = 5000,
    trist_id = "phase_2_demo",
    email = NA,
    user = NA,
    status = "uploaded"
  )

  # mock lubridate::now to return a particular time (20mins from the job table time)
  stub(check_api_job, "lubridate::now", as.POSIXct("2023-09-15 12:20:00", tz = "UTC"))

  # no pending job - status uploaded
  # mock database connection dplyr::tbl to return job table with status 'completed'
  stub(check_api_job, "dplyr::tbl", api_table)
  result1 <- check_api_job("pool", "phase_2_demo", schedule_time = 10)
  expect_equal(result1, list("latest_time" = NULL, "estimated_wait_time" = 0))


  # still busy if job status is submitted
  # mock database connection dplyr::tbl to return job table with status 'submitted'
  stub(check_api_job, "dplyr::tbl", mutate(api_table, status = "submitted"))

  result2 <- check_api_job("pool", "phase_2_demo", schedule_time = 10)

  expect_equal(result2, list(
    "latest_time" = as.POSIXct("2023-09-15 12:00:00", tz = "UTC"),
    "estimated_wait_time" = 20
  ))


  # still busy if job status is completed
  # mock database connection dplyr::tbl to return job table with status 'completed'
  stub(check_api_job, "dplyr::tbl", mutate(api_table, status = "submitted"))

  result3 <- check_api_job("pool", "phase_2_demo", schedule_time = 10)

  expect_equal(result3, list(
    "latest_time" = as.POSIXct("2023-09-15 12:00:00", tz = "UTC"),
    "estimated_wait_time" = 20
  ))
})
