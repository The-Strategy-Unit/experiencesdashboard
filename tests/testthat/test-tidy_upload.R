test_that("text cleaning works", {
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
