test_that("Searching text works", {
  
  checks <- matched_comments(
    lowered_comments = c(
      "tricky times, I recommend quick appraisals ",
      "time of my situation IS quick.",
      ">quick response time, competent appraisal of my situation"
    ),
    search_fn = all, search_strings = c("quick", "time", "appraisal")
  )
  expect_equal(sum(checks), 2)
  expect_true(is.logical(checks))

  text_data <- tibble::tribble(
    ~comment_type, ~comment_txt,
    "a", "hello world",
    "a", "world things",
    "a", "other stuff"
  )

  expect_equal(
    text_data |> return_search_text("&things,wo%^$rld",
      comment_type_filter = NULL,
      search_type = "or",
      return_dataframe = F
    ),
    c("<hr/>hello world", "<hr/>world things")
  )

  expect_equal(
    text_data |> return_search_text("thi!ngs,world>",
      comment_type_filter = "a", "and",
      return_dataframe = F
    ),
    "<hr/>world things"
  )

  test_text <- return_search_text(
    text_data = tidy_trust_data,
    filter_text = "Listen, staff",
    comment_type_filter = "comment_2",
    search_type = "or"
  )
  expect_equal(nrow(test_text), 8)

  test_text <- return_search_text(
    text_data = tidy_trust_data,
    filter_text = "Listen, staff",
    comment_type_filter = "comment_2",
    search_type = "and"
  )
  expect_equal(nrow(test_text), 1)

  text_data <- data.frame(
    comment_txt = c(
      "tricky times, I recommend quick appraisals ",
      "time of my situation.", ">quick response time, competent appraisal of my situation"
    ),
    comment_type = c("comment_1", "comment_1", "comment_1")
  )
  test_text <- return_search_text(
    text_data,
    filter_text = "qUick, time, appraisal$, &!",
    comment_type_filter = NULL, search_type = "and"
  )
  expect_equal(nrow(test_text), 2)
})
