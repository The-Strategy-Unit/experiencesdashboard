test_that("filter_text works", {
  
  # check_match and match_term_or_stem works as expected
  comment = "There was a bit of a wait, which can’t be helped during shortage of doctors"
  match_term_or_stem(comment, all, "doctors") |>
    expect_true()
  match_term_or_stem(comment, all, "doctor") |>
    expect_true()
  
  # lowered_comments works as expected
  expect_equal("this   and ThaT" %>%
    lowered_comments(), "this and that")

  test1 <- matched_comments(
    lowered_comments = c(
      "tricky times, I recommend quick appraisals ",
      "time of my situation is quick",
      ">quick response time, competent appraisal of my situation"
    ),
    search_fn = all, search_strings = c("quick", "time", "appraisal")
  )

  expect_equal(sum(test1), 2)
  expect_type(test1, "logical")

  test2 <- return_search_text(
    text_data = data.frame(
      comment_txt = c(
        "tricky times, I recommend quick appraisals ",
        "time of my situation.", ">quick response time, competent appraisal of my situation"
      ),
      comment_type = c("comment_1", "comment_1", "comment_1")
    ),
    filter_text = "qUick, time, appraisal$, &!",
    comment_type_filter = "comment_1", search_type = "and",
    return_dataframe = FALSE
  )

  expect_equal(test2, c(
    "<hr/>tricky times, I recommend quick appraisals ",
    "<hr/>>quick response time, competent appraisal of my situation"
  ))

  test3 <- return_search_text(
    text_data = data.frame(
      comment_txt = c(
        "tricky times, I recommend quick appraisals ",
        "time of my situation.", ">quick response time, competent appraisal of my situation"
      ),
      comment_type = c("comment_1", "comment_1", "comment_1")
    ),
    filter_text = "qUick, timer",
    comment_type_filter = "comment_1", search_type = "and",
    return_dataframe = FALSE
  )

  expect_equal(test3, c("<hr/>no matching result"))

  # test 4 input_sanitizer() work correctly
  expect_equal(input_sanitizer("DocTORS, staffs"), c("doctors", "staffs"))
})

test_that("filter_text works - stemmed words version of each search term are searched and return", {
  text_data <- data.frame(
    comment_txt = c(
      "tricky times, I need emergency doctor appointment",
      "interesting doctor consultation, thanks", ">quick response time, all the doctors were helpful",
      "All the doctors did well"
    ),
    comment_type = c("comment_1", "comment_1", "comment_1", "comment_2")
  )
  filter_text <- "DocTORS"
  comment_type_filter <- "comment_1"
  search_type <- "and"
  return_dataframe <- FALSE

  search_strings <- input_sanitizer(filter_text)
  expect_equal(search_strings, c("doctors"))

  result <- filter_df(text_data, comment_type_filter)
  expect_equal(nrow(result), 3)

  comments <- result |>
    dplyr::pull(comment_txt)
  expect_equal(comments, c(
    "tricky times, I need emergency doctor appointment",
    "interesting doctor consultation, thanks", ">quick response time, all the doctors were helpful"
  ))

  expect_true(
    matched_rows <- comments |>
      lowered_comments() |>
      matched_comments(all, search_strings) |> all()
  )

  test1 <- return_search_text(text_data, filter_text, comment_type_filter, search_type, return_dataframe)
  expect_equal(test1, c(
    "<hr/>tricky times, I need emergency doctor appointment",
    "<hr/>interesting doctor consultation, thanks",
    "<hr/>>quick response time, all the doctors were helpful"
  ))

  test2 <- return_search_text(text_data, filter_text, NULL, search_type, FALSE)
  expect_equal(test2, c(
    "<hr/>tricky times, I need emergency doctor appointment",
    "<hr/>interesting doctor consultation, thanks",
    "<hr/>>quick response time, all the doctors were helpful",
    "<hr/>All the doctors did well"
  ))

  expect_true(return_search_text(text_data, filter_text, NULL, search_type, TRUE) |> inherits("data.frame"))
})
