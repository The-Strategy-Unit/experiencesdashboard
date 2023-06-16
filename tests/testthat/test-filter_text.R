test_that("filter_text works", {
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
    filter_text = "Uick, time, appraisal$, &!",
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
})
