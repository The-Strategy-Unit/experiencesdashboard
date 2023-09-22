test_that("prep_data_for_comment_table works", {
  withr::local_envvar("R_CONFIG_ACTIVE" = "phase_2_demo")
  
  test <- prep_data_for_comment_table(phase_2_db_data, in_tidy_format = FALSE)

  expect_true(inherits(test$`Sub-Category`, "character"))
  expect_true(inherits(test$`Category`, "character"))
  expect_identical(names(test), c("Date", "FFT Question", "FFT Score", "Comment Sentiment", "FFT Answer", "Sub-Category", "Category"))

  test2 <- phase_2_db_data %>%
    get_tidy_filter_data(TRUE) %>%
    prep_data_for_comment_table()

  expect_true(inherits(test2$`Sub-Category`, "character"))
  expect_true(inherits(test2$`Category`, "character"))
  expect_identical(names(test2), c("Date", "FFT Question", "FFT Score", "Comment Sentiment", "FFT Answer", "Sub-Category", "Category"))

  test3 <- prep_data_for_comment_table(phase_2_db_data, in_tidy_format = TRUE)
  expect_identical(nrow(test), nrow(test2), nrow(test3))

  expect_error(phase_2_db_data %>%
    get_tidy_filter_data(TRUE) %>%
    prep_data_for_comment_table(in_tidy_format = F))
})

test_that("single_to_multi_label works", {
  test <- single_to_multi_label(phase_2_db_data)

  test2 <- phase_2_db_data %>%
    get_tidy_filter_data(TRUE) %>%
    single_to_multi_label()

  expect(nrow(test), nrow(test2))
  
    expect_equal(c("comment_id", "comment_type", "date", "comment_txt", 
                        "fft", "sentiment", "category", "super_category"), names(test2))
})


test_that("render_comment_table works", {
  test <- render_comment_table(phase_2_upload_data)
  expect_no_error(test)
  expect_true(inherits(test, "datatables"))
})


test_that("upset_plot works", {
  single_labeled_filter_data <- phase_2_db_data %>%
    get_tidy_filter_data(TRUE)

  upset_data <- single_labeled_filter_data %>%
    dplyr::rename(value = category) %>%
    one_hot_labels(column = "value")

  expect_true(inherits(upset_data, "data.frame"))

  all_categories <- single_labeled_filter_data %>%
    dplyr::pull(category) %>%
    unique() %>%
    na.omit() %>%
    sort()

  expect_no_error(
    plot <- upset_plot(upset_data,
      intersect = all_categories,
      min_size = 2,
      title = "Upset plot showing relationship between All Sub-categories"
    )
  )
  expect_true(inherits(plot, "ggplot"))
})

test_that("get_unique_value works", {
  phase_2_db_data %>%  head() %>% get_unique_value('sex') %>%  expect_no_error()
  phase_2_db_data %>%  head() %>% get_unique_value('date') %>%  expect_no_error()
})

test_that("parse_date works as expected", {
  date_s1 <- data.frame(
    date = c(
      "01 06 2020",
      "07 06 20",
      "10 06 2020",
      "12 06 2020",
      "17 06 2020",
      "29 06 2020"
    )
  )
  
  date_s2 <- data.frame(
    date = 
      c(
        "2020 06 01",
        "2020 06 07",
        "2020 06 10",
        "2020 06 12",
        "2020 06 17",
        "2020 06 29"
      )
  )
  
  date_s3 <- data.frame(
    date = c(
      "2020/06/01",
      "2020/06/07",
      "2020/06/10",
      "2020/06/12",
      "2020/06/17",
      "2020/06/29"
    )
  )
  
  date1 <- data.frame(
    date = as.Date(
      c(
        "01/06/2020",
        "07/06/20",
        "10/06/2020",
        "12/06/2020",
        "17/06/2020",
        "29/06/2020"
      ),
      "%d/%m/%y"
    )
  )
  
  date2 <- data.frame(
    date = as.Date(
      c(
        "2020/06/01",
        "2020/06/07",
        "2020/06/10",
        "2020/06/12",
        "2020/06/17",
        "2020/06/29"
      )
    )
  )
  
  all(
    parse_date(date_s1) == parse_date(date_s2),
    parse_date(date_s1) == parse_date(date_s3),
    parse_date(date_s1) ==  parse_date(date1),
    parse_date(date_s1) ==  parse_date(date2)
  ) |> expect_true()
  
  
  # rows without dates are removed
  date3 <- data.frame(
    date = as.Date(
      c(
        NA,
        "2020/06/01",
        "2020/06/07",
        "2020/06/10",
        "2020/06/12",
        "2020/06/17",
        "2020/06/29"
      )
    )
  )
  
  all(
    parse_date(date_s1) == parse_date(date3)
  ) |> expect_true()
})

test_that("get_sentiment_text works", {
  codes = c(1,2,3,3,5,6)
  test <- get_sentiment_text(codes)
  
  expect_equal(get_sentiment_text(c(5, 2,6, 9)), c("Negative", "Positive", NA, NA))
  
  expect_equal(test, c('Positive', 'Positive', 'Neutral/Mixed', 'Neutral/Mixed', 'Negative', NA))
  
  expect_equal(length(test), length(codes))
})

# mod_sentiment_utils_helper ----

test_that("plot_sentiment_trend works", {
  
  data <- phase_2_db_data %>% 
    head(20) %>%
    dplyr::mutate(
      sentiment = sample(1:5, nrow(.), replace = T)
    ) |>
    dplyr::mutate(date = as.Date(cut(date, "month"))) |>
    multigroup_calculated_data("date", "sentiment") %>%
    dplyr::mutate(sentiment = as.factor(sentiment))
  
  
  expect_no_error(
    plot <- data %>%
      plot_sentiment_trend(
        x = ~date,
        y = ~percent,
        colors = c("#DA291C", "#FAE100", "#009639"),
        event_id = 'event_id',
        plot_title = "Sentiment score over time",
        xaxis_title = "Date (Month)",
        yaxis_title = "% contribution",
      )
  )
  
  expect_true(inherits(plot, 'plotly'))
})

test_that("transform_sentiment works and return expected result", {
  
  comment_id <- c("1", "2", "3", '4')
  sentiment <- c(5, 2,6, 9)
  df <- data.frame(comment_id, sentiment)
  
  result <- transform_sentiment(df)
  
  expect_true(inherits(result, 'data.frame'))
  
  expect_true(inherits(result$sentiment, 'factor'))
  
  expect_equal(levels(result$sentiment),  c("Positive", "Neutral/Mixed", "Negative"))
 
  expect_equal(as.character(result$sentiment), c("Negative", "Positive", NA, NA))
   
  expect_equal(nrow(df), nrow(result))
})