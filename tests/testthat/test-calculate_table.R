test_that("Calculate table works", {
  test_table_imp <- calculate_table(
    table_data = tidy_trust_data,
    count_column = "category",
    comment_type = "comment_1"
  )

  expect_gt(nrow(test_table_imp), 0)

  test_table_best <- calculate_table(
    table_data = tidy_trust_data,
    count_column = "category",
    comment_type = "comment_2"
  )

  expect_gt(nrow(test_table_best), 0)

  expect_false(isTRUE(all.equal(test_table_best, test_table_imp)))
})

test_that("multigroup_calculated_data works", {
  
  test_data <- phase_2_db_data |>
    dplyr::mutate(date = as.Date(cut(date, 'month'))) |>
    multigroup_calculated_data('date', 'fft')
  
  test_data2 <- phase_2_db_data |>
    dplyr::mutate(date = as.Date(cut(date, 'month'))) |>
    get_tidy_filter_data(TRUE) |>
    multigroup_calculated_data('date', 'fft')
  
  expect_true(identical(test_data, test_data2))
  
  selected_date = '2020-05-01'
  expect_equal(
    phase_2_db_data |>
      filter(format(as.Date(date), "%Y-%m") == format(as.Date(selected_date), "%Y-%m"),
             fft == 1) |> nrow(),
    test_data |>
      filter(fft == 1,
             date == selected_date) |> 
      pull(n)
  )
  
  expect_equal(ncol(test_data), 4)
  expect_equal(sum(test_data$n), nrow(phase_2_db_data))
  expect_true(max(test_data$percent) <= 100)
})
