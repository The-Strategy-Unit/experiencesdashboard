test_that("API and Framework are in sync", {
  single_filter_data <- db_data %>%
    dplyr::mutate(across(category, ~ purrr::map(.x, jsonlite::fromJSON))) %>% # unserialise the category data from json into list
    tidyr::unnest(category) %>% # Unnest the category column into rows and columns
    dplyr::mutate(super_category = assign_highlevel_categories(category))

  not_in_framewk <- single_filter_data %>%
    filter(super_category == "Other Category") %>%
    pull(category) %>%
    unique()
  expect_true(all(not_in_framewk == c("Labelling not possible", "Admission")))

  in_framework_not_prediction <- setdiff(unique(framework$`Sub-category`), unique(single_filter_data$category))

  expect_true(all(in_framework_not_prediction == c("")))
})

test_that("API prediction function is working...", {
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

test_that("api_question_code works", {
  expect_equal(api_question_code("What did we do well"), "what_good")
  expect_equal(api_question_code("What could be improved"), "could_improve")
  expect_equal(api_question_code("why that answer"), "nonspecific")
})
