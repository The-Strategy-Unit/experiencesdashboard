test_that("API and Framework are in sync", {
  single_filter_data <- phase_2_db_data %>%
    dplyr::mutate(across(category, ~ purrr::map(.x, jsonlite::fromJSON))) %>% # unserialise the category data from json into list
    tidyr::unnest(category) %>% # Unnest the category column into rows and columns
    dplyr::mutate(super_category = assign_highlevel_categories(category))

  not_in_framewk <- single_filter_data %>%
    filter(super_category == "Other Category") %>%
    pull(category) %>%
    unique()
  expect_true(all(not_in_framewk == c("Labelling not possible", "Admission")))
})

# test_that("API prediction function is working...", {
#   text_data <- data.frame(
#     comment_id = c(1, 2, 3),
#     comment_text = c(
#       "Food was cold", "Those satffs are just too amazing",
#       "I hate the meal"
#     ),
#     question_type = c("what_good", "could_improve", "nonspecific")
#   )
# 
#   expect_error(api_pred(text_data |>
#     select(-question_type)))
# 
#   expect_no_error(api_pred(text_data))
# 
#   preds <- text_data |>
#     batch_predict()
# 
#   expect_equal(nrow(preds), 3)
#   expect_true(all(c("comment_id", "comment_text", "labels") %in% names(preds)))
#   expect_equal(sum(is.na(preds$labels)), 0)
# })
# 
# test_that("api_question_code works", {
#   expect_equal(api_question_code("What did we do well"), "what_good")
#   expect_equal(api_question_code("What could be improved"), "could_improve")
#   expect_equal(api_question_code("why that answer"), "nonspecific")
# })

test_that("api_pred_url works and return expected result", {
  
  comment_id <- c("1", "2", "3")
  comment_text <- c(
    "Nurse was great but very difficult to find parking",
    "The ward was freezing.",
    ""
  )
  question_type <- c("what_good", "could_improve", "nonspecific")
  df <- data.frame(comment_id, comment_text, question_type)
  
  
  expect_error(api_pred_url(df |>
                          select(-question_type)))
  
  expect_no_error(api_pred_url(df))
  
  api_result <- api_pred_url(df)
  expect_true(inherits(api_result,  c("character", 'data.frame')))
  
  expect_no_error(get_pred_from_url(api_result))
  expect_true(any(inherits(get_pred_from_url(api_result), 'data.frame') | get_pred_from_url(api_result) == 'Busy'))
})
