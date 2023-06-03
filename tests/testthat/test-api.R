
test_that("API/Framework syn check", {
  single_filter_data <- filter_data %>%
    dplyr::mutate(across(category, ~ purrr::map(.x, jsonlite::fromJSON))) %>% # unserialise the category data from json into list 
    tidyr::unnest(category) %>% # Unnest the category column into rows and columns
    dplyr::mutate(super_category = assign_highlevel_categories(category))
  
  not_in_framewk  <- single_filter_data %>%
    filter(super_category == 'Other Category') %>% pull(category) %>% unique()
  expect_true(all(not_in_framewk == c("Labelling not possible" ,"Admission")))
  
  in_framework_not_prediction <- setdiff(unique(framework$`Sub-category`), unique(single_filter_data$category))
  
  expect_true(all(in_framework_not_prediction == c('')))
})
