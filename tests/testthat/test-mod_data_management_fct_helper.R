test_that("data management data is well formatted", {
  selected_columns = c(
    "comment_id", "date", "location_1", "location_2", "location_3",
    "comment_type", "comment_txt", "fft", "category", "super_category", "sentiment", "sex",
    "gender", "age", "ethnicity", "sexuality", "disability", "religion",
    "extra_variable_1", "extra_variable_2", "extra_variable_3",
    "pt_id"
  )
  
  test1 <- prepare_data_management_data(head(phase_2_db_data, 100), 
          selected_columns, "comment_txt", comment_1 = 'why your answer', comment_2 =  NULL)
  test_1_no <- nrow(test1)
  data_no <- phase_2_db_data %>% head(100) %>% filter(comment_type %in% c('comment_1')) %>% nrow()
  expect_equal(test_1_no, data_no)
  expect_true(unique(test1$comment_type) == 'why your answer')
  
  test2 <- prepare_data_management_data(head(phase_2_db_data, 100), 
                   selected_columns, "comment_txt", comment_1 = 'why your answer', comment_2 =  'what could improve')
  expect_equal(nrow(test2), 100)
  expect_true(all(unique(test2$comment_type) == c('why your answer',  'what could improve')))
  
  # if some location and demography data a not in the data?
  test3 <- prepare_data_management_data(phase_2_db_data %>% 
                     head(100) %>% 
                     select(-location_3, -sex, -sexuality,  -religion, -disability), 
          selected_columns, "comment_txt", comment_1 = 'why your answer', comment_2 =  NULL)
  expect_equal(length(names(test3)), 14)
  
  
  # if empty column and columns with 'NA' are removed (ex. extra_variable_1)
  test3 <- prepare_data_management_data(phase_2_db_data %>% 
                     head(100) %>% 
                     select(-location_3, -sex, -sexuality,  -religion, -disability),
                   selected_columns, "comment_txt", comment_1 = 'why your answer', comment_2 =  NULL)
  expect_equal(length(names(test3)), 14)
})

test_that("prepare data for download function works", {
  
  test1 <- phase_2_db_data %>%
    filter(super_category == "[\"General\",\"General\"]") %>% 
    clean_super_category()
  expect_equal(unique(test1$super_category) %>% unlist(), '"General"')
  
  test2  = test1 %>% prepare_data_for_download() 
  expect_true(inherits(test2$super_category, 'character'))
})
