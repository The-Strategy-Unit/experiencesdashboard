test_that("text cleaning works", {
  
  template <- structure(
    list(location_1 = c("XXX", "YYY", "ZZZ"), 
         location_2 = c("xxxxxx", "yyyyyy", "zzzzzz"), 
         location_3 = c("abcdef", 
                        "ghijk", 
                        "lmonp"), 
         fft = c(5, 2, 1), 
         comment_1 = c(
           "Service too slow", 
           NA, 
           "????"), 
         comment_2 = c(NA, "NULL", NA), 
         date = structure(c(1625097600, 1625097600, 1625097600), 
                          class = c("POSIXct", "POSIXt"), tzone = "UTC"), 
         gender = c(NA, NA, NA), 
         age = c(NA, NA, NA), 
         ethnicity = c(NA, NA, NA)), 
    row.names = c(NA, -3L), class = c("tbl_df", "tbl", "data.frame"))
  
  test_template <- template %>% 
    clean_dataframe()
  
  expect_equal(test_template$comment_2, rep(NA_character_, 3))
  
  expect_equal(test_template$ethnicity, rep(NA, 3))
})
