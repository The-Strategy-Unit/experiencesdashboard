test_that("split data spc works", {
  
  tidy_trust_data <- dplyr::tbl(pool,
                                dbplyr::in_schema("TEXT_MINING",
                                                  "trust_a")) %>% 
    tidy_all_trusts(conn = pool, trust_id = "trust_a") %>%
    dplyr::collect()
  
  monthly <- split_data_spc(tidy_trust_data, variable = "fft", 
                            chunks = "monthly")
  
  equal <- split_data_spc(tidy_trust_data, variable = "fft", 
                          chunks = 12)
  
  expect_gt(nrow(monthly), 0)
  
  expect_equal(length(unique(equal$date)), 12)
})

