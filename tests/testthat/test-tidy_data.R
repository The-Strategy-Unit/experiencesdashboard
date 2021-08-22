test_that("Data tidies in all trusts", {
  
  db_data <- dplyr::tbl(pool, 
                        dbplyr::in_schema("TEXT_MINING", 
                                          test_trust)) %>% 
    tidy_all_trusts(conn = pool, trust_id = test_trust) %>%
    dplyr::collect()
  
  testthat::expect_gt(nrow(db_data), 0)
})
