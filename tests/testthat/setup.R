
test <- readr::read_csv(here::here('tests/test_data.csv'),
                        show_col_types = FALSE)

tidy_trust_data <-  test %>% 
    tidy_all_trusts()

