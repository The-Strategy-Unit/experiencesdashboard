
test_trust <- "trust_b"

pool <- pool::dbPool(drv = odbc::odbc(),
                     driver = "Maria DB",
                     server = Sys.getenv("HOST_NAME"),
                     UID = Sys.getenv("DB_USER"),
                     PWD = Sys.getenv("MYSQL_PASSWORD"),
                     database = "TEXT_MINING",
                     Port = 3306)

tidy_trust_data <- dplyr::tbl(pool,
                              dbplyr::in_schema("TEXT_MINING",
                                                test_trust)) %>% 
  dplyr::arrange(desc(date)) %>% 
  head(1000) %>% 
  tidy_all_trusts(conn = pool) %>%
  dplyr::collect()

test <- dplyr::tbl(pool, dbplyr::in_schema("TEXT_MINING", test_trust)) %>% 
  tidy_all_trusts(conn = pool) # %>%
  dplyr::collect()
