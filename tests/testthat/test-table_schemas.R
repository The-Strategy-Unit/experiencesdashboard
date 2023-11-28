test_that("Validate all trusts' table schema", {
  skip_on_ci()

  # Create  DB connection pool
  pool <- get_pool()

  onStop(function() {
    pool::poolClose(pool)
  })

  principal_table <- "phase_2_demo"

  # NUH
  expect_identical(
    DBI::dbGetQuery(pool, "DESCRIBE trust_NUH"),
    DBI::dbGetQuery(pool, paste0("DESCRIBE ", principal_table))
  )
  
  # LPT
  expect_identical(
    DBI::dbGetQuery(pool, "DESCRIBE trust_LPT"),
    DBI::dbGetQuery(pool, paste0("DESCRIBE ", principal_table))
  )
  
  # NEAS
  expect_identical(
    DBI::dbGetQuery(pool, "DESCRIBE trust_NEAS"),
    DBI::dbGetQuery(pool, paste0("DESCRIBE ", principal_table))
  )
})
