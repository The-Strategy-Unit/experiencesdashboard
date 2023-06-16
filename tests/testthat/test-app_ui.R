# app_ui ----
test_that("app_ui works", {
  expect_snapshot(app_ui("id"))
})

# mod_category_criticality_ui ----
test_that("mod_category_criticality_ui works", {
  expect_snapshot(mod_category_criticality_ui("id"))
})

# mod_click_tables_ui ----
test_that("mod_click_tables_ui works", {
  expect_snapshot(mod_click_tables_ui("id"))
})

# mod_data_management_ui ----
test_that("mod_data_management_ui works", {
  expect_snapshot(mod_data_management_ui("id"))
})

# mod_demographics_ui ----
test_that("mod_demographics_ui works", {
  expect_snapshot(mod_demographics_ui("id"))
})

# mod_documentation_page_ui ----
test_that("mod_documentation_page_ui works", {
  expect_snapshot(mod_documentation_page_ui("id"))
})

# mod_trend_overlap_ui ----
test_that("mod_trend_overlap_ui works", {
  expect_snapshot(mod_trend_overlap_ui("id"))
})

# mod_patient_experience_ui ----
test_that("mod_patient_experience_ui works", {
  expect_snapshot(mod_patient_experience_ui("id"))
})

# mod_trend_ui ----
test_that("mod_trend_ui works", {
  expect_snapshot(mod_trend_ui("id"))
})

# mod_report_builder_ui ----
test_that("mod_report_builder_ui works", {
  expect_snapshot(mod_report_builder_ui("id"))
})

# mod_search_text_ui ----
test_that("mod_search_text_ui works", {
  expect_snapshot(mod_search_text_ui("id"))
})

# mod_summary_record_ui ----
test_that("mod_summary_record_ui works", {
  expect_snapshot(mod_summary_record_ui("id"))
})

# mod_summary_ui ----
test_that("mod_summary_ui works", {
  expect_snapshot(mod_summary_ui("id"))
})