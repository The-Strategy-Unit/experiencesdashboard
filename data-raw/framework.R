## code to prepare `framework` dataset goes here
framework <- readxl::read_excel(
  here::here(
    app_sys(),
    "app/www",
    "FFT-QDC_Framework_MVP_version_20230925.xlsx"
  ),
  sheet = 2
)

usethis::use_data(framework, overwrite = TRUE)
