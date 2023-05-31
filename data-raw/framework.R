## code to prepare `framework` dataset goes here
framework <- readxl::read_excel(here::here(app_sys(), "app/www", "FFT-QDC Framework v5 - 20230428.xlsx"),
                                sheet=2) %>% 
  dplyr::arrange(Category, `Sub-category`) %>% 
  dplyr::select(-Examples)

usethis::use_data(framework, overwrite = TRUE)
