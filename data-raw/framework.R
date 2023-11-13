## code to prepare `framework` dataset goes here
framework <- readxl::read_excel(
  here::here(
    app_sys(),
    "app/www",
    "FFT-QDC_Framework_MVP_version_20230925.xlsx"
  ),
  sheet = 2
) |>
  dplyr::select(Category,	`Sub-category`, `Sub-category description`,	Examples)

# Assign the color to use for plotting members of each Category
# inline with the color used in the framework document
color <- c(
  "#FFB81C", "#005EB8", "#330072", "#AE2573", "#DA291C",
  "#00A9CE", "#7C2855", "#ED8B00", "#009639", "#8A1538"
)
categories <- unique(framework$Category)

for (i in 1:length(categories)) {
  framework[framework$Category == categories[i], "color"] <- color[i]
}

usethis::use_data(framework, overwrite = TRUE)
