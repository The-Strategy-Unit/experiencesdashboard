files <- c(
  "DESCRIPTION",
  "NAMESPACE",
  "app.R",
  fs::dir_ls("data"),
  fs::dir_ls("data-raw"),
  fs::dir_ls("R"),
  fs::dir_ls("inst", recurse = TRUE, type = "file")
)

rsconnect::writeManifest(appFiles = files)