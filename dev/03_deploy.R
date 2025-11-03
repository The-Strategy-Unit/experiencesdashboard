rsconnect::deployApp(
  appName = "experiencesdashboard",
  appTitle = "Test: patient experiences dashboard",
  appFiles = c("DESCRIPTION", "NAMESPACE", "R/", "inst/", "data/", "data-raw/", "app.R"),
  server = "connect.strategyunitwm.nhs.uk",
  appId = 210,
  lint = FALSE,
  forceUpdate = TRUE
)
