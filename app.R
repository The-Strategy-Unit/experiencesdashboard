# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# Sys.setenv("R_CONFIG_ACTIVE" = "phase_2_demo")
# Sys.setenv("R_CONFIG_ACTIVE" = "phase_2_trust")
# Sys.setenv("R_CONFIG_ACTIVE" = "trust_NUH")
# Sys.setenv("R_CONFIG_ACTIVE" = "trust_LPT")
# Sys.setenv("R_CONFIG_ACTIVE" = "trust_GOSH")

pkgload::load_all(export_all = FALSE,
                  helpers = FALSE,
                  attach_testthat = FALSE)
options( "golem.app.prod" = TRUE)
experiencesdashboard::run_app() # add parameters here (if any)
