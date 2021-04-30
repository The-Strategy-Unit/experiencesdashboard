# Building a Prod-Ready, Robust Shiny Application.
# 
# README: each step of the dev files is optional, and you don't have to 
# fill every dev scripts before getting started. 
# 01_start.R should be filled at start. 
# 02_dev.R should be used to keep track of your development during the project.
# 03_deploy.R should be used once you need to deploy your app.
# 
# 
###################################
#### CURRENT FILE: DEV SCRIPT #####
###################################

# Engineering

stop()

## Dependencies ----
## Add one line by package you want to add as dependency
usethis::use_package( "thinkr" )
usethis::use_package( "dplyr" )
usethis::use_package( "tibble" )
usethis::use_package( "ggplot2" )
usethis::use_package( "purrr" )
usethis::use_package( "tidyr" )
usethis::use_package( "readr" )
usethis::use_package( "stringr" )
usethis::use_package( "forcats" )
usethis::use_package( "reactable" )
usethis::use_package( "tidytext" )
usethis::use_package( "janitor" )
usethis::use_package( "UpSetR" )
usethis::use_package( "tibbletime" )
usethis::use_package( "qicharts2" )
usethis::use_package( "shinydashboard" )
usethis::use_package( "dashboardthemes" )
usethis::use_package( "shinyWidgets" )
usethis::use_package( "waiter" )
usethis::use_package( "shinyjqui" )
usethis::use_package( "timevis" )
usethis::use_package( "rmarkdown" )
usethis::use_package( "pander" )
usethis::use_package( "pool" )
usethis::use_package( "odbc" )
usethis::use_package( "DBI" )
usethis::use_dev_package("nottshc", remote = "CDU-data-science-team/nottshc")

## Add modules ----
## Create a module infrastructure in R/
golem::add_module( name = "sentiment" ) # Name of the module
golem::add_module( name = "patient_experience" ) # Name of the module
golem::add_module( name = "category_criticality" ) # Name of the module
golem::add_module( name = "fft" ) # Name of the module
golem::add_module( name = "report_builder" ) # Name of the module
golem::add_module( name = "click_tables" ) # Name of the module
golem::add_module( name = "search_text" ) # Name of the module

## Add helper functions ----
## Creates ftc_* and utils_*
golem::add_fct( "helpers" ) 
golem::add_utils( "helpers" )

## External resources
## Creates .js and .css files at inst/app/www
golem::add_js_file( "script" )
golem::add_js_handler( "handlers" )
golem::add_css_file( "custom" )

## Add internal datasets ----
## If you have data in your package
usethis::use_data_raw( name = "my_dataset", open = FALSE ) 

## Tests ----
## Add one line by test you want to create
usethis::use_test( "fft" )
usethis::use_test( "sentiment_tidy" )
usethis::use_test( "show_text" )
usethis::use_test( "search_text" )
usethis::use_test( "calculate_table" )
usethis::use_test( "tidy_data" )

# Documentation

## Vignette ----
usethis::use_vignette("experiencesdashboard")
devtools::build_vignettes()

## Code coverage ----
## (You'll need GitHub there)
usethis::use_github()
usethis::use_travis()
usethis::use_appveyor()

# You're now set! ----
# go to dev/03_deploy.R
rstudioapi::navigateToFile("dev/03_deploy.R")

