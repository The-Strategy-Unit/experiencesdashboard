
<!-- README.md is generated from README.Rmd. Please edit that file -->

# experiencesdashboard

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## About

This read me is based on version [0.7.2](https://github.com/CDU-data-science-team/experiencesdashboard/tree/0.7.2)

The Experience dashboard is the front end tool (Shiny App) for the [Patient Experience Qualitative Data Categorisation project](https://cdu-data-science-team.github.io/PatientExperience-QDC/), funded by NHS England and hosted by Nottinghamshire Healthcare NHS Foundation Trust. It ties a machine learning back-end, the [Pxtextmining](https://cdu-data-science-team.github.io/pxtextmining/reference/API/API/), to the data source via an API and present metrics and graphs to help clinical staffs and managers quickly gain insight from patient experience data collected via the [NHS England Friends and Family Test](https://www.england.nhs.uk/fft/).

[Link to hosted version](https://connect.strategyunitwm.nhs.uk/qdc_public/). Please note that some of the data has been modified for the purposes of
demonstration so it should NOT be used for reporting and is not accurate
in several important ways.

### Folder Stucture

This shiny app is built using [golem](https://engineering-shiny.org/golem.html#golem) and follows the advised [folder structure](https://engineering-shiny.org/golem.html#understanding-golem-app-structure). Because a golem application is an R package, this package incorporates all the benefits that comes with R package development and management process. 

Below is the folder structure and general description of the content of each important folder.

``` r
fs::dir_tree(recurse = 0)
```

```
experiencesdashboard
├── .github/workflows
├── LICENSE
├── LICENSE.md
├── NEWS.md
├── README.md
├── CODE_OF_CONDUCT.md
├── app.R
├── DESCRIPTION
├── NAMESPACE
├── vignettes/
├── R/
├── dev/
├── inst/
├── tests/
├── data/
├── data-raw/
├── man/
└── rsconnect/
```

------

| Name | Link | Description |
| ---- | ---- | ----------- |
| .github/workflows | [[Link](/.github/workflows)]  | Github Action workflow files that automate the `R CMD check` and `deployment` process |
| app.R | [[Link](.)]  | A `golem` file that contains the set of functions needed to deploy the app on any platform such as Posit Connect, etc |
| DESCRIPTION | [[Link](.)]  | A standard `R` package file containing series of metadata about the package including the package dependencies required to run the app. It forms a key part of the dependency management |
| NAMESPACE | [[Link](.)]  | A standard `R` package file that contains functions to import and from which package and what functions to export |
| R/ | [[Link](R/)]  | Standard `R` package folder holding all the package functions. It contains the functions required for the app core functionality such as the Server function `app_server.R`, UI function `app_ui.R`, all the modules `mod_*` files and utilitarian/business logic functions `fct_*.R` or `*utils*.R`/ or other `.R` files. It also contains an important file, `run_app.R`, which in turn contains the [`run_app()`](R/run_app.R) function that is called to launch the app |
| dev/ | [[Link](dev/)]  | This folder contains utilitarian files used during development phase only and not core functionalities of the app.  |
| inst/ | [[Link](inst)]  | It contains the [`golem-config.yml`](inst/golem-config.yml) file and [`inst/app/www/`](inst/app/www/) files. [`inst/app/www/`](inst/app/www/) contains all files that are made available at application run time, while [`golem-config.yml`](inst/golem-config.yml) is an important yaml file to configure the app. |
| test/ | [[Link](tests/)]  | This folder contains the codes for the unit test infrastructure |
| data/ | [[Link](data/)]  | Contains `.rda` data used by the app during runtime |
| data-raw/ | [[Link](data-raw/)]  | It contains scripts to prepare dataset in the `data` folder. We also store some data in there that are not required at runtime |
| man/ | [[Link](man/)]  | This is a standard `R` package folder containing automatically filled files for function documentations |
| rsconnect/ | [[Link](rsconnect/)]  |  Contains posit connect deployment files |

### Built With

- [golem](https://github.com/ThinkR-open/golem)
- [R](https://www.r-project.org/)
- [GitHub Actions](https://github.com/features/actions)
- [pxtextmining API](https://cdu-data-science-team.github.io/pxtextmining/reference/API/API/)

## Using this Solution

The implementation you will follow will depend on your use case and if you have access to the project database (for internal users).

### A. Installation: 

Follow this approach if you have the right access to this project database

#### Install the package

{experiencesdashboard} is not currently on CRAN, so you will have to install it directly from Github.

``` r
# install.packages("devtools")
devtools::install_github("CDU-data-science-team/experiencesdashboard")
```

#### Run the app 
``` r
library(experiencesdashboard) # load the package
# usethis::edit_r_environ() # add the environment variables to connect to the database. see `get_pool()`
Sys.setenv("R_CONFIG_ACTIVE" = "my_config") # set the configuration to use inline with the `golem-config.yml` file
run_app() # run the app
```

### B. Local Implementation with your own data

This package uses data from a database to populate the dashboard. The [`get_pool()`](R/fct_app_server-helpers.R) set up the DB connection. To use this package locally and on your own dataset, you will need to do the following:

1. Clone the repo. [cloning-a-repository](https://docs.github.com/en/repositories/creating-and-managing-repositories/cloning-a-repository)

2. Connect your data either  
    a. _**Via Database Connection**_: Set the environmental variables needed to establish a Database connection (see [`get_pool()`](R/fct_app_server-helpers.R))
   
    b. _**Via Local file**_: Read in your data into the `db_data` object in the [`app_server.R`](R/app_server.R) by replacing `db_data <- get_db_data(pool, get_golem_config("trust_name")))` with e.g. `db_data <- read.csv(''my_data_path.csv)`. With this you can safely ignore the preceding codes that creates the db connection.

4. Set up your data/app configuration: If you need to use this app locally, then you will need to set up a configuration for your use case in the [`golem-config.yml`](inst/golem-config.yml) ([get help here](https://engineering-shiny.org/golem.html#golem-config))

5. choose your configuration: run `Sys.setenv("R_CONFIG_ACTIVE" = "my_config")` and run the app with `run_app()`. please see sample code in the [`run_dev.R`](dev/run_dev.R)


#### Format your data for the app

Your data type must follow the schema in [Database table schema](data-raw/phase_2_schema.csv) before you can load the data into the app in step 2 above. Though not all the columns are required but to ignore any will depend on your configuration in step 3 above. 

  i. You can safely ignore these columns without any modification:  `'extra_variable_1', 'extra_variable_2', 'extra_variable_3'`
  
  ii. To ignore the following columns `
   'location_2', 'location_3', 'sex', 'gender', 'age', 'ethnicity', 'sexuality', 'disability', 'religion'`, You need to set your configuration file accordingly. A sample configuation is this:
   
  ```
  my_config:
    trust_name: my_config
    comment_1: Why did you answer this way?
    comment_2: What could be improved?
    question_1: fft
    location_1: Division
  ```
Please [get in touch](mailto:chris.beeley@gmail.com) if you need additional help implementing this solution locally.

## Code of Conduct

Please note that the experiencesdashboard project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

## License

Distributed under a MIT License. _See [LICENSE.md](/LICENSE.md) for more information._
