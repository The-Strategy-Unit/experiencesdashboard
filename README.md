
<!-- README.md is generated from README.Rmd. Please edit that file -->

# experiencesdashboard

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

## Installation

The development version is available from GitHub with:

``` r
# install.packages("devtools")
# devtools::install_github("CDU-data-science-team/experiencesdashboard")
```

## Running

Run with:

``` r
library(experiencesdashboard)
run_app()
```

There is a known bug where the report button does not work when the
application is downloaded as a package (as opposed to being cloned and
run). It relates to the file location of report.Rmd and if you know how
to fix this bug please make a PR 😁.

A hosted version can be found
[here](https://feedbackmatters.uk/rsconnect/experience_a/).
Please note the some of the data has been modified for the purposes of
demonstration so it should NOT be used for reporting and is not accurate
in several important ways.

## Code of Conduct

Please note that the experiencesdashboard project is released with a
[Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
