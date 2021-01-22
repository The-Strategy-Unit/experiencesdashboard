## code to prepare datasets goes here

## Load packages ----
library(tidyverse)



## Load results from Andreas
sentiment_txt_data <- readRDS(file = here("data-raw/sentiment_txt_data.rds"))
usethis::use_data(sentiment_txt_data, overwrite = TRUE)
