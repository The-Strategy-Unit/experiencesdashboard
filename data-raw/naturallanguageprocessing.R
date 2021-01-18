## code to prepare datasets goes here

## Load packages ----
library(tidyverse)

# Load data ----
## Load naturallanguageprocessing data from Chris
load(url("https://raw.github.com/ChrisBeeley/naturallanguageprocessing/master/cleanData.Rdata"))

categoriesTable <- categoriesTable %>% 
  as_tibble()

improve_words <- improve_words %>% 
  as_tibble()

trustData <- trustData %>% 
  as_tibble()

use_data <- use_data %>% 
  as_tibble()

## Load results from Andreas




sentiment_txt_data <- readRDS(file = here("data-raw/sentiment_txt_data.rds"))
usethis::use_data(sentiment_txt_data, overwrite = TRUE)
