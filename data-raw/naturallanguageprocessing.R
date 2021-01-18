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

usethis::use_data(my_dataset, overwrite = TRUE)
