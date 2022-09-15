## code to prepare dataset goes here
  # I created this file and folder by using usethis::use_data_raw()
library(tidyverse)
# load high school and beyond data ---
hsb <- haven::read_spss("data-raw/HSB.sav")

# prep data
hsb <- hsb %>%
  mutate_at(vars(minority, female, catholic),
            as_factor)

usethis::use_data(hsb, overwrite = TRUE)
