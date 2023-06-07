library(tidyverse)
library(rvest)

content <- read_html("https://www.iban.com/country-codes")
tables <- content %>% html_table(fill = TRUE)

table <- tables[[1]]

table %>% 
  rename(iso2c = 'Alpha-2 code', iso3c = 'Alpha-3 code') %>% 
  select(-Numeric) %>% 
  write_csv('data/country_code.csv')
