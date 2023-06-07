library(tidyverse)
library(rvest)
library(countrycode)

names_wos <- read_excel('data/country_portfolios_WOS.xlsx') %>% 
  group_by(eregroupement) %>% 
  summarise(Country=unique(eregroupement))

    
names_wos <- names_wos %>% 
  mutate(iso2c = countrycode(Country,origin = 'country.name.en', destination = 'iso2c'),
         iso2c = case_match(Country,
                             'Kosovo' ~'XK',
                             'Yugoslavia' ~ 'YU',
                             .default = iso2c)) 

names_wos  %>% 
  select(-Country) %>% 
  write_csv('data/country_codes.csv')
  
  
  
  
