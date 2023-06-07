library(tidyverse)


df <- read_delim('data/migrants_origin.csv',delim = ';')


df2 <- df %>% 
  group_by(country_code_origin,country_code,for_group_id,for_group,for_division) %>% 
  summarise(N = sum(p))

df2 %>% 
  write_csv('data/migrants_origin2.csv')
