library(tidyverse)
library(readxl)

CS_country_portfoliosdimjun22_v2 <- read_excel("data/CS_country_portfoliosdimjun22_v2.xlsx", 
                                               sheet = "world-field baselines")


countries_df <- CS_country_portfoliosdimjun22_v2 %>% 
  filter(group=='country')


countries_df %>% 
  filter(total_p>=3000) %>% 
  select(country_code, total_citations=total_c, total_publications=total_p) %>%
  write_csv(file = 'data/countries_list.txt')
