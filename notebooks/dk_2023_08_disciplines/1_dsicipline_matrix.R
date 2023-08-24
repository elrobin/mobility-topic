library(tidyverse)
library(countrycode)
library(readxl)
library(plotly)
library(ggrepel)
library(scales)

df <- read_excel('data/discipline_country_migrations.xlsx',sheet = 'Data')

# I aggregate to the division level
df <- df %>% 
  group_by(for_division,country_code_origin,country_code) %>% 
  summarise(N = sum(N)) %>%
  ungroup() %>% 
  mutate(origin = countrycode(country_code_origin, 
                                           origin = 'iso2c',
                                           destination = 'country.name'),
         destination = countrycode(country_code, 
                                           origin = 'iso2c',
                                           destination = 'country.name'),
         continent_origin = countrycode(country_code_origin, 
                                        origin = 'iso2c',
                                        destination = 'continent')) %>% 
  filter(!is.na(origin),!is.na(destination)) %>% 
  select(-c(country_code_origin,country_code))


us_df <- df %>% 
  filter(destination=='United States')


#I keep origin countries with high N

#double normalization

us_normalized <- us_df %>%
  group_by(origin) %>% 
  mutate(origin_total = sum(N),
         P_origin = N/origin_total) %>% 
  group_by(for_division) %>% 
  mutate(for_total = sum(N),
         P_for = N/for_total)

keep_countries <- us_df %>% 
  group_by(origin) %>% 
  summarise(N = sum(N)) %>% 
  arrange(-N) %>% 
  top_n(50) %>% pull(origin)


us_normalized %>% 
  # mutate(for_division = str_replace_all(for_division, ' ', '\n')) %>% 
  filter(origin %in% keep_countries) %>% 
  ggplot(aes(P_origin,P_for,size=N, color=continent_origin, label=for_division))+
  # geom_point()+
  geom_text()+
  theme(legend.position = 'bottom')

ggplotly(
us_normalized %>% 
  # mutate(for_division = str_replace_all(for_division, ' ', '\n')) %>% 
  filter(origin %in% keep_countries) %>% 
  ggplot(aes(P_origin,P_for,size=N, color=for_division, label=origin,
             text= paste('% origin: ',percent(P_origin),
                         '<br>% field: ', percent(P_for),
                         '<br>N: ',N,
                         '<br>field: ', for_division,
                         '<br>origin: ',origin))
             )+
  # geom_point()+
  # geom_text_repel()+
  geom_text()+
  theme(legend.position = 'none'),
tooltip = 'text'
)
