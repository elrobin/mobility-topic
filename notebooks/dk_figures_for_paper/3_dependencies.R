library(tidyverse)
library(countrycode)
library(readxl)
library(plotly)
library(ggrepel)
library(scales)
library(ggpubr)
library(ggh4x)

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
         origin_region = countrycode(country_code_origin, 
                                     origin = 'iso2c',
                                     destination = 'region'),
         destination_region = countrycode(country_code, 
                                          origin = 'iso2c',
                                          destination = 'region')) %>% 
  filter(!is.na(origin),!is.na(destination)) %>% 
  select(-c(country_code_origin,country_code))

df %>% group_by(origin) %>% summarise(N = sum(N)) %>% summary()

min_countries_selection <- df %>% group_by(origin) %>% 
  summarise(N = sum(N)) %>% 
  arrange(-N) %>% 
  filter(N>3000) %>% pull(origin)

df <- df %>% 
  filter(origin %in% min_countries_selection, 
         destination %in% min_countries_selection)

df %>% 
  group_by(destination_region,destination) %>% 
  summarise(N=sum(N)) %>% 
  arrange(-N) %>% 
  mutate(destination = fct_reorder(destination, -N)) %>% 
  group_by(destination_region) %>% 
  top_n(1) %>% 
  filter(!is.na(destination_region)) 

selection_countries <- c('United States','United Kingdom','China','Saudi Arabia','India','Brazil','South Africa')
# selection_countries <- c('United States','United Kingdom','China','India','Brazil')


countries_flows <- df %>% 
  group_by(origin,origin_region,destination,destination_region) %>%
  summarise(n_origin_destination = sum(N)) %>% 
  group_by(origin) %>% 
  mutate(n_origin = sum(n_origin_destination)) %>% 
  group_by(destination) %>% 
  mutate(n_destination = sum(n_origin_destination)) %>% 
  mutate(p_destination = n_origin_destination/n_destination,
         p_origin =  n_origin_destination/n_origin) %>% 
  ungroup()



# relative importance of countries as origins and destinations ------------
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
gg_color_hue(7)
strip <- strip_themed(background_y = elem_list_rect(fill =
                                                      c("#53B400","#F8766D",
                                                        "#A58AFF", "#00C094",
                                                        "#FB61D7","#C49A00",
                                                        "#00B6EB"))) #I sort the colors to match regions

countries_as_destinations <- function(df,countries_flows,selection_countries){
  
  selection_df <- countries_flows %>% 
    filter(destination%in% selection_countries) %>% #I keep the selection of destinations
    group_by(destination) %>% #for each country of destinations
    # top_n(n=5,wt = p_destination) %>% View()#I keep the countries for which 
    top_n(n=5,wt = p_origin) %>%  #I keep the countries for which
    mutate(origin = tidytext::reorder_within(origin, p_origin, within = destination),
           destination = str_replace(destination,' ','\n'))
  
  selection_df %>% 
    ggplot(aes(origin,p_origin, fill=origin_region))+
    geom_col()+
    scale_y_continuous(labels = percent)+
    coord_flip()+
    labs(x='most dependent origins',y= 'proportion of emmigrants from those origins\nthat go to the selected country',  fill='region')+
    tidytext::scale_x_reordered() +
    facet_grid2(destination~., scales = 'free',strip = strip)
}

countries_as_origins <- function(df,countries_flows,selection_countries){
  
  selection_df <- countries_flows %>% 
    filter(origin %in% selection_countries) %>% #I keep the selection of destinations
    group_by(origin) %>% #for each country of destinations
    # top_n(n=5,wt = p_destination) %>% View()#I keep the countries for which 
    top_n(n=5,wt = p_destination) %>%  #I keep the countries for which
    mutate(destination = tidytext::reorder_within(destination, p_destination, within = origin),
           origin = str_replace(origin,' ','\n'))
  
  selection_df %>% 
    ggplot(aes(destination,p_destination, fill=destination_region))+
    geom_col()+
    scale_y_continuous(labels = percent)+
    coord_flip()+
    labs(x='most dependent destinations',
         y= 'proportion of inmigrants going to those destinations\nthat come from the selected country', fill='region')+
    tidytext::scale_x_reordered() +
    facet_grid2(origin~., scales = 'free',strip = strip)
}

plt_1a <- countries_as_destinations(df,countries_flows,selection_countries)
plt_1b <- countries_as_origins(df,countries_flows,selection_countries)

ggarrange(plt_1a, plt_1b, common.legend = TRUE, legend = 'bottom',labels = 'AUTO')

ggsave('results/figures/4_origin_destination_dependencies.png', width = 10, height = 7)



