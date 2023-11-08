library(tidyverse)
library(countrycode)
library(readxl)
library(plotly)
library(ggrepel)
library(scales)
library(ggpubr)
library(ggh4x)

df <- read_excel('data/discipline_country_migrations.xlsx',sheet = 'Data')
countries <- read_csv('data/countries_list.txt')$country_code

# I aggregate to the division level
df <- df %>% 
  group_by(for_division,country_code_origin,country_code) %>% 
  filter(country_code_origin %in% countries,
         country_code %in% countries) %>% 
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

selection_countries <- df %>% 
  group_by(destination_region,destination) %>% 
  summarise(N=sum(N)) %>% 
  arrange(-N) %>% 
  mutate(destination = fct_reorder(destination, -N)) %>% 
  group_by(destination_region) %>% 
  top_n(1) %>% 
  filter(!is.na(destination_region)) %>% 
  pull(destination)

regions_order <- df %>%
  group_by(origin_region) %>% 
  summarise(N = sum(N)) %>% 
  arrange(-N) %>%
  filter(!is.na(origin_region)) %>% 
  pull(origin_region)
# selection_countries <- c('United States','United Kingdom','China','Saudi Arabia','India','Brazil','South Africa')

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
                                                      c("#00B6EB",
                                                        "#C49A00",
                                                        "#F8766D",
                                                        "#00C094",
                                                        "#A58AFF",
                                                        "#53B400",
                                                        "#FB61D7"
                                                      )))#I manually sort the colors to match regions
regions_colors <- c("#C49A00","#00B6EB","#F8766D","#A58AFF","#53B400","#00C094",
                    "#FB61D7")

names(regions_colors) <- regions_order


countries_as_destinations <- function(df,countries_flows,selection_countries){
  
  selection_df <- countries_flows %>% 
    filter(destination%in% selection_countries) %>% #I keep the selection of destinations
    group_by(destination,origin_region) %>% #for each country of destinations
    summarise(p_origin = weighted.mean(p_origin,n_origin)) %>% 
    # top_n(n=10,wt = p_origin) %>%  #I keep the countries for which
    mutate(origin_region = factor(origin_region,levels=regions_order),
           destination = factor(destination, levels=selection_countries),
           destination = fct_relabel(destination,~str_replace(.x,' ','\n')),
    ) 
  
  selection_df %>% 
    ggplot(aes(origin_region,p_origin, fill=origin_region))+
    geom_col()+
    scale_y_continuous(labels = percent,limits = c(0,0.45))+
    coord_flip()+
    labs(x='regions as exporters',
         y='proportion of papers exported to the focal country',  fill='region')+
    tidytext::scale_x_reordered() +
    scale_fill_manual(values = regions_colors)+
    facet_grid2(destination~., scales = 'free',strip = strip)
}

countries_as_origins <- function(df,countries_flows,selection_countries){
  
  selection_df <- countries_flows %>% 
    filter(origin %in% selection_countries) %>% #I keep the selection of destinations
    group_by(origin,destination_region) %>% #for each country of destinations
    summarise(p_destination = weighted.mean(p_destination,n_destination)) %>% 
    mutate(destination_region = factor(destination_region,levels=regions_order),
           origin = factor(origin, levels=selection_countries),
           origin = fct_relabel(origin,~str_replace(.x,' ','\n')))
  
  selection_df %>% 
    ggplot(aes(destination_region,p_destination, fill=destination_region))+
    geom_col()+
    scale_y_continuous(labels = percent,limits = c(0,0.45))+
    coord_flip()+
    labs(x='regions as importers',
         y= 'proportion of the imported papers from the focal country', fill='region')+
    scale_fill_manual(values = regions_colors)+
    tidytext::scale_x_reordered() +
    facet_grid2(origin~., scales = 'free',strip = strip)
}

plt_1a <- countries_as_destinations(df,countries_flows,selection_countries)
plt_1b <- countries_as_origins(df,countries_flows,selection_countries) + 
  theme(axis.text.y = element_blank())

ggarrange(plt_1a, plt_1b,legend = 'none',widths = c(1.3,1))

ggsave('results/figures/4_origin_destination_dependencies_agg.png', width = 12, height = 7)


