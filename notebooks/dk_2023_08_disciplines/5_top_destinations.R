library(wesanderson)
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

min_countries_selection <- df %>% group_by(origin) %>% 
  summarise(N = sum(N)) %>% 
  arrange(-N) %>% 
  filter(N>1000) %>% pull(origin)

# df <- df %>% 
#   filter(origin %in% min_countries_selection, 
#          destination %in% min_countries_selection)
# 

world <- map_data("world") %>% 
  mutate(country = countryname(region,destination = 'country.name.en'),
         region  = countryname(region,destination = 'region')) 

# map

plain_theme <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5),
  legend.position = 'bottom'
)

## regional level
gdata_p1 <- df %>% 
  # group_by(origin,origin_region,destination,destination_region) %>% 
  group_by(origin,destination_region) %>% 
  # filter(origin %in% min_countries_selection,
  #        destination %in% min_countries_selection) %>% 
  summarise(N = sum(N)) %>% 
  group_by(origin) %>% 
  filter(N == max(N)) %>% 
  arrange(N)

## countyr level
gdata_p2 <- df %>% 
  # group_by(origin,origin_region,destination,destination_region) %>% 
  group_by(origin,destination) %>% 
  # filter(origin %in% min_countries_selection,
  #        destination %in% min_countries_selection) %>% 
  summarise(N = sum(N)) %>% 
  group_by(origin) %>% 
  filter(N == max(N)) %>% 
  ungroup() %>% 
  mutate(destination =  fct_lump_n(destination,n=10),
         origin = case_when(destination=='Other'~NA_character_, # I will remove them so they are greyed on the map
                            TRUE ~origin))
## mapping

country_map <- world %>% 
  left_join(gdata_p2,by = join_by(country==origin)) %>% 
  filter(region!='Antarctica') %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = destination)) +
  plain_theme+
  labs(fill='Main\ndestination\ncountry')+
  theme(legend.position = 'bottom',
    # legend.key.width = unit(1.5, "cm"),
    # text = element_text(size = 18),
    plot.margin = margin(1,-10,1,-10)
    # plot.background = element_rect(color = "black"),
    )

regional_map <- world %>% 
  left_join(gdata_p1,by = join_by(country==origin)) %>% 
  filter(region!='Antarctica', !is.na(destination_region)) %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = destination_region)) +
  plain_theme+
  labs(fill='Main\ndestination\nregion')+
  theme(# legend.position = 'none',
    # legend.key.width = unit(1.5, "cm"),
    # text = element_text(size = 18),
    plot.margin = margin(10,-10,10,-10)
    # plot.background = element_rect(color = "black")
    )

ggarrange(country_map, regional_map, labels = 'AUTO', ncol = 2)

ggsave('results/figures/7_main_destination_map.png', width = 16, height = 10)


# main origins -----------------------------------------------------------------


## regional level
gdata_p1 <- df %>% 
  group_by(origin_region,destination) %>% 
  summarise(N = sum(N)) %>% 
  group_by(destination) %>% 
  filter(N == max(N)) %>% 
  arrange(N)

## country level
gdata_p2 <- df %>% 
  group_by(origin,destination) %>% 
  summarise(N = sum(N)) %>% 
  group_by(destination) %>% 
  filter(N == max(N)) %>% 
  ungroup() %>% 
  mutate(origin =  fct_lump_n(origin,n=10),
         destination = case_when(origin=='Other'~NA_character_, # I will remove them so they are greyed on the map
                            TRUE ~destination))
## mapping

country_map <- world %>% 
  left_join(gdata_p2,by = join_by(country==destination)) %>% 
  filter(region!='Antarctica') %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = origin)) +
  plain_theme+
  labs(fill='Main\norigin\ncountry')+
  theme(legend.position = 'bottom',
        # legend.key.width = unit(1.5, "cm"),
        # text = element_text(size = 18),
        plot.margin = margin(1,-10,1,-10)
        # plot.background = element_rect(color = "black"),
  )

regional_map <- world %>% 
  left_join(gdata_p1,by = join_by(country==destination)) %>% 
  filter(region!='Antarctica', !is.na(origin_region)) %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = origin_region)) +
  plain_theme+
  labs(fill='Main\norigin\nregion')+
  theme(# legend.position = 'none',
    # legend.key.width = unit(1.5, "cm"),
    # text = element_text(size = 18),
    plot.margin = margin(10,-10,10,-10)
    # plot.background = element_rect(color = "black")
  )

ggarrange(country_map, regional_map, labels = 'AUTO', ncol = 2)

ggsave('results/figures/8_main_origin_map.png', width = 16, height = 10)


