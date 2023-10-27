library(tidyverse)
library(countrycode)
library(readxl)
# library(ggrepel)
library(scales)
library(ggpubr)
# library(ggalluvial)
# library(gt)

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
                                        destination = 'continent'),
         origin_region = countrycode(country_code_origin, 
                                     origin = 'iso2c',
                                     destination = 'region'),
         destination_region = countrycode(country_code, 
                                          origin = 'iso2c',
                                          destination = 'region')) %>% 
  filter(!is.na(origin),!is.na(destination)) %>% 
  select(-c(country_code_origin,country_code))


regions_order <- df %>%
  group_by(origin_region) %>% 
  summarise(N = sum(N)) %>% 
  arrange(-N) %>%
  filter(!is.na(origin_region)) %>% 
  pull(origin_region) %>% str_replace(., '&', '&\n')

fields_order <- df %>%
  group_by(for_division) %>% 
  summarise(N = sum(N)) %>% 
  arrange(-N) %>%
  filter(!is.na(for_division)) %>% 
  pull(for_division)

origin_destination_flow <-
  df %>% 
  group_by(origin_region,destination_region) %>% 
  summarise(total_flow = sum(N)) %>% 
  ungroup() %>% 
  mutate(prop_flow = total_flow/sum(total_flow)) %>% 
  ungroup() %>% 
  mutate(origin_region = factor(str_replace(origin_region, '&', '&\n'),levels=regions_order),
         destination_region = factor(str_replace(destination_region, '&', '&\n'), levels=regions_order)) %>% 
  filter(!is.na(origin_region), 
       !is.na(destination_region))

plt_1 <-origin_destination_flow %>% 
  ggplot(aes(origin_region,destination_region,
             fill=prop_flow,label = scales::percent(prop_flow,accuracy=0.1))) + 
  geom_tile() +
  geom_text()+
  labs(x='Origin', y='Destination', fill='proportion of mobility')+
  scale_x_discrete()+
  scale_fill_viridis_c(labels=scales::percent)+
  theme(legend.position = 'bottom')

#vertical normalization
plt_1b <- origin_destination_flow %>% 
  group_by(origin_region) %>% 
  mutate(prop_flow = prop_flow/sum(prop_flow)) %>% 
  ggplot(aes(origin_region,destination_region,
             fill=prop_flow,label = scales::percent(prop_flow,accuracy=0.1))) + 
  geom_tile() +
  geom_text()+
  labs(x='Origin', y='Destination', fill='proportion of mobility')+
  scale_x_discrete()+
  scale_fill_viridis_c(labels=scales::percent)+
  theme(legend.position = 'bottom')

#horizontal normalization
plt_1c <- origin_destination_flow %>% 
  group_by(destination_region) %>% 
  mutate(prop_flow = prop_flow/sum(prop_flow)) %>% 
  ggplot(aes(origin_region,destination_region,
             fill=prop_flow,label = scales::percent(prop_flow,accuracy=0.1))) + 
  geom_tile() +
  geom_text()+
  labs(x='Origin', y='Destination', fill='proportion of mobility')+
  scale_x_discrete()+
  scale_fill_viridis_c(labels=scales::percent)+
  theme(legend.position = 'bottom')


origin_field_flow <- df %>% 
  group_by(origin_region,for_division) %>% 
  summarise(total_flow = sum(N)) %>% 
  ungroup() %>% 
  mutate(prop_flow = total_flow/sum(total_flow)) %>% 
  ungroup() %>% 
  mutate(region = factor(str_replace(origin_region, '&', '&\n'),levels=regions_order),
         for_division = factor(for_division, fields_order),
         type='origin') %>% 
  filter(!is.na(origin_region), 
         !is.na(for_division)) %>% 
  select(-origin_region)
  
destination_field_flow <- df %>% 
  group_by(destination_region,for_division) %>% 
  summarise(total_flow = sum(N)) %>% 
  ungroup() %>% 
  mutate(prop_flow = total_flow/sum(total_flow)) %>% 
  ungroup() %>% 
  mutate(region = factor(str_replace(destination_region, '&', '&\n'),levels=regions_order),
         for_division = factor(for_division, fields_order),
         type='destination') %>% 
  filter(!is.na(destination_region), 
         !is.na(for_division)) %>% 
  select(-destination_region)

plt_2 <- bind_rows(origin_field_flow,destination_field_flow) %>% 
  ggplot(aes(region,for_division, fill=prop_flow,label = scales::percent(prop_flow,accuracy=0.1))) + 
  geom_tile() +
  geom_text()+
  labs(x='Region', y='Field', fill='proportion of mobility')+
  scale_x_discrete()+
  scale_fill_viridis_c(labels=scales::percent)+
  theme(legend.position = 'bottom')+
  facet_wrap(.~type, ncol=1)



plt_1
ggsave('results/figures/2_region_region_matrix.png', dpi = 300, width = 7, height = 4)

ggarrange(plt_1b,plt_1c,labels = 'AUTO')
ggsave('results/figures/2b_region_region_matrix.png', dpi = 300, width = 16, height = 6)

plt_2
ggsave('results/figures/3_region_field_matrices.png', dpi = 300, width =10, height = 10)

# ggsave('results/figures/alluvial.png', dpi = 300, width = 10, height = 12)