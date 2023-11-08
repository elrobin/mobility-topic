library(tidyverse)
library(countrycode)
library(readxl)
# library(ggrepel)
library(scales)
library(ggpubr)
# library(ggalluvial)
# library(gt)

df <- read_excel('data/discipline_country_migrations.xlsx',sheet = 'Data')
df2 <- read_excel('data/discipline_country_migrations.xlsx',sheet = 'Data')

countries <- read_csv('data/countries_list.txt')$country_code

# I aggregate to the division level
df <- df %>% 
  filter(country_code_origin %in% countries,
         country_code %in% countries) %>% 
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
  theme(legend.position = 'none')

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
  theme(legend.position = 'none')

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
  theme(legend.position = 'none')

# ggsave('results/figures/2_region_region_matrix.png', dpi = 300, width = 7, height = 4)

ggarrange(plt_1,ggarrange(plt_1b,plt_1c,nrow = 1, labels = c('B','C')),nrow = 2,labels = c('A',''))
ggsave('results/figures/2_region_region_matrix.png', dpi = 300, width = 16, height = 12)



# Disciplines -------------------------------------------------------------

## proportions, normalization
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


origin_field_flow_norm <- df %>% 
  group_by(origin_region,for_division) %>% 
  summarise(total_flow = sum(N)) %>% 
  ungroup() %>% 
  mutate(prop_flow = total_flow/sum(total_flow)) %>% 
  group_by(origin_region) %>% 
  mutate(prop_flow = prop_flow/sum(prop_flow)) %>% 
  ungroup() %>% 
  mutate(region = factor(str_replace(origin_region, '&', '&\n'),levels=regions_order),
         for_division = factor(for_division, fields_order),
         type='origin, region normalized') %>% 
  filter(!is.na(origin_region), 
         !is.na(for_division)) %>% 
  select(-origin_region)

origin_field_flow_norm2 <- df %>% 
  group_by(origin_region,for_division) %>% 
  summarise(total_flow = sum(N)) %>% 
  ungroup() %>% 
  mutate(prop_flow = total_flow/sum(total_flow)) %>% 
  group_by(for_division) %>% 
  mutate(prop_flow = prop_flow/sum(prop_flow)) %>% 
  ungroup() %>% 
  mutate(region = factor(str_replace(origin_region, '&', '&\n'),levels=regions_order),
         for_division = factor(for_division, fields_order),
         type='origin, field normalized') %>% 
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

destination_field_flow_norm <- df %>% 
  group_by(destination_region,for_division) %>% 
  summarise(total_flow = sum(N)) %>% 
  ungroup() %>% 
  mutate(prop_flow = total_flow/sum(total_flow)) %>% 
  group_by(destination_region) %>% 
  mutate(prop_flow = prop_flow/sum(prop_flow)) %>% 
  ungroup() %>% 
  mutate(region = factor(str_replace(destination_region, '&', '&\n'),levels=regions_order),
         for_division = factor(for_division, fields_order),
         type='destination, region normalized') %>% 
  filter(!is.na(destination_region), 
         !is.na(for_division)) %>% 
  select(-destination_region)

destination_field_flow_norm2 <- df %>% 
  group_by(destination_region,for_division) %>% 
  summarise(total_flow = sum(N)) %>% 
  ungroup() %>% 
  mutate(prop_flow = total_flow/sum(total_flow)) %>% 
  group_by(for_division) %>% 
  mutate(prop_flow = prop_flow/sum(prop_flow)) %>% 
  ungroup() %>% 
  mutate(region = factor(str_replace(destination_region, '&', '&\n'),levels=regions_order),
         for_division = factor(for_division, fields_order),
         type='destination, field normalized') %>% 
  filter(!is.na(destination_region), 
         !is.na(for_division)) %>% 
  select(-destination_region)



plt_2a <- bind_rows(origin_field_flow,
                   destination_field_flow) %>% 
  ggplot(aes(region,for_division, fill=prop_flow,label = scales::percent(prop_flow,accuracy=0.1))) + 
  geom_tile() +
  geom_text()+
  labs(x='Region', y='Field', fill='')+
  scale_x_discrete()+
  scale_fill_viridis_c(labels=scales::percent)+
  theme(legend.position = 'bottom')+
  facet_wrap(.~type, ncol=1)

plt_2b <- bind_rows(origin_field_flow_norm,
                   destination_field_flow_norm) %>% 
  ggplot(aes(region,for_division, fill=prop_flow,label = scales::percent(prop_flow,accuracy=0.1))) + 
  geom_tile() +
  geom_text()+
  labs(x='Region', y='', fill='')+
  scale_x_discrete()+
  scale_fill_viridis_c(labels=scales::percent)+
  facet_wrap(.~type, ncol=1)+
  theme(legend.position = 'bottom',
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank())

plt_2c <- bind_rows(origin_field_flow_norm2,
                   destination_field_flow_norm2) %>% 
  ggplot(aes(region,for_division, fill=prop_flow,label = scales::percent(prop_flow,accuracy=0.1))) + 
  geom_tile() +
  geom_text()+
  labs(x='Region', y='', fill='')+
  scale_x_discrete()+
  scale_fill_viridis_c(labels=scales::percent)+
  facet_wrap(.~type, ncol=1)+
  theme(legend.position = 'bottom',
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

plt_2 <- ggarrange(plt_2a,plt_2b,plt_2c,ncol = 3,widths = c(1.75,1,1))

plt_2
ggsave('results/figures/3b_region_field_matrices.png', dpi = 300, width =16, height = 10)


# relative over-underrepresentation ---------------------------------------------------------

origin_field_flow <-
  df %>% 
  mutate(total_flow = sum(N)) %>% 
  group_by(origin_region) %>% 
  mutate(total_flow_origin = sum(N),
         p_flow_origin = total_flow_origin/total_flow) %>%
  group_by(for_division) %>% 
  mutate(total_flow_div = sum(N),
         p_flow_div = total_flow_div/total_flow) %>%
  group_by(origin_region,for_division) %>% 
  mutate(total_flow_orig_div = sum(N),
         p_flow_orig_div = total_flow_orig_div/total_flow,
         expected_p = p_flow_div*p_flow_origin,
         ratio = p_flow_orig_div/expected_p,
         diff = ratio-1) %>% 
  select(origin_region,for_division,p_flow_origin,p_flow_div,
         p_flow_orig_div,ratio,diff) %>% 
  distinct() %>% 
  mutate(region = factor(str_replace(origin_region, '&', '&\n'),levels=regions_order),
         for_division = factor(for_division, fields_order),
         type='origin') %>% 
  filter(!is.na(origin_region), 
         !is.na(for_division)) %>% 
  select(-origin_region)



destination_field_flow <-
  df %>% 
  mutate(total_flow = sum(N)) %>% 
  group_by(destination_region) %>% 
  mutate(total_flow_dest = sum(N),
         p_flow_dest = total_flow_dest/total_flow) %>%
  group_by(for_division) %>% 
  mutate(total_flow_div = sum(N),
         p_flow_div = total_flow_div/total_flow) %>%
  group_by(destination_region,for_division) %>% 
  mutate(total_flow_dest_div = sum(N),
         p_flow_dest_div = total_flow_dest_div/total_flow,
         expected_p = p_flow_div*p_flow_dest,
         ratio = p_flow_dest_div/expected_p,
         diff = ratio-1) %>% 
  select(destination_region,for_division,p_flow_dest,p_flow_div,
         p_flow_dest_div,ratio,diff) %>% 
  distinct() %>% 
  mutate(region = factor(str_replace(destination_region, '&', '&\n'),levels=regions_order),
         for_division = factor(for_division, fields_order),
         type='destination') %>% 
  filter(!is.na(destination_region), 
         !is.na(for_division)) %>% 
  select(-destination_region)

bind_rows(origin_field_flow,destination_field_flow) %>% 
ggplot(aes(region,for_division, fill=diff,label = scales::percent(diff,accuracy=0.1))) + 
  geom_tile() +
  geom_text()+
  facet_wrap(.~type, ncol=2)+
  labs(x='Region', y='', fill='')+
  scale_x_discrete()+
  scale_fill_fermenter(type = 'div',palette = 'Spectral')
#BrBG, PiYG, PRGn, PuOr, RdBu, RdGy, RdYlBu, RdYlGn, Spectral

ggsave('results/figures/3_region_field_matrices_rel_diff.png', dpi = 300, width =16, height = 10)
