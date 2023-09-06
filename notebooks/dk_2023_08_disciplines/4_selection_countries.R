library(tidyverse)
library(countrycode)
library(readxl)
library(plotly)
library(ggrepel)
library(scales)
library(ggpubr)

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


df %>% 
  group_by(destination) %>% 
  summarise(N=sum(N)) %>% 
  arrange(-N) %>% 
  mutate(destination = fct_reorder(destination, -N)) %>% 
  top_n(20) %>% 
  ggplot(aes(destination,N))+
  geom_col()

selection_countries <- c('United States','United Kingdom','China','Germany','Brazil')

# keep only the top 30 origins of each destination
#top 30 countries of origin per destination
origin_dest_selection <- df %>% 
  filter(destination %in% selection_countries) %>% 
  group_by(origin,destination) %>% 
  summarise(total_origin_dest = sum(N)) %>% 
  group_by(destination) %>% 
  top_n(n=10,wt=total_origin_dest) %>% select(origin,destination,total_origin_dest)

df_selection <- df %>%
  complete(origin,destination,for_division) %>% 
  group_by(origin,destination) %>% 
  mutate(N = coalesce(N,0)) %>% 
  inner_join(origin_dest_selection,by = join_by(origin, destination))

fields <- df_selection$for_division %>% unique()
names(fields) <- paste0(LETTERS[1:22],':',fields)

df_selection <- df_selection %>%
  group_by(for_division,destination) %>% 
  mutate(N_division = sum(N)) %>%
  group_by(origin,destination) %>% 
  mutate(N_origin = sum(N)) %>% 
  ungroup() %>% 
  mutate(p_origin = N/N_origin,
         p_div = N/N_division,
         fields = factor(for_division,levels=fields,label=names(fields)),
         for_division = factor(for_division, label=LETTERS[1:22]),
         origin = tidytext::reorder_within(origin, N_origin, within = destination)) 

plt1 <- df_selection %>%
  ggplot(aes(for_division,origin, fill=fields, alpha=p_div)) + 
  geom_tile() +
  facet_wrap(destination~., scales = 'free',nrow = 1)+
  tidytext::scale_y_reordered() +
  theme(legend.position = 'bottom')

plt2 <- df_selection %>% 
  ggplot(aes(for_division,origin, fill=fields, alpha=p_origin)) + 
  geom_tile() +
  tidytext::scale_y_reordered() +
  facet_wrap(destination~., scales = 'free',nrow = 1)+
  theme(legend.position = 'bottom')


ggarrange(plt1,plt2, common.legend = TRUE, ncol = 1, labels = 'AUTO',legend = 'bottom')

ggsave('results/figures/selection_countries.png', width = 16,height = 6)

## alternative: origin and destination selection

selection_countries <- c('United States','United Kingdom','China','Germany','Brazil', 'South Africa')


# keep only the top 30 origins of each destination
#top 30 countries of origin per destination
origin_dest_selection <- df %>% 
  filter(origin %in% selection_countries, destination %in% selection_countries) %>% 
  group_by(origin,destination) %>% 
  summarise(total_origin_dest = sum(N))


df_selection <- df %>%
  complete(origin,destination,for_division) %>% 
  group_by(origin,destination) %>% 
  mutate(N = coalesce(N,0)) %>% 
  inner_join(origin_dest_selection,by = join_by(origin, destination))

fields <- df_selection$for_division %>% unique()
names(fields) <- paste0(LETTERS[1:22],':',fields)

df_selection <- df_selection %>%
  group_by(for_division,destination) %>% 
  mutate(N_division = sum(N)) %>%
  group_by(origin,destination) %>% 
  mutate(N_origin = sum(N)) %>% 
  ungroup() %>% 
  mutate(p_origin = N/N_origin,
         p_div = N/N_division,
         fields = factor(for_division,levels=fields,label=names(fields)),
         for_division = factor(for_division, label=LETTERS[1:22]),
         origin = tidytext::reorder_within(origin, N_origin, within = destination)) 

plt1 <- df_selection %>%
  ggplot(aes(for_division,origin, fill=fields, alpha=p_div)) + 
  geom_tile() +
  facet_wrap(destination~., scales = 'free',nrow = 1)+
  tidytext::scale_y_reordered() +
  theme(legend.position = 'bottom')

plt2 <- df_selection %>% 
  ggplot(aes(for_division,origin, fill=fields, alpha=p_origin)) + 
  geom_tile() +
  tidytext::scale_y_reordered() +
  facet_wrap(destination~., scales = 'free',nrow = 1)+
  theme(legend.position = 'bottom')


ggarrange(plt1,plt2, common.legend = TRUE, ncol = 1, labels = 'AUTO',legend = 'bottom')

ggsave('results/figures/selection_countries2.png', width = 18,height = 5, dpi = 300)


  