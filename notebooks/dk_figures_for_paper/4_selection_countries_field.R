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
  filter(N>5000) %>% pull(origin)

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
  group_by(origin,origin_region,destination,destination_region) %>% #bi-variate sums
  mutate(n_origin_destination = sum(N)) %>% 
  group_by(origin,origin_region,for_division) %>%
  mutate(n_origin_field = sum(N)) %>% 
  group_by(destination,destination_region,for_division) %>%
  mutate(n_destination_field = sum(N)) %>%
  group_by(origin) %>% #totals
  mutate(n_origin = sum(N)) %>% 
  group_by(destination) %>% 
  mutate(n_destination = sum(N)) %>% 
  # group_by(for_division) %>% 
  # mutate(n_field = sum(N)) %>% 
  mutate(p_destination = n_origin_destination/n_destination,
         p_origin =  n_origin_destination/n_origin,
         p_origin_field = N/n_origin_field,
         p_destination_field= N/n_destination_field,
         p_field_destination = N/n_origin_destination) %>% 
  ungroup()

countries_as_destinations_field <- function(df,countries_flows,selection_countries){
  
  selection_df <- countries_flows %>% 
    select(destination,origin,p_destination) %>% distinct() %>% # I keep only countries and prop
    filter(destination%in% selection_countries) %>% #I keep the selection of destinations
    group_by(destination) %>% #for each country of destinations
    # top_n(n=5,wt = p_destination) %>% View()#I keep the countries for which 
    top_n(n=5,wt = p_destination)  #I keep the countries for which
  
  fields <- countries_flows$for_division %>% unique()
  names(fields) <- paste0(LETTERS[1:22],':',fields)
  
  countries_flows %>% 
    inner_join(selection_df,by = join_by(origin, destination, p_destination)) %>% 
    mutate(fields = factor(for_division,levels=fields,label=names(fields)),
           for_division = factor(for_division, label=LETTERS[1:22]),
           origin = tidytext::reorder_within(origin, p_destination_field, within = destination)) %>% 
    ggplot(aes(for_division,origin, fill=fields,alpha=p_destination_field))+
    geom_tile() +
    facet_wrap(destination~., scales = 'free',nrow = 2)+
    tidytext::scale_y_reordered() +
    labs(x='')+
    theme(legend.position = 'bottom')
}

countries_as_origin_field <- function(df,countries_flows,selection_countries){
  
  selection_df <- countries_flows %>% 
    select(destination,origin,p_origin) %>% distinct() %>% # I keep only countries and prop
    filter(origin%in% selection_countries) %>% #I keep the selection of destinations
    group_by(origin) %>% #for each country of destinations
    # top_n(n=5,wt = p_destination) %>% View()#I keep the countries for which 
    top_n(n=5,wt = p_origin)  #I keep the countries for which
  
  fields <- countries_flows$for_division %>% unique()
  names(fields) <- paste0(LETTERS[1:22],':',fields)
  
  countries_flows %>% 
    inner_join(selection_df,by = join_by(origin, destination, p_origin)) %>% 
    mutate(fields = factor(for_division,levels=fields,label=names(fields)),
           for_division = factor(for_division, label=LETTERS[1:22]),
           destination = tidytext::reorder_within(destination, p_origin_field, within = origin)) %>% 
    ggplot(aes(for_division,destination, fill=fields,alpha=p_destination_field))+
    geom_tile() +
    facet_wrap(origin~., scales = 'free',nrow = 2)+
    tidytext::scale_y_reordered() +
    labs(x='')+
    theme(legend.position = 'bottom')
}

plt_1a <- countries_as_destinations_field(df,countries_flows,selection_countries)+
  theme(plot.background = element_rect(color = "black"))
plt_1b <- countries_as_origin_field(df,countries_flows,selection_countries)+
  theme(plot.background = element_rect(color = "black"))

ggarrange(plt_1a, plt_1b, common.legend = TRUE,labels = 'AUTO', legend = 'bottom',ncol = 1)

ggsave('results/figures/5_origin_destination_field.png', width = 16, height = 10)

countries_as_destinations_field_2 <- function(df,countries_flows,selection_countries){
  
  selection_df <- countries_flows %>% 
    select(destination,origin,p_destination) %>% distinct() %>% # I keep only countries and prop
    filter(destination%in% selection_countries) %>% #I keep the selection of destinations
    group_by(destination) %>% #for each country of destinations
    # top_n(n=5,wt = p_destination) %>% View()#I keep the countries for which 
    top_n(n=5,wt = p_destination)  #I keep the countries for which
  
  fields <- countries_flows$for_division %>% unique()
  names(fields) <- paste0(LETTERS[1:22],':',fields)
  
  countries_flows %>% 
    inner_join(selection_df,by = join_by(origin, destination, p_destination)) %>% 
    mutate(fields = factor(for_division,levels=fields,label=names(fields)),
           for_division = factor(for_division, label=LETTERS[1:22]),
           origin = tidytext::reorder_within(origin, p_destination_field, within = destination)) %>% 
    ggplot(aes(for_division,origin, fill=fields,alpha=p_field_destination))+
    geom_tile() +
    facet_wrap(destination~., scales = 'free',nrow = 2)+
    tidytext::scale_y_reordered() +
    labs(x='')+
    theme(legend.position = 'bottom')
}

countries_as_origin_field_2 <- function(df,countries_flows,selection_countries){
  
  selection_df <- countries_flows %>% 
    select(destination,origin,p_origin) %>% distinct() %>% # I keep only countries and prop
    filter(origin%in% selection_countries) %>% #I keep the selection of destinations
    group_by(origin) %>% #for each country of destinations
    # top_n(n=5,wt = p_destination) %>% View()#I keep the countries for which 
    top_n(n=5,wt = p_origin)  #I keep the countries for which
  
  fields <- countries_flows$for_division %>% unique()
  names(fields) <- paste0(LETTERS[1:22],':',fields)
  
  countries_flows %>% 
    inner_join(selection_df,by = join_by(origin, destination, p_origin)) %>% 
    mutate(fields = factor(for_division,levels=fields,label=names(fields)),
           for_division = factor(for_division, label=LETTERS[1:22]),
           destination = tidytext::reorder_within(destination, p_origin_field, within = origin)) %>% 
    ggplot(aes(for_division,destination, fill=fields,alpha=p_field_destination))+
    geom_tile() +
    facet_wrap(origin~., scales = 'free',nrow = 2)+
    tidytext::scale_y_reordered() +
    labs(x='')+
    theme(legend.position = 'bottom')
}

plt_1a <- countries_as_destinations_field_2(df,countries_flows,selection_countries)+
  theme(plot.background = element_rect(color = "black"))
plt_1b <- countries_as_origin_field_2(df,countries_flows,selection_countries)+
  theme(plot.background = element_rect(color = "black"))

ggarrange(plt_1a, plt_1b, common.legend = TRUE,labels = 'AUTO', legend = 'bottom',ncol = 1)

ggsave('results/figures/6_origin_destination_field_2.png', width = 16, height = 10)


