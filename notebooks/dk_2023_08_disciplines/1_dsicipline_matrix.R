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


us_df <- df %>% 
  filter(destination=='United States')


#I keep origin countries with high N

#double normalization


# scatterplots ------------------------------------------------------------

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

ggplotly(
  us_normalized %>% 
    # mutate(for_division = str_replace_all(for_division, ' ', '\n')) %>% 
    filter(origin %in% keep_countries) %>% 
    ggplot(aes(P_origin,P_for,size=N, color=continent_origin, label=for_division,
               text= paste('% origin: ',percent(P_origin),
                           '<br>% field: ', percent(P_for),
                           '<br>N: ',N,
                           '<br>field: ', for_division,
                           '<br>origin: ',origin)))+
    # geom_point()+
    geom_text()+
    theme(legend.position = 'bottom'),
  tooltip = 'text'
)


ggplotly(
  us_normalized %>%
    filter(N>1000) %>% 
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


# heatmaps ----------------------------------------------------------------

heatmap_country <- function(data,
                            dest = 'United States',
                            min_share_origin=.01,min_share_division=0){
  
  disciplines_prop <- 
    data %>% 
    filter(destination==dest) %>% 
    group_by(for_division) %>% 
    summarise(total_division = sum(N)) %>% 
    ungroup() %>% 
    mutate(p_division = total_division/sum(total_division)) %>% 
    filter(p_division>=min_share_division)
  
  immigrants_prop <- 
    data %>% 
    filter(destination==dest) %>% 
    group_by(origin) %>% 
    summarise(total_origin = sum(N)) %>% 
    ungroup() %>% 
    mutate(p_origin = total_origin/sum(total_origin)) %>% 
    filter(p_origin>=min_share_origin)
  
  gdata <- data %>% 
    filter(destination==dest) %>%
    # filter(origin%in%immigrants_prop$origin,destination==dest) %>%
    inner_join(disciplines_prop,by = join_by(for_division)) %>% 
    inner_join(immigrants_prop,by = join_by(origin)) %>% 
    ungroup() %>% 
    mutate(p_division_origin = N/total_division,
           ratio = p_division_origin/p_origin,
           for_division = str_replace_all(for_division,' ','\n'),
           origin = paste0(origin, ' (',round(p_origin*100,digits = 1),'%)'),
           origin = fct_reorder(origin, p_origin),
           for_division = paste0(for_division, ' (',round(p_division*100,digits = 1),'%)'),
           for_division = fct_reorder(for_division, p_division),
    ) 
  
  gdata %>% 
    ggplot(aes(for_division,origin,fill=ratio, label=round(ratio,digits = 2)))+
    geom_tile()+
    geom_text()+
    scale_fill_viridis_c()+
    theme(legend.position = 'none',
          plot.margin = margin(-1,-1,-1,-1),
          axis.text.x = element_text(size=8))+
    labs(x='',y='',title=dest)
}

# heatmap_country(df)

# selection of countries --------------------------------------------------
df %>% 
  group_by(destination) %>% 
  summarise(N=sum(N)) %>% 
  arrange(-N) %>% 
  mutate(destination = fct_reorder(destination, -N)) %>% 
  top_n(20) %>% 
  ggplot(aes(destination,N))+
  geom_col()

selection_countries <- c('United States','United Kingdom','China','Germany','Australia','Canada')

figures <- list()
for (country in selection_countries) {
  figures[[country]] <- heatmap_country(df,country,min_share_division = 0,
                                        min_share_origin = 0.01)
}

ggarrange(plotlist = figures)

# regions

