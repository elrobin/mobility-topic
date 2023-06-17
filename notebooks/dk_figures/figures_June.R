library(wesanderson)
library(ISOcodes)
library(ggpubr)
source("notebooks/dk_figures/utils.R")

df_dim <- read_delim('data/country_portfolios_dimensions.csv',delim = ';') %>%
  # df_dim <- read_delim('data/country_portfolios_dimensions.csv',delim = ';') %>%
  rename(N=p, 
         field = for_group_id) %>% 
  filter(type !='all') %>% #!country_code %in% c('ZZALL','Unknown')
  mutate(type = case_match(type,
                           'national' ~ 'National',
                           'international' ~ 'International',
                           'migrant' ~ 'Immigrant',
                           'abroad' ~ 'Emigrant'),
         country_code = countrycode(country_code, origin = 'iso2c', destination = 'country.name')
  )  


theme_plot <- function(g){
  g+ labs(x= 'Total number of papers per country', color = '')+
  theme_minimal()+
  theme(legend.position = 'bottom')
}

## we cannot use RCA and KL distance, because RCA is not a probability dist, and KL is a distance betweeen probs.

plot_distance_N(df = df_dim,t = 5000,f_empty_topics = FALSE,comparison_group = 'diff',
                distance_formula = 'cosine', plotly = FALSE,regions = FALSE,val = 'rca') %>% 
  theme_plot()+
  geom_smooth()+
  labs(title = "", y= 'cosine similarity')

ggsave('results/figures/N_distance_cosine_diff_rca.png')


### Word maps

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

pal <- wes_palette("Zissou1", 100, type = "continuous")


world <- map_data("world") %>% 
  mutate(country_code = countryname(region,destination = 'country.name.en')) 

#1.  proportion of authors by group
gdata_p <-  df_dim %>%
  group_by(country_code,type) %>% 
  summarise(N = sum(N)) %>% 
  group_by(country_code) %>% 
  mutate(p = N/sum(N)) %>% 
  select(-N) %>% 
  filter(type=='National') %>% 
  # pivot_wider(names_from = type, values_from = p,values_fill = 0) %>% 
  mutate(country_code = countryname(country_code,destination = 'country.name.en'))

  prop_nationals <- world %>% 
    left_join(gdata_p,by = join_by(country_code)) %>% 
    mutate(type='National') %>% 
    filter(region!='Antarctica') %>% 
    ggplot(aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) +
    geom_polygon(aes(fill = p)) +
    facet_wrap(.~type)+
    scale_fill_gradientn(colours = pal, labels=scales::percent,limits = c(0,1)) +
    plain_theme+
    labs(fill='Proportion of authors')+
    theme( legend.position = 'none',
           text = element_text(size = 18),
      plot.margin = margin(0,-20,0,-20,unit = 'pt'))

  gdata_p2 <-  df_dim %>%
    filter(type!='National') %>% 
    mutate(type = case_match(type,
                             'Emigrant' ~'Migrant',
                             'Immigrant' ~'Migrant',
                             'International' ~ 'International')) %>% 
    group_by(country_code,type) %>% 
    summarise(N = sum(N)) %>% 
    group_by(country_code) %>% 
    mutate(p = N/sum(N)) %>%
    # ungroup() %>% 
    # complete(country_code,type,fill=list(N=0,p=0))
    select(-N) %>% 
    pivot_wider(names_from = type, values_from = p,values_fill = 0) %>%
    mutate(country_code = countryname(country_code,destination = 'country.name.en'))
  
  prop_rest1 <- world %>% 
    left_join(gdata_p2,by = join_by(country_code)) %>% 
    pivot_longer(cols = International:Migrant, names_to = 'groups', values_to = 'p') %>%
    filter(region!='Antarctica') %>% 
    ggplot(aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) +
    geom_polygon(aes(fill = p)) +
    facet_wrap(.~groups)+
    scale_fill_gradientn(colours = pal, labels=scales::percent, limits = c(0,1)) +
    plain_theme+
    labs(fill='Proportion of authors')+
    theme(text = element_text(size = 18),
          legend.position = 'none',
          plot.margin = margin(-10,-10,-10,-10,unit = 'pt'))
  
  gdata_p3 <-  df_dim %>%
    filter(type%in%c('Emigrant','Immigrant')) %>% 
    group_by(country_code,type) %>% 
    summarise(N = sum(N)) %>% 
    group_by(country_code) %>% 
    mutate(p = N/sum(N)) %>%
    # ungroup() %>% 
    # complete(country_code,type,fill=list(N=0,p=0))
    select(-N) %>% 
    pivot_wider(names_from = type, values_from = p,values_fill = 0) %>%
    mutate(country_code = countryname(country_code,destination = 'country.name.en'))
  
  prop_rest2 <- world %>% 
    left_join(gdata_p3,by = join_by(country_code)) %>% 
    pivot_longer(cols = Emigrant:Immigrant, names_to = 'groups', values_to = 'p') %>%
    filter(region!='Antarctica') %>% 
    ggplot(aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) +
    geom_polygon(aes(fill = p)) +
    facet_wrap(.~groups)+
    scale_fill_gradientn(colours = pal, labels=scales::percent, limits = c(0,1)) +
    plain_theme+
    labs(fill='Proportion of\nauthors')+
    theme(text = element_text(size = 18),
          legend.key.width = unit(1.5, "cm"),
          plot.margin = margin(-10,-10,-10,-10,unit = 'pt'))
  
  leg <- get_legend(prop_rest2)
  
ggarrange(prop_nationals,prop_rest1,prop_rest2,ncol = 1,labels = 'auto',heights = c(1.1,1,1),
          common.legend = TRUE,legend = 'bottom',legend.grob = leg)
  
ggsave('results/figures/maps1_prop.png',bg = 'white')
  
#2 diff, rca

gdata2 <- build_dataset(df_dim,t=1000,f_empty_topics=TRUE, comparison_group='diff',
                       distance_formula='cosine',val = 'rca') %>% 
  pivot_wider(names_from = type, values_from = distance) %>% 
  mutate(country_code = countryname(country_code,destination = 'country.name.en'))

world %>% 
  left_join(gdata2,by = join_by(country_code)) %>% 
  pivot_longer(cols = International:Emigrant, names_to = 'groups', values_to = 'distance') %>% 
  filter(region!='Antarctica') %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = distance)) +
  facet_wrap(.~groups)+
  scale_fill_gradientn(colours = pal,limits = c(.25,1)) +
  plain_theme+
  labs(fill='Cosine similarity')

ggsave('results/figures/maps1_rca.png')

# 3. pairs, rca
gdata4 <- build_dataset(df_dim,t=1000,f_empty_topics=TRUE, comparison_group='pairs',
                        distance_formula='cosine',val = 'rca') %>% 
  pivot_wider(names_from = type, values_from = distance) %>% 
  mutate(country_code = countryname(country_code,destination = 'country.name.en'))


world %>%   
  left_join(gdata4,by = join_by(country_code)) %>% 
  pivot_longer(cols = International_Immigrant:Emigrant_International, names_to = 'groups', values_to = 'distance') %>% 
  filter(region!='Antarctica') %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = distance)) +
  facet_wrap(.~groups)+
  # scale_fill_gradientn(colours = pal,limits = c(.5,1)) +
  scale_fill_gradientn(colours = pal)+
  # scale_fill_gradientn(colours = pal,limits = c(.75,1)) + 
  plain_theme+
  labs(fill='Cosine similarity')

ggsave('results/figures/maps2_rca.png')


