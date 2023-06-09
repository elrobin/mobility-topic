library(wesanderson)
library(ISOcodes)
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

plot_distance_N(df = df_dim,t = 5000,f_empty_topics = TRUE,comparison_group = 'pairs',
                distance_formula = 'cosine', plotly = FALSE,regions = FALSE,val = 'p')  %>% 
  theme_plot()

ggsave('results/figures/N_distance_cosine_pairs.png')

plot_distance_N(df = df_dim,t = 5000,f_empty_topics = TRUE,comparison_group = 'pairs',
                distance_formula = 'cosine', plotly = FALSE,regions = FALSE,val = 'rca')  %>% 
  theme_plot()

ggsave('results/figures/N_distance_cosine_pairs_rca.png')

plot_distance_N(df_dim,t = 5000,f_empty_topics = TRUE,comparison_group = 'pairs', 
                distance_formula = 'kullback-leibler', plotly = FALSE,regions = FALSE,
                val = 'p')  %>% 
  theme_plot()

ggsave('results/figures/N_distance_KL_pairs.png')

# plot_distance_N(df_dim,t = 500,f_empty_topics = TRUE,comparison_group = 'pairs', 
#                 distance_formula = 'kullback-leibler', plotly = FALSE,regions = FALSE,
#                 val = 'rca')  %>% 
#   theme_plot()
# 
# ggsave('results/figures/N_distance_KL_pairs_rca.png')
## cannot compute KL

plot_distance_N(df_dim,t = 5000,f_empty_topics = TRUE,comparison_group = 'diff',
                distance_formula = 'cosine', plotly = FALSE,regions = FALSE,val = 'p') %>% 
  theme_plot()

ggsave('results/figures/N_distance_cosine_diff.png')

plot_distance_N(df_dim,t = 5000,f_empty_topics = TRUE,comparison_group = 'diff',
                distance_formula = 'cosine', plotly = FALSE,regions = FALSE,val = 'rca') %>% 
  theme_plot()

ggsave('results/figures/N_distance_cosine_diff_rca.png')


plot_distance_N(df_dim,t = 5000,f_empty_topics = TRUE,comparison_group = 'diff',
                distance_formula = 'kullback-leibler', plotly = FALSE,regions = FALSE,val = 'p') %>% 
  theme_plot()

ggsave('results/figures/N_distance_KL_diff.png')

# plot_distance_N(df_dim,t = 5000,f_empty_topics = TRUE,comparison_group = 'diff',
#                 distance_formula = 'kullback-leibler', plotly = FALSE,regions = FALSE,val = 'rca') %>% 
#   theme_plot()
# 
# ggsave('results/figures/N_distance_KL_diff_rca.png')


## by regions

plot_distance_N(df_dim,t = 5000,f_empty_topics = TRUE,comparison_group = 'pairs',
                distance_formula = 'cosine', plotly = FALSE,regions = TRUE)  %>% 
  theme_plot()

ggsave('results/figures/N_distance_cosine_pairs_region.png')

plot_distance_N(df_dim,t = 5000,f_empty_topics = TRUE,comparison_group = 'pairs', 
                distance_formula = 'kullback-leibler', plotly = FALSE,regions = TRUE)  %>% 
  theme_plot()

ggsave('results/figures/N_distance_KL_pairs_region.png')



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

#1. Diff, prop
gdata <- build_dataset(df_dim,t=1000,f_empty_topics=TRUE, comparison_group='diff',
                       distance_formula='cosine',val = 'p') %>% 
  pivot_wider(names_from = type, values_from = distance) %>% 
  mutate(country_code = countryname(country_code,destination = 'country.name.en'))

# world %>% filter(is.na(International)) %>% 
#   pull(region) %>% 
#   unique()


  world %>% 
    left_join(gdata,by = join_by(country_code)) %>% 
    pivot_longer(cols = International:Emigrant, names_to = 'groups', values_to = 'distance') %>% 
    filter(region!='Antarctica') %>% 
    ggplot(aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) +
    geom_polygon(aes(fill = distance)) +
    facet_wrap(.~groups)+
    scale_fill_gradientn(colours = pal,limits = c(.75,1)) + 
    plain_theme

ggsave('results/figures/maps1.png')
  
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
  plain_theme

ggsave('results/figures/maps1_rca.png')


#3. pairs, p

gdata3 <- build_dataset(df_dim,t=1000,f_empty_topics=TRUE, comparison_group='pairs',
                       distance_formula='cosine',val = 'p') %>% 
  pivot_wider(names_from = type, values_from = distance) %>% 
  mutate(country_code = countryname(country_code,destination = 'country.name.en'))


world %>%   
  left_join(gdata3,by = join_by(country_code)) %>% 
  pivot_longer(cols = International_Immigrant:Emigrant_International, names_to = 'groups', values_to = 'distance') %>% 
  filter(region!='Antarctica') %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = distance)) +
  facet_wrap(.~groups)+
  scale_fill_gradientn(colours = pal,limits = c(.75,1)) + 
  plain_theme

ggsave('results/figures/maps2.png')


# 4. pairs, rca
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
  plain_theme

ggsave('results/figures/maps2_rca.png')

### Distance needs to be above/below expected! The topic size's makes all groups similar. 
## I can renormalize the topics to show the RCA instead of the number of papers.
### or the RCA of each group-country, but normalize by global topic size.


