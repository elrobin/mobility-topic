library(wesanderson)
library(ISOcodes)
library(ggpubr)
library(scales)
library(ggridges)
library(ggrepel)
library(gt)
library(gtExtras)
source("notebooks/dk_figures/utils.R")


### I add the International collaboration to national papers
df_dim <- read_delim('data/country_portfolios_dimensions.csv',delim = ';') %>%
  # df_dim <- read_delim('data/country_portfolios_dimensions.csv',delim = ';') %>%
  rename(N=p, 
         field = for_group_id) %>% 
  filter(type !='all') %>% #!country_code %in% c('ZZALL','Unknown')
  mutate(type = case_match(type,
                           'national' ~ 'National',
                           'international' ~ 'National',
                           # 'international' ~ 'International',
                           'migrant' ~ 'Immigrant',
                           'abroad' ~ 'Emigrant'),
         country_code = countrycode(country_code, origin = 'iso2c', destination = 'country.name')
  )  %>% 
  group_by(type, country_code, field, pub_year) %>% 
  summarise(N = sum(N))


theme_plot <- function(g){
  g+ labs(x= 'Total number of papers per country', color = '')+
  theme_minimal()+
  theme(legend.position = 'bottom',
        text = element_text(size=18))
}

## we cannot use RCA and KL distance, because RCA is not a probability dist, and KL is a distance betweeen probs.

plot_distance_N(df = df_dim,t = 5000,f_empty_topics = FALSE,comparison_group = 'pairs',
                distance_formula = 'cosine', plotly = FALSE,regions = FALSE,val = 'rca') %>% 
  theme_plot()+
  geom_smooth()+
  labs(title = "", y= 'cosine similarity')+
  scale_color_discrete(type =  wes_palette("Zissou1", 5)[c(2,4)])+
  theme(plot.margin = margin(0,10,0,0))

ggsave('results/figures/N_distance_cosine_pairs_rca.png', width = 12, height = 6,dpi = 300)


# Word maps ---------------------------------------------------------------

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
  filter_countries(tr = 5000) %>% 
  group_by(country_code,type) %>% 
  summarise(N = sum(N)) %>% 
  group_by(country_code) %>% 
  mutate(p = N/sum(N)) %>% 
  select(-N) %>% 
  # filter(type=='National') %>% 
  pivot_wider(names_from = type, values_from = p,values_fill = 0) %>%
  mutate(country_code = countryname(country_code,destination = 'country.name.en'))

map_prop_1 <- world %>% 
  left_join(gdata_p,by = join_by(country_code)) %>% 
  pivot_longer(cols = Emigrant:National, names_to = 'groups', values_to = 'p') %>%
  mutate(groups = factor(groups, levels =c('National','Immigrant', 'Emigrant'))) %>% 
  filter(region!='Antarctica') %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = p)) +
  facet_wrap(.~groups)+
  scale_fill_gradientn(colours = pal)+
  # scale_fill_gradientn(colours = pal, labels=scales::percent,limits = c(0,1)) +
  plain_theme+
  labs(fill='Proportion of\nauthors')+
  # labs(fill='Proportion of authors')+
  theme(# legend.position = 'none',
    legend.key.width = unit(1.5, "cm"),
         text = element_text(size = 18),
         plot.margin = margin(-20,-15,-20,-15,unit = 'pt'))

  gdata_p3 <-  df_dim %>%
    filter_countries(tr = 5000) %>% 
    filter(type%in%c('Emigrant','Immigrant')) %>%
    group_by(country_code,type) %>%
    summarise(N = sum(N)) %>%
    group_by(country_code) %>%
    mutate(p = N/sum(N)) %>%
    # ungroup() %>% 
    # complete(country_code,type,fill=list(N=0,p=0))
    select(-N) %>% 
    pivot_wider(names_from = type, values_from = p,values_fill = 0) %>%
    ungroup() %>% 
    mutate(country_code = countryname(country_code,destination = 'country.name.en'),
           #National = percent_rank(National)*100,
           Emigrant = percent_rank(Emigrant)*100,
           Immigrant = percent_rank(Immigrant)*100)
  
  map_prop_2 <- world %>% 
    left_join(gdata_p3,by = join_by(country_code)) %>% 
    pivot_longer(cols = Emigrant:Immigrant, names_to = 'groups', values_to = 'p') %>%
    filter(region!='Antarctica') %>% 
    ggplot(aes(x = long, y = lat, group = group)) + 
    coord_fixed(1.3) +
    geom_polygon(aes(fill = p)) +
    facet_wrap(.~groups)+
    scale_fill_gradientn(colours = pal) +
    # scale_fill_gradientn(colours = pal, labels=scales::percent, limits = c(0.25,.75)) +
    plain_theme+
    labs(fill='Ranking')+
    theme(text = element_text(size = 18),
          legend.key.width = unit(1.5, "cm"),
          plot.margin = margin(-10,-10,-10,-10,unit = 'pt'))
  
  leg <- get_legend(map_prop_2)
  
ggarrange(map_prop_1,map_prop_2,ncol = 1,labels = 'auto')#,heights = c(1.1,1)
          #common.legend = TRUE,legend = 'bottom',legend.grob = leg)
  
ggsave('results/figures/maps1_prop.png',bg = 'white')
  
#2 diff, rca

gdata2 <- build_dataset(df_dim,t=5000,f_empty_topics=TRUE, comparison_group='diff',
                       distance_formula='cosine',val = 'rca') %>% 
  pivot_wider(names_from = type, values_from = distance) %>% 
  mutate(country_code = countryname(country_code,destination = 'country.name.en'))

world %>% 
  left_join(gdata2,by = join_by(country_code)) %>% 
  pivot_longer(cols = National:Emigrant, names_to = 'groups', values_to = 'distance') %>% 
  mutate(groups = factor(groups, levels =c('National','Immigrant', 'Emigrant'))) %>% 
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
gdata4 <- build_dataset(df_dim,t=5000,f_empty_topics=TRUE, comparison_group='pairs',
                        distance_formula='cosine',val = 'rca') %>%
  filter(type!='Emigrant_Immigrant') %>% 
  pivot_wider(names_from = type, values_from = distance) %>% 
  mutate(country_code = countryname(country_code,destination = 'country.name.en'))
#,
         # Immigrant_National = percent_rank(Immigrant_National)*100,
         # Emigrant_National = percent_rank(Emigrant_National)*100)


world %>%   
  left_join(gdata4,by = join_by(country_code)) %>% 
  pivot_longer(cols = Immigrant_National:Emigrant_National, names_to = 'groups', values_to = 'distance') %>% 
  mutate(groups = factor(groups, levels=c('Immigrant_National','Emigrant_National','Emigrant_Immigrant'))) %>% 
  filter(region!='Antarctica') %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = distance)) +
  facet_wrap(.~groups)+
  # scale_fill_gradientn(colours = pal,limits = c(.5,1)) +
  scale_fill_gradientn(colours = pal)+
  # scale_fill_gradientn(colours = pal,limits = c(0,1)) +
  plain_theme+
  theme(text=element_text(size = 24))+
  labs(fill='Cosine similarity\nranking')

ggsave('results/figures/maps2_rca2.png', width = 16, height = 6, dpi = 300)


# Ratios
gdata5 <- build_dataset(df_dim,t=5000,f_empty_topics=TRUE, comparison_group='pairs',
                        distance_formula='cosine',val = 'rca') %>%
  filter(type!='Emigrant_Immigrant') %>% 
  pivot_wider(names_from = type, values_from = distance) %>% 
  mutate(country_code = countryname(country_code,destination = 'country.name.en'),
         ratio = Immigrant_National/Emigrant_National)
         # ratio = percent_rank(ratio)*100)


world %>%   
  left_join(gdata5,by = join_by(country_code)) %>% 
  # pivot_longer(cols = Immigrant_National:Emigrant_National, names_to = 'groups', values_to = 'distance') %>% 
  # mutate(groups = factor(groups, levels=c('Immigrant_National','Emigrant_National','Emigrant_Immigrant'))) %>% 
  filter(region!='Antarctica') %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = ratio)) +
  # facet_wrap(.~groups)+
  # scale_fill_gradientn(colours = pal,limits = c(.5,1)) +
  # scale_fill_gradientn(colours = pal)+
  scale_fill_gradientn(colours = pal,limits = c(0.5,1.5)) +
  plain_theme+
  theme(text=element_text(size = 12))+
  labs(fill='Immigrants/Emigrants\ncosine ratio ranking')

ggsave('results/figures/maps3_ratio1.png', width = 12, height = 6, dpi = 300)


data_for_nico_1 <- build_dataset(df_dim,t=1000,f_empty_topics=TRUE, comparison_group='pairs',
                                         distance_formula='cosine',val = 'rca') %>%
  filter(type!='Emigrant_Immigrant') %>% 
  pivot_wider(names_from = type, values_from = distance) %>% 
  mutate(cosine_ratio = Immigrant_National/Emigrant_National)

data_for_nico_2 <- df_dim %>%
  filter_countries(tr = 1000) %>% 
  group_by(country_code,type) %>% 
  summarise(N = sum(N)) %>% 
  group_by(country_code) %>% 
  mutate(p = N/sum(N)) %>% 
  select(-N) %>% 
  pivot_wider(names_from = type, values_from = p,values_fill = 0,names_prefix = 'prop_') %>%
  mutate(prop_ratio = prop_Immigrant/prop_Emigrant)

left_join(data_for_nico_1,data_for_nico_2) %>% 
  write_csv('results/emigrant_immigrant_ratio.csv')
### PROP


# prop vs similarity ------------------------------------------------------

df_prop <- df_dim %>%
  group_by(country_code,type) %>% 
  summarise(N = sum(N)) %>% 
  group_by(country_code) %>% 
  mutate(prop = N/sum(N)) %>% 
  select(-N) %>% 
  # pivot_wider(names_from = type, values_from = p,values_fill = 0) %>% 
  mutate(country_code = countryname(country_code,destination = 'country.name.en'))


df_cosine <- build_dataset(df_dim,t=5000,f_empty_topics=TRUE, comparison_group='diff',
                        distance_formula='cosine',val = 'rca') 

gdata <- df_cosine %>% 
  rename(cosine=distance) %>% 
  left_join(df_prop,by = join_by(country_code, type)) %>% 
  group_by(type) %>% 
  mutate(mean_cos = weighted.mean(cosine,w=country_N),
         mean_prop = weighted.mean(prop,w=country_N),
         region = countryname(country_code,destination='region'))

# library(crosstalk)
# shared_df <- SharedData$new(gdata, key = ~country_code)


# plot <- shared_df %>% 
plot <- gdata %>% 
  ggplot(aes(prop, cosine,color=region, size=country_N))+
  geom_hline(aes(yintercept = mean_cos))+
  geom_vline(aes(xintercept = mean_prop))+
  geom_point()+
  facet_grid(type~.)+
  theme_minimal()+
  labs(x= 'Proportion',y='Cosine', color='', size= 'Country # publications')+
  scale_x_continuous(labels = percent)+
  theme(legend.position = 'bottom')

plot

ggsave('results/figures/prop_vs_cosine.png')

# density -----------------------------------------------------------------

continent_palette <- wesanderson::wes_palettes$Darjeeling1[c(1:3,5)]
names(continent_palette) <- c('Africa', 'Americas','Asia','Europe')

annotate_data1 <-  gdata %>% 
  mutate(continent = countryname(country_code,destination='continent')) %>% 
  group_by(type) %>% 
  top_n(5,wt = prop) %>%  
  arrange(-country_N)

density_1 <-
  gdata %>% 
  # filter(type=='National') %>% 
  ggplot(aes(x=prop, y=type, fill = factor(stat(quantile))))+
  stat_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
    geom_label_repel(data=annotate_data1,
                     aes(label=country_code,
                         x=prop, y = type,
                         color =continent),
                     # nudge_x = .05,
                     # nudge_y = .15,
                     inherit.aes = FALSE)+
    scale_x_continuous(labels = percent)+
  scale_fill_viridis_d(name = "Quartiles",alpha=0.75)+
  scale_color_manual(values=continent_palette)+
  theme_minimal()+
    guides(fill= 'none')+
  labs(x = 'Proportion', y='')+
  theme(legend.position = 'bottom',
        text = element_text(size=20))


# ggplot('results/figures/prop_dist.png')
gdata_pairs <-  build_dataset(df_dim,t=5000,f_empty_topics=TRUE, comparison_group='pairs',
                        distance_formula='cosine',val = 'rca')  %>% 
  filter(type!='Emigrant_Immigrant') %>% 
  rename(cosine=distance) %>% 
  group_by(type) %>% 
  mutate(mean_cos = weighted.mean(cosine,w=country_N),
         region = countryname(country_code,destination='region'),
         type = str_replace(type,'_','\n'),
         type = factor(type))

annotate_data2 <-  gdata_pairs %>% 
  mutate(continent = countryname(country_code,destination='continent')) %>% 
  group_by(type) %>% 
  top_n(5,wt = -cosine) %>%  
  arrange(-country_N)

density_2 <- gdata_pairs %>%
# gdata %>% 
  # filter(type=='National') %>% 
  ggplot(aes(x=cosine, y=type, fill = factor(stat(quantile))))+
  stat_density_ridges(
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0),
    point_shape = '|', point_size = 3, point_alpha = 1, alpha = 0.7,
    geom = "density_ridges_gradient", calc_ecdf = TRUE,
    quantiles = 4, quantile_lines = TRUE
  ) +
  scale_fill_viridis_d(name = "Quartiles",alpha=0.75)+
  scale_color_manual(values=continent_palette)+
  theme_minimal()+
  ggrepel::geom_label_repel(data=annotate_data2, aes(label=country_code,
                                    x=cosine, y = type, color=continent),
                           # nudge_x = .05,
                           # nudge_y = .15,
            inherit.aes = FALSE)+
  # scale_x_continuous(labels = percent)+
  labs(x = 'Cosine', y='')+
  guides(fill= 'none')+
  theme(legend.position = 'bottom',
        text = element_text(size=20),
        axis.text.y = element_text(size=12))


ggarrange(density_1,density_2,ncol = 1, labels = 'auto', common.legend = TRUE,
          legend = 'bottom')

ggsave('results/figures/densities.png', width = 12, height = 7, dpi = 300)

 gdata_pairs %>% 
  arrange(-cosine) %>% 
  group_by(type) %>% 
  top_n(.,10,wt = cosine) %>% 
  select(country=country_code,type,cosine, total_publications=country_N) %>%
  group_by(type) %>%
  gt() %>% 
  fmt_number(columns = cosine,decimals = 2) %>% 
  gt::gtsave('results/tables/most_similar_pairs.docx')

gdata_pairs %>% 
  arrange(cosine) %>% 
  group_by(type) %>% 
  top_n(.,-10,wt = cosine) %>% 
  select(country=country_code,type,cosine, total_publications=country_N) %>%
  group_by(type) %>%
  gt() %>% 
  row_group_order(groups = levels(gdata_pairs$type)) %>% 
  fmt_number(columns = cosine,decimals = 2) %>% 
  gt::gtsave('results/tables/least_similar_pairs.docx')
