library(wesanderson)
library(ISOcodes)
library(ggpubr)
library(scales)
# library(ggridges)
# library(ggrepel)
# library(gt)
# library(gtExtras)
source("notebooks/dk_figures/utils.R")

countries <- read_csv('data/countries_list.txt')$country_code

df_dim <- read_delim('data/country_portfolios_dimensions.csv',delim = ';') %>%
  # df_dim <- read_delim('data/country_portfolios_dimensions.csv',delim = ';') %>%
  rename(N=p, 
         field = for_group_id) %>% 
  filter(type !='all', country_code %in% countries) %>% #!country_code %in% c('ZZALL','Unknown')
  mutate(type = case_match(type,
                           'national' ~ 'National',
                           'international' ~ 'National',
                           # 'international' ~ 'International',
                           'migrant' ~ 'Imports',
                           'abroad' ~ 'Exports'),
         country_code = countrycode(country_code, origin = 'iso2c', destination = 'country.name'),
         continent = countrycode(country_code, origin = 'country.name', destination = 'continent')
  )  %>% 
  group_by(type,continent, country_code, field, pub_year) %>% 
  summarise(N = sum(N))

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

map_prop_1 <-
  world %>% 
  left_join(gdata_p,by = join_by(country_code)) %>% 
  pivot_longer(cols = Exports:National, names_to = 'groups', values_to = 'p') %>%
  mutate(groups = factor(groups, levels =c('National','Imports', 'Exports'))) %>% 
  filter(region!='Antarctica') %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = p)) +
  facet_wrap(.~groups)+
  scale_fill_viridis_c()+
  plain_theme+
  labs(fill='Proportion of\npapers')+
  theme(# legend.position = 'none',
    legend.key.width = unit(1.5, "cm"),
    text = element_text(size = 18),
    plot.margin = margin(-20,-15,-20,-15,unit = 'pt'))

gdata_p3 <-  df_dim %>%
  filter_countries(tr = 5000) %>% 
  filter(type%in%c('Exports','Imports')) %>%
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
         ratio = Exports/Imports)
#National = percent_rank(National)*100,
# Emigrant = percent_rank(Emigrant)*100,
# Immigrant = percent_rank(Immigrant)*100)

map_prop_2 <-
  world %>% 
  left_join(gdata_p3,by = join_by(country_code)) %>% 
  filter(region!='Antarctica') %>% 
  ggplot(aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) +
  geom_polygon(aes(fill = ratio)) +
  scale_fill_viridis_c(values = rescale(c(0,.75,1.25,2,round(max(gdata_p3$ratio),digits = 2))),
                       breaks = c(0,.75,1.25,2,round(max(gdata_p3$ratio),digits = 2)-.01))+
  plain_theme+
  labs(fill='Exports\nImports\nratio')+
  theme(legend.position = 'right',
        text = element_text(size = 16),
        legend.key.height = unit(1.2, "cm"),
        legend.text = element_text(size=12),
        # legend.key.width = unit(1.5, "cm"),
        plot.margin = margin(-10,-10,-10,-10,unit = 'pt'))


ggarrange(map_prop_1,map_prop_2,ncol = 1,labels = 'auto')#,heights = c(1.1,1)

ggsave('results/figures/1_migrations_maps.png',bg = 'white', width = 12, height = 8, dpi=300)
