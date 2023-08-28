library(tidyverse)
library(countrycode)
library(readxl)
library(plotly)
library(ggrepel)
library(scales)
library(ggpubr)
library(ggalluvial)

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


data <- df %>% 
  filter(!is.na(origin_region), !is.na(destination_region)) %>% 
  group_by(for_division,origin=origin_region, destination=destination_region) %>% 
  summarise(N=sum(N))

disciplines_prop <- 
  data %>% 
  group_by(for_division,destination) %>%
  summarise(total_division = sum(N)) %>% 
  # ungroup() %>% 
  group_by(destination) %>%
  mutate(p_division = total_division/sum(total_division)) # %>% 
# filter(p_division>=min_share_division)

immigrants_prop <- 
  data %>% 
  group_by(origin,destination) %>% 
  summarise(total_origin = sum(N)) %>% 
  # ungroup() %>%    
  group_by(destination) %>%
  mutate(p_origin = total_origin/sum(total_origin)) #%>% 
# filter(p_origin>=min_share)

emigrants_prop <- 
  data %>% 
  group_by(origin,destination) %>% 
  summarise(total_destination = sum(N)) %>% 
  # ungroup() %>%    
  group_by(origin) %>%
  mutate(p_destination = total_destination/sum(total_destination)) #%>% 
# filter(p_origin>=min_share)

gdata <- data %>% 
  # filter(destination==dest) %>%
  # filter(origin%in%immigrants_prop$origin,destination==dest) %>%
  inner_join(disciplines_prop,by = join_by(for_division,destination)) %>% 
  inner_join(immigrants_prop,by = join_by(origin,destination)) %>% 
  inner_join(emigrants_prop,by = join_by(origin,destination)) %>% 
  ungroup() %>% 
  mutate(p_division_origin = N/total_division,
         ratio = p_division_origin/p_origin,
         # for_division = str_replace_all(for_division,' ','\n'),
         # origin = str_replace_all(origin,' ','\n'),
         destination = str_replace_all(destination,' ','\n'),
         # origin = paste0(origin, ' (',round(p_origin*100,digits = 1),'%)'),
         # destination = paste0(destination, ' (',round(p_destination*100,digits = 1),'%)'),
         origin = fct_reorder(origin, p_origin),
         destination = fct_reorder(destination, p_destination),
         # for_division = paste0(for_division, ' (',round(p_division*100,digits = 1),'%)'),
         for_division = fct_reorder(for_division, p_division),
  )

ggdata <- gdata %>%
  filter(N > 1000, ratio > 1) %>%
  arrange(-ratio)


# origin, destination

### ALLUVIAL PLOT

region_origin <- data %>%
  group_by(origin) %>%
  summarise(N = sum(N)) %>%
  ungroup() %>%
  mutate(origin_p = N / sum(N)) %>% 
  select(-N)

region_destination <- data %>%
  group_by(destination) %>%
  summarise(N = sum(N)) %>%
  ungroup() %>%
  mutate(destination_p = N / sum(N)) %>% 
  select(-N)

origin_destination_data <- data %>%
  group_by(origin, destination) %>%
  summarise(N = sum(N)) %>%
  ungroup() %>%
  left_join(region_origin,by = join_by(origin)) %>% 
  left_join(region_destination,by = join_by(destination)) %>% 
  mutate(
    origin_lab = paste0(origin, ' (', percent(origin_p),')'),
    destination_lab = paste0(destination, ' (', percent(destination_p),')'),
    # origin = factor(origin, regions_order, label=origin_lab),
    # destination = factor(destination, levels = regions_order, label=destination_lab)
    )
  
origin_labs <- 
  origin_destination_data %>% arrange(-origin_p) %>% 
  select(origin, origin_lab) %>% distinct() %>% pull(origin, origin_lab)

destination_labs <-
  origin_destination_data %>% 
  mutate(destination = factor(destination, levels = origin_labs)) %>% 
  arrange(destination) %>% 
  select(destination, destination_lab) %>% distinct() %>% pull(destination,destination_lab)

origin_destination_data <- origin_destination_data %>% 
  mutate(origin = factor(origin, levels =origin_labs,labels=names(origin_labs)),
         destination = factor(destination, levels =destination_labs,labels=names(destination_labs))
         )

origin_destination_data %>%
  ggplot(aes(axis1 = origin, axis2 = destination, y = N)) +
  geom_alluvium(aes(fill = origin)) +
  geom_stratum(fill='gray80') +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum))) +
  theme_void()+
  theme(legend.position = 'none')
ggsave('results/figures/origin_destination.png', dpi = 300, width = 12, height = 7)

# origin, discipline, rca

div_dist_global <- data %>% 
  group_by(for_division) %>% 
  summarise(N = sum(N)) %>% 
  ungroup() %>% 
  mutate(p_div_global = N/sum(N)) %>% select(-N)

origin_RCA <- data %>% 
  group_by(for_division,origin) %>% 
  summarise(N = sum(N)) %>% 
  left_join(div_dist_global, by=join_by(for_division)) %>% 
  group_by(origin) %>% 
  mutate(#origin = str_replace_all(origin,' ','\n'),
         p_div_region = N/sum(N),
         rca = p_div_region/p_div_global)

  # ggplot(aes(rca,for_division, fill=origin))+
  # geom_vline(xintercept = 1)+
  # geom_col(position = position_dodge())+
  # facet_grid(.~origin, scales = 'free')+
  # theme(legend.position = 'none')+
  # labs(y= '', title = 'Origin')


destination_RCA <- data %>% 
  group_by(for_division,destination) %>% 
  summarise(N = sum(N)) %>% 
  left_join(div_dist_global, by=join_by(for_division)) %>% 
  group_by(destination) %>% 
  mutate(#destination = str_replace_all(destination,' ','\n'),
         p_div_region = N/sum(N),
         rca = p_div_region/p_div_global) 




origin_RCA %>%
  select(-N,-p_div_region,rca_origin = rca, region=origin) %>% 
  left_join(destination_RCA %>% 
              select(-N,-p_div_region,
                     rca_destination=rca,
                     region=destination),
            by = join_by(for_division, region,p_div_global)) %>% 
  ggplot(aes(rca_origin,rca_destination, size=p_div_global,color=region, label=for_division))+
  # geom_point()+
  geom_text_repel(max.overlaps = 20)+
  geom_hline(yintercept = 1)+
  geom_vline(xintercept = 1)+
  theme(legend.position = 'bottom')

ggsave('results/figures/rca_field_origin_destination.png', width = 12, height = 10)

  # ggplot(aes(rca,for_division, fill=destination))+
  # geom_vline(xintercept = 1)+
  # geom_col(position = position_dodge())+
  # facet_grid(.~destination, scales = 'free')+
  # theme(legend.position = 'none',
  #       axis.text.y = element_blank())+
  # labs(y= '', title = 'Destination')


# ggarrange(origin_RCA,destination_RCA,widths = c(1.5,1))
# destination, discipline, ratio


# origin, destination, discipline and ratio
ggplot(ggdata, aes(for_division,ratio, color=origin, size=N))+
  geom_point()+
  facet_grid(.~destination)+
  coord_flip()

ggsave('results/figures/origin_destination_relative_ratio.png', width = 14, height = 7)

