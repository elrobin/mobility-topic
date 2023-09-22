library(tidyverse)
library(countrycode)
library(readxl)
library(plotly)
library(ggrepel)
library(scales)
library(ggpubr)
library(ggalluvial)
library(gt)

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
         #ratio = p_division_origin/p_origin,
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



# origin, destination

### ALLUVIAL PLOT

alluvial_plots <- function(data,axis_1='origin', axis_2='destination'){
  
  axis_1 <- sym(axis_1)
  axis_2 <- sym(axis_2)
  
  axis_1_df <- data %>% 
    group_by(!!axis_1) %>%
    summarise(N = sum(N)) %>%
    ungroup() %>%
    mutate(p_1 = N / sum(N)) %>% 
    select(-N) 
  
  axis_2_df <- data %>% 
    group_by(!!axis_2) %>%
    summarise(N = sum(N)) %>%
    ungroup() %>%
    mutate(p_2 = N / sum(N)) %>% 
    select(-N)
  
  axis_1_2_df <- data %>%
    group_by(!!axis_1, !!axis_2) %>%
    summarise(N = sum(N)) %>%
    ungroup() %>%
    left_join(axis_1_df,by = join_by(!!axis_1)) %>% 
    left_join(axis_2_df,by = join_by(!!axis_2)) %>% 
    mutate(
      ax1_lab = paste0(!!axis_1, ' (', percent(p_1,accuracy=0.01),')'),
      ax2_lab = paste0(!!axis_2, ' (', percent(p_2,accuracy=0.01),')'),
      ax1_lab = case_when(p_1<0.015 ~ 'Other',
                          TRUE ~ax1_lab),
      ax2_lab = case_when(p_2<0.015 ~ 'Other',
                          TRUE ~ax2_lab))
  
  ax1_labs <- axis_1_2_df %>% arrange(-p_1) %>% 
    select(!!axis_1, ax1_lab) %>% distinct() %>% pull(!!axis_1, ax1_lab)
  
  if (axis_1=='origin' & axis_2=='destination') {
    ax2_labs <- axis_1_2_df %>% 
      mutate(axis_2 = factor(!!axis_2, levels = ax1_labs)) %>% 
      arrange(axis_2) %>% 
      select(axis_2, ax2_lab) %>% distinct() %>% pull(axis_2, ax2_lab)
  }else{
    ax2_labs <- axis_1_2_df %>% arrange(-p_2) %>% 
      select(!!axis_2, ax2_lab) %>% distinct() %>% pull(!!axis_2, ax2_lab)
  }
  axis_1_2_df <- axis_1_2_df %>% 
    mutate(axis1 = factor(!!axis_1, levels =ax1_labs,labels=names(ax1_labs)),
           axis2 = factor(!!axis_2, levels =ax2_labs,labels=names(ax2_labs))
    )
  label_pos <- axis_1_2_df$N %>% sum()*1.1
  
  if (axis_1 == 'for_division') {
    ax1_label = 'field'
    ax2_label = paste0(axis_2)
  }else{
  if (axis_2 == 'for_division') {
    ax1_label = paste0(axis_1)
    ax2_label = 'field'
  }else{
    ax1_label = paste0(axis_1)
    ax2_label = paste0(axis_2)
  }
  }
  axis_1_2_df %>%
    ggplot(aes(axis1 = axis1, axis2 = axis2, y = N)) +
    geom_alluvium(aes(fill = axis1)) +
    geom_stratum(fill='gray80') +
    geom_text(stat = "stratum",
              aes(label = after_stat(stratum))) +
    theme_void()+
    geom_text(inherit.aes = FALSE,
              data = data.frame(x = c(1, 2), y = c(label_pos, label_pos), 
                                label = c(ax1_label,ax2_label)),
              size=6,
              aes(label = label, x = x, y = y))+
    theme(legend.position = 'none')
  
}

all1 <- alluvial_plots(data,'origin','destination')
all2 <- alluvial_plots(data,axis_1 = 'origin',axis_2 = 'for_division')
all3 <- alluvial_plots(data,axis_1 = 'for_division',axis_2 = 'destination')

ggarrange(all1,ggarrange(all2,all3),ncol = 1)

ggsave('results/figures/alluvial.png', dpi = 300, width = 10, height = 12)


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

rca_data <- origin_RCA %>%
  select(for_division,p_div_global,N_origin=N,-p_div_region,rca_origin = rca, region=origin) %>% 
  left_join(destination_RCA %>% 
              select(for_division,p_div_global,N_destination=N,-p_div_region,
                     rca_destination=rca,
                     region=destination),
            by = join_by(for_division, region,p_div_global))

for_div <- rca_data$for_division %>% unique()
names(for_div) <- 1:22
rca_data %>% 
  filter(N_origin>1000,N_destination>1000) %>% 
  mutate(for_division = factor(for_division,levels=for_div,labels = names(for_div))) %>% 
  ggplot(aes(rca_origin,rca_destination, size=p_div_global,color=region, label=for_division))+
  # geom_point()+
  geom_text_repel(max.overlaps = 100)+
  geom_hline(yintercept = 1)+
  # geom_smooth(method = 'lm', se=FALSE)+
  geom_smooth(method = 'lm', se=FALSE, aes(weight=p_div_global))+
  geom_vline(xintercept = 1)+
  theme_minimal()+
  theme(legend.position = 'bottom',
        text = element_text(size=18))

#probar grafico barras

ggsave('results/figures/rca_field_origin_destination.png', width = 14, 
       height = 10, bg = 'white')

tibble(ref=names(for_div),field=for_div) %>% gt() %>% 
  gtsave('results/figures/ref_table.docx')

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

exports <- data %>% 
  group_by(for_division,region=origin) %>% 
  summarise(n_export = sum(N))

imports <- data %>% 
  group_by(for_division,region=destination) %>% 
  summarise(n_import = sum(N)) 

net_flow <- left_join(imports,exports,by = join_by(for_division, region)) %>% 
  mutate(net_import_flow = n_import-n_export,
         rel_net_flow = net_import_flow/n_import)

rca_data %>% 
  left_join(net_flow,by = join_by(for_division, region)) %>% 
  filter(n_import>1000, n_export>1000) %>%
  mutate(origin_destination_rca_ratio = rca_origin/rca_destination) %>% 
  ggplot(aes(origin_destination_rca_ratio,rel_net_flow,color =region,
             label=for_division, size=p_div_global))+
  # geom_point()+
  geom_text_repel(max.overlaps = 500,min.segment.length = unit(0, 'lines'))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 1)
# facet_wrap(.~flow)

rca_data %>% 
  left_join(net_flow,by = join_by(for_division, region)) %>% 
  # filter(n_import>1000, n_export>1000) %>% 
  mutate(origin_destination_rca_ratio = rca_origin/rca_destination) %>% 
  ggplot(aes(origin_destination_rca_ratio,rel_net_flow,color =region,
             label=for_division, size=n_import))+
  geom_smooth()+
  geom_point()+
  # geom_text_repel(max.overlaps = 500,min.segment.length = unit(0, 'lines'))+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 1)
# facet_wrap(.~flow)


# 
# ggdata <- gdata %>%
#   mutate(ratio = p_division_origin/p_origin) %>% 
#   filter(N > 1000, ratio > 1) %>%
#   arrange(-ratio)
# 
# 
# ggplot(ggdata, aes(for_division,ratio, color=origin, size=N))+
#   geom_point()+
#   facet_grid(.~destination)+
#   coord_flip()

ggsave('results/figures/origin_destination_relative_ratio.png', width = 14, height = 7)

