library(tidyverse)
library(countrycode)
library(ggraph)
library(tidygraph)
library(readxl)
library(gt)

df <- read_excel('data/discipline_country_migrations.xlsx',sheet = 'Data')

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
                                        destination = 'continent')) %>% 
  filter(!is.na(origin),!is.na(destination)) %>% 
  select(-c(country_code_origin,country_code))

nodes_country <- df %>% 
  group_by(origin) %>% 
  summarise(N=sum(N)) %>% 
  mutate(continent =countrycode(origin, 
                                origin = 'country.name',
                                destination = 'continent')) %>% 
  filter(!is.na(continent))

nodes_country <- nodes_country %>% 
  arrange(-N) %>%
  ungroup() %>% 
  mutate(p = N/sum(N),
         country=factor(origin, levels=origin),
         continent = factor(continent, levels=unique(continent))) 
# filter(p>0.01)

plot_graph <- function(df,nodes_metadata, layout='circle',width='N'){
  g <- as_tbl_graph(df)
  g <- g %>% 
    activate(nodes) %>%
    left_join(nodes_metadata, by=join_by(name==origin))
  ggraph(g, layout = layout) + 
    geom_edge_fan(aes(alpha = N, edge_width=width,color=for_division),arrow = arrow(type = "closed", length = unit(1, "mm"))) + 
    geom_node_label(aes(label=name, fill = continent)) + 
    theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
}



#relations with N>1000 and top 1% countries of origin

df_graph <- df %>% 
  filter(origin%in%nodes_country$origin, destination%in%nodes_country$origin) %>% 
  filter(N>1000) %>%
  mutate(from = origin, to=destination)

plot_graph(df_graph,nodes_country)

# US and China as Origins and destinations --------------------------------

df %>% 
  filter(origin%in% c('China','United States'), destination %in% c('United States')) %>% 
  mutate(from = origin, to=destination) %>% 
  plot_graph(.,nodes_country)



# countries as destination ------------------------------------------------

plot_country_origins <- function(data,nodes_metadata, dest = 'United States',
                                 min_share_origin=.01,min_share_division=.01){
  
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
    filter(p_origin>=min_share)
  
  gdata <-  data %>% 
    filter(destination==dest, for_division%in%disciplines_prop$for_division) %>%
    # filter(origin%in%immigrants_prop$origin,destination==dest) %>%
    inner_join(immigrants_prop,by = join_by(origin)) %>% 
    group_by(for_division) %>% 
    mutate(total_division = sum(N)) %>% 
    ungroup() %>% 
    mutate(p_division = N/total_division,
           ratio = p_division/p_origin,
           from = origin, to=destination) %>% 
    filter(ratio>1.5)
  
  g <- as_tbl_graph(gdata)
  g <- g %>% 
    activate(nodes) %>%
    left_join(nodes_metadata, by=join_by(name==origin))
  
  ggraph(g, layout ='graphopt' ) + 
    geom_edge_fan(aes(edge_width=ratio,color=for_division),arrow = arrow(type = "closed", length = unit(1, "mm"))) + 
    geom_node_label(aes(label=name, fill = continent)) + 
    theme_graph(foreground = 'steelblue', fg_text_colour = 'white')
  
  
}


plot_country_origins(df,nodes_country,min_share_origin=0.01,min_share_division=0)
