library(tidyverse)
# library(lsa)
library(readxl)
library(plotly)
library(countrycode)
library(philentropy)

# df_dim <- read_delim('data/country_portfolios_dimensions.csv',delim = ';') %>%
# # df_dim <- read_delim('data/country_portfolios_dimensions.csv',delim = ';') %>%
#   rename(N=p, 
#          field = for_group_id) %>% 
#   filter(type !='all') %>% #!country_code %in% c('ZZALL','Unknown')
#   mutate(type = case_match(type,
#                            'national' ~ 'National',
#                            'international' ~ 'International',
#                            'migrant' ~ 'Immigrant',
#                            'abroad' ~ 'Emigrant'),
#          country_code = countrycode(country_code, origin = 'iso2c', destination = 'country.name')
#   )  

# df_wos <- read_excel('../../data/country_portfolios_WOS.xlsx') %>%
# # df_wos <- read_excel('data/country_portfolios_WOS.xlsx') %>% 
#   rename(country_code=eregroupement,
#          field = Especialite,
#          type=Type)
  
filter_countries <- function(df,tr=1000){
  countries_keep <- df %>% group_by(country_code) %>% 
    summarise(N=sum(N)) %>% 
    filter(N>tr) %>% 
    pull(country_code)
  
  df %>% 
    filter(country_code %in% countries_keep)
}

complete_data <-function(df){
  df %>% 
    group_by(type,field,country_code) %>% 
    summarise(N = sum(N)) %>% 
    ungroup() %>% 
    mutate(field = factor(field),
           country_code = factor(country_code)) %>% 
    complete(type,field,country_code,fill=list(N=0))
}

filter_empty_topics <- function(df) {
  empty_topics <- df %>% 
    group_by(field,country_code) %>% 
    summarise(N= sum(N)) %>% 
    filter(N==0)
  
  df %>% 
    anti_join(empty_topics)
}

summarise_data <- function(df){
  
  df <- df %>% 
    pivot_wider(id_cols = c(country_code,field),names_from = type, values_from = N) %>%
  # mutate(All = International + Immigrant + National,
  #        c_International = All - International,
  mutate(All =  Emigrant+Immigrant + National,
         c_Immigrant = All - Immigrant,
         c_National = All - National,
         c_Emigrant = All - Emigrant) %>% 
    select(-All) %>% 
    pivot_longer(Emigrant:c_Emigrant,names_to = 'type',values_to = 'N') %>% 
    group_by(type,country_code) %>% 
    reframe(field,
            N,
            p = N/sum(N))
  
  global_dist <- df %>%
    group_by(field) %>% 
    summarise(global_N = sum(N)) %>% 
    ungroup() %>% 
    mutate(global_p = global_N/sum(global_N))
  
  df %>% 
    left_join(global_dist,by = join_by(field)) %>% 
    mutate(rca = p/global_p)
}

compute_distances <- function(df,dist_method='kullback-leibler',type='pairs', v='p'){
  
  val <- sym(v)
  if (type=='pairs') {
    
  res <- df %>% 
    select(-country_code,-N) %>% 
    pivot_wider(id_cols = c(country_code,field),names_from = type,values_from = !!val) %>% 
    summarise(#International_Immigrant = distance(rbind(International,Immigrant),method=dist_method),
              #International_National = distance(rbind(International,National),method=dist_method),
              Immigrant_National = distance(rbind(Immigrant,National),method=dist_method),
              Emigrant_Immigrant = distance(rbind(Emigrant,Immigrant),method=dist_method),
              Emigrant_National= distance(rbind(Emigrant,National),method=dist_method))
              #Emigrant_International= distance(rbind(Emigrant,International),method=dist_method))
  }
  if (type=='diff') {
    res <- df %>%
      select(-country_code) %>%
      pivot_wider(id_cols = c(country_code,field),names_from = type, values_from = !!val) %>%
      summarise(#International = distance(rbind(International,c_International),method=dist_method),
                National = distance(rbind(National,c_National),method=dist_method),
                Immigrant = distance(rbind(Immigrant,c_Immigrant),method=dist_method),
                Emigrant = distance(rbind(Emigrant,c_Emigrant),method=dist_method)
      )
  }
  res
  }

# compute_distance <- function(df, distance_formula){
#   df %>% 
#     group_by(country_code) %>% 
#     distance_formula()
# }

total_by_country <- function(df){
  
  df %>% 
    group_by(country_code) %>% 
    summarise(country_N = sum(N))
    
}

build_dataset <- function(df,t=1000,f_empty_topics=FALSE, comparison_group='diff',
                          distance_formula="kullback-leibler", val='rca'){
  
  # df: dataset used (df_dim)
  # t: Threshold, countries with les than t papers are removed
  # f_empty_topics: remove topics from the vector of country that have 0 publications
  # comparison grous: 'pairs' or 'diff'. pairs of groups or the group w.r.t. others
  # distance formula: any from ?distance()
  # val: either the probabilities or the rca vector
  
  tot_n <- total_by_country(df)
  df <- df %>% 
    filter_countries(tr=t) %>% 
    complete_data()
  
  if (f_empty_topics) {
    df <- df %>% filter_empty_topics()
  }
    df <- df %>% 
    summarise_data() %>% 
    group_by(country_code) 
    
    df <- compute_distances(df,dist_method = distance_formula,
                            type = comparison_group, v = val)
  # if (comparison_group=='pairs') {
  #   df <- dist_pairs(df,dist_method = distance_formula, v = val)
  # }
  # if(comparison_group=='diff'){
  #   df <- dist_diff(df,dist_method = distance_formula, v = val)
  # }

    df <- df %>% 
      pivot_longer(cols = -country_code, names_to = 'type', values_to = 'distance') %>% 
    left_join(tot_n)
  df
}

# plot_data(df_dim,t = 5000,f_empty_topics = TRUE,
#           comparison_group = 'diff',distance_formula = 'cosine', plotly = TRUE,
#           regions=TRUE)
#plot_data(df_wos,t = 1000,distance_formula = kl_diff, plotly = TRUE)
## proportion of type respect to distance


# build_dataset(df_dim,t=1000,f_empty_topics=TRUE, comparison_group='diff',
#                           distance_formula="cosine", val='rca')
#   

plot_distance_N <- function(df,t,f_empty_topics=TRUE, comparison_group='pairs',
                      distance_formula="kullback-leibler",plotly=FALSE,regions=TRUE,val='rca'){
  
  gdata <- build_dataset(df,t,f_empty_topics = f_empty_topics, comparison_group=comparison_group,
                         distance_formula=distance_formula, val=val) %>% 
    mutate(region = countrycode(country_code, origin = 'country.name', destination = "un.region.name"))
  
  if (regions) {
    gdata <- gdata %>% 
      filter(region!='NA')
  }
  
  g <- gdata %>% ggplot(aes(country_N, distance, color=type, label=country_code)) +
    geom_point()+
    labs(title = paste("comparison_group:",comparison_group,'-','distance_formula:',distance_formula))+
    scale_x_log10()
  
  if (regions) {
    g <- g + facet_wrap(.~region)
  }
  
  if (plotly) {
    ggplotly(g)
  }else{
    g
  }
}


## figure two: w.r.p to the proportion of the group on the country

summarise_data2 <- function(df){
  df %>% 
    group_by(country_code,type) %>% 
    summarise(N_type = sum(N)) %>% 
    group_by(country_code) %>% 
    reframe(type,
            p_type = N_type/sum(N_type))
}


build_dataset2 <- function(df,t=1000,f_empty_topics=TRUE, #comparison_group='diff',
                           distance_formula="kullback-leibler"){
  
  tot_n <- total_by_country(df)
  prop_type <-  summarise_data2(df)
  df <- df %>% 
    filter_countries(tr=t) %>% 
    complete_data()
  
  if (f_empty_topics) {
    df <- df %>% filter_empty_topics()
  }
  df <- df %>% 
    summarise_data() %>% 
    group_by(country_code) 
  
  # if (comparison_group=='pairs') {
  #   df <- dist_pairs(df,dist_method = distance_formula)
  # }else{
  # }
  df <- dist_diff(df,dist_method = distance_formula) %>% 
    pivot_longer(cols = -country_code, names_to = 'type', values_to = 'distance') %>% 
    left_join(prop_type) %>% 
    left_join(tot_n)
  df
}

plot_distance_prop <- function(df,t=5000,f_empty_topics=TRUE, #comparison_group='pairs',
                            distance_formula="cosine",plotly=FALSE,regions=TRUE){
  
  gdata <- build_dataset2(df,t,f_empty_topics = f_empty_topics, distance_formula=distance_formula) %>% 
    mutate(region = countrycode(country_code, origin = 'country.name', destination = "un.region.name")) %>% 
    filter(Type!='Emmigrant') #The comparison of the size of the emmigrant group w.r.t. their country of origin is not informative 
  
  if (regions) {
    gdata <- gdata %>% 
      filter(region!='NA')
  }
  
  g <- gdata %>% ggplot(aes(p_type, distance, color=region, label=country_code)) +
    geom_point()+
    labs(title = paste('distance_formula:',distance_formula))+
    lims(x=c(0,0.8))

  
  if (regions) {
    g <- g + facet_wrap(type~region)
  }else{
    g <- g + facet_grid(.~type)
  }
  
  if (plotly) {
    ggplotly(g)
  }else{
    g
  }
}
