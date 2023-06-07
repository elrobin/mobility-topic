library(tidyverse)
# library(lsa)
library(readxl)
library(plotly)
library(countrycode)
library(philentropy)

df_dim <- read_delim('../../data/country_portfolios_dimensions.csv',delim = ';') %>%
# df_dim <- read_delim('data/country_portfolios_dimensions.csv',delim = ';') %>% 
  rename(N=p, 
         field = for_group_id) %>% 
  filter(type !='all') %>% #!country_code %in% c('ZZALL','Unknown')
  mutate(type = case_match(type,
                           'national' ~ 'National',
                           'international' ~ 'International',
                           'migrant' ~ 'Mobile'),
         country_code =countrycode(country_code, origin = 'iso2c', destination = 'country.name')
  )  

df_wos <- read_excel('../../data/country_portfolios_WOS.xlsx') %>%
# df_wos <- read_excel('data/country_portfolios_WOS.xlsx') %>% 
  rename(country_code=eregroupement,
         field = Especialite,
         type=Type)
  

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
  df %>% 
    group_by(type,country_code) %>% 
    reframe(field,
            N,
            p = N/sum(N))
}


dist_pairs <- function(df,dist_method='kullback-leibler'){
  df %>% 
    select(-country_code,-N) %>% 
    pivot_wider(names_from = type,values_from = p) %>% 
    summarise(International_Mobile = distance(rbind(International,Mobile),method=dist_method),
              International_National = distance(rbind(International,National),method=dist_method),
              Mobile_National = distance(rbind(Mobile,National),method=dist_method))
}

dist_diff <- function(df,dist_method='kullback-leibler'){
  df %>%
    select(-country_code,-p) %>% 
    pivot_wider(id_cols = c(country_code,field),names_from = type,values_from = N) %>% 
    mutate(All = International + Mobile + National,
           International = All - International,
           Mobile = All - Mobile,
           National = All - National) %>% 
    mutate(International = International/sum(International),
           Mobile = Mobile/sum(Mobile),
           National = National/sum(National),
           All = All/sum(All)) %>% 
    summarise(International = distance(rbind(International,All),method=dist_method),
              National = distance(rbind(National,All),method=dist_method),
              Mobile = distance(rbind(Mobile,All),method=dist_method))
}



compute_distance <- function(df, distance_formula){
  df %>% 
    group_by(country_code) %>% 
    distance_formula()
}

total_by_country <- function(df){
  
  df %>% 
    group_by(country_code) %>% 
    summarise(country_N = sum(N))
    
}

build_dataset <- function(df,t=1000,f_empty_topics=TRUE, comparison_group='diff',
                          distance_formula="kullback-leibler"){
  
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
    
  if (comparison_group=='pairs') {
    df <- dist_pairs(df,dist_method = distance_formula)
  }else{
    df <- dist_diff(df,dist_method = distance_formula)
  }
    df <- df %>% 
      pivot_longer(cols = -country_code, names_to = 'type', values_to = 'distance') %>% 
    left_join(tot_n)
  df
}

plot_data <- function(df,t,f_empty_topics=TRUE, comparison_group='pairs',
                      distance_formula="kullback-leibler",plotly=FALSE){
  g <- build_dataset(df,t,f_empty_topics = f_empty_topics, comparison_group=comparison_group,
                     distance_formula=distance_formula) %>% 
    ggplot(aes(country_N, distance, color=type, label=country_code)) +
    geom_point()+
    labs(title = paste("comparison_group:",comparison_group,'-','distance_formula:',distance_formula))+
    scale_x_log10()
  
  if (plotly) {
    ggplotly(g)
  }else{
    g
  }
}

plot_data(df_dim,t = 5000,f_empty_topics = TRUE,
          comparison_group = 'diff',distance_formula = 'cosine', plotly = TRUE)
#plot_data(df_wos,t = 1000,distance_formula = kl_diff, plotly = TRUE)

