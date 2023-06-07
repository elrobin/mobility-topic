

```{r}
country_origin_simple <-
  df %>% 
  filter(first_year==pub_year) %>% 
  group_by(Researcher_id) %>% 
  summarise(country_code = unique(country_code),
            ncountry=length(country_code)) %>% 
  filter(ncountry == 1) %>% 
  mutate(country_origin = country_code) %>% 
  select(-country_code, -ncountry)

df_first_year <- df %>% 
  filter(pub_year==first_year, 
         !Researcher_id %in% country_origin_simple$Researcher_id) %>% 
  group_by(Researcher_id) %>% 
  summarise(first_countries = paste0(unique(country_code),collapse ='-'))

country_origin_2 <- 
  df %>% 
  filter(!Researcher_id %in% country_origin_simple$Researcher_id) %>% 
  left_join(df_first_year, by = 'Researcher_id') %>%
  filter(!is.na(first_countries)) %>%
  group_by(Researcher_id,country_code) %>% 
  summarise(ncountry = n(),
            is_in_first_countries = str_detect(string = first_countries,pattern = country_code)) %>% 
  distinct() %>% 
  filter(is_in_first_countries) %>% 
  group_by(Researcher_id) %>% 
  mutate(nmax = max(ncountry)) %>% 
  filter(ncountry == nmax) %>% 
  distinct(Researcher_id, .keep_all = TRUE) %>% 
  select(Researcher_id, country_origin = country_code)

country_origin <- bind_rows(country_origin_simple,country_origin_2)
```


```{r}
df <- df %>% 
  left_join(country_origin) %>% 
  filter(!is.na(country_origin)) %>%
  mutate(country_code = factor(country_code),
         cluster_id1 = factor(cluster_id1))
```

```{r}
df2 <- df %>% 
  group_by(pub_id) %>% 
  mutate(w_migrant = any(country_origin != country_code),
         int_collab =  length(unique(country_code))>1)
```


1. research profile of papers without migrants and without international collaborations

```{r}
domestic_TP <- df %>% 
  filter(country_code==country_origin) %>% 
  group_by(country_code,cluster_id1) %>% 
  count() %>% 
  ungroup() %>% 
  complete(country_code,cluster_id1,fill=list(n=0))
```

2.research profile of papers with migrant authors (including those with international collaborations)

```{r}
migrants_TP <- df %>% 
  filter(country_code!=country_origin) %>% 
  group_by(country_code,cluster_id1) %>% 
  count() %>% 
  ungroup() %>% 
  complete(country_code,cluster_id1,fill=list(n=0))
```


3. topical profile of papers with international collaborations and without migrants

```{r}
int_collab_df <- df %>% 
  group_by(pub_id) %>% 
  mutate(involved_countries = length(unique(country_code)))

int_collab_TP <- int_collab_df %>% 
  filter(involved_countries>1) %>% 
  group_by(country_code,cluster_id1) %>% 
  count() %>% 
  ungroup() %>% 
  complete(country_code,cluster_id1,fill=list(n=0))
```


## Cosine similarity

between the three topical profiles of each country.

result should look like a matrix of 4x4 per country

```{r}
# cos_country <- country_topical_profile %>% 
#  # select(-cluster_id1) %>% 
#   pivot_wider(names_from = cluster_id1, values_from = n) %>% 
#   select(-country_code) %>% 
#   as.matrix() %>% 
#   cosine(.)


```
