library(ggplot2)
library(tidyverse)

# read file
df <- read_delim("~/Desktop/CS_individuals_jun23.csv", delim = ";", col_names = c("Researcher_id", "first_year", "active_years", "pub_year", "pub_id", "country_code", "author_seq", "is_corresponding_author", "cluster_id1", "LR_main_field"))
df$cluster_id1 <- as.character(df$cluster_id1)


### AUTHORS
# create dataframe based on authors' information
df_authors <- df %>%
  group_by (Researcher_id) %>%
  summarise(first_year = unique(first_year),
            active_years = unique(active_years),
            pub_year = paste(unique(sort(pub_year)), collapse = "-"),
            n_pub = length(unique(pub_id)),
            country_code = paste(unique(country_code), collapse = "-"),
            n_first_author = sum(author_seq == 1),
            author_seq = paste(unique(sort(author_seq)), collapse = "-"),
            n_author_corr = sum(is_corresponding_author),
            main_field = paste(unique(LR_main_field), collapse = "-"),
            n_main_field = length(unique(LR_main_field)),
            n_topics = length(unique(cluster_id1)))

# compute variable to identify gaps within publication years
df_authors$pub_year2 <- strsplit(df_authors$pub_year, split = "-", fixed = TRUE)
df_authors$pub_year2 <- lapply(df_authors$pub_year2, as.numeric)
df_authors$pub_year_gap <- lapply(df_authors$pub_year2, function(years) any(diff(years) > 1))

# compute variable to identify mobile and non-mobile researchers
df_authors$is_mobile <- grepl("-", df_authors$country_code)

# compute variable to determine researchers' countries of origin
df_authors <-

# compute variable to determine researchers' academic ages
df$academic_age <- (df$pub_year - df$first_year) + 1

# compute variable to classify researchers' publications into career stages
df$career_stage <- ifelse(df$academic_age <= 5, "junior",
                          ifelse(df$academic_age > 5 & df$academic_age <= 10, "early",
                                        ifelse(df$academic_age > 10, "middle", NA)))

# create dataframe based on authors' career stages information
df_authors_careers <- df %>%
  group_by (Researcher_id, career_stage) %>%
  summarise(first_year = unique(first_year),
            active_years = unique(active_years),
            pub_year = paste(unique(sort(pub_year)), collapse = "-"),
            n_pub = length(unique(pub_id)),
            country_code = paste(unique(country_code), collapse = "-"),
            n_first_author = sum(author_seq == 1),
            author_seq = paste(unique(sort(author_seq)), collapse = "-"),
            n_author_corr = sum(is_corresponding_author),
            main_field = paste(unique(LR_main_field), collapse = "-"),
            n_main_field = length(unique(LR_main_field)),
            n_topics = length(unique(cluster_id1)))


### TOPICS
# create dataframe based on topics' information
df_topics <- df %>%
  group_by (cluster_id1, career_stage) %>%
  summarise(n_pub = length(unique(pub_id)),
            main_field = paste(unique(LR_main_field), collapse = "-"),
            n_main_field = length(unique(LR_main_field)))

# compute variable to determine the proportion of publications per topic
df_topics$pp_topic <- df_topics$n_pub / 7280077


### MAIN FIELDS
# create dataframe based on main fields' information
df_fields <- df %>%
  group_by (LR_main_field, career_stage) %>%
  summarise(n_researchers = length(unique(Researcher_id)))
