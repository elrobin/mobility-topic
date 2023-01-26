# Load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggridges)
library(googledrive)

# Import data via Google Drive
drive_auth(email = NA)
drive_auth()

pubs_id <- drive_get(id = '1qikO0VnzL-Avu25eWsKehXDsTsXNbO8z')
pubs <- pubs_id %>%
  drive_read_string(encoding = 'UTF-8') %>%
  read.delim(text = ., header = TRUE, sep = "\t", strip.white = TRUE)

researchers_id <- drive_get(id = '1DD3wGyRjHSZ0aeybVMP2s76VBgKEJwGO')
researchers <- researchers_id %>%
  drive_read_string(encoding = 'UTF-8') %>%
  read.delim(text = ., header = TRUE, sep = "\t", strip.white = TRUE)

clusters_id <- drive_get(id = '1vgP94g41V85bGrqZsXSqn4jCz63tHcL_')
clusters <- clusters_id %>%
  drive_read_string(encoding = 'UTF-8') %>%
  read.delim(text = ., header = TRUE, sep = "\t", strip.white = TRUE)

# Convert researchers in unique table
researchers <- researchers %>%
  select(researcher_id, first_year, country_code_origin) %>% # remove institution
  unique()  # remove duplicates
pubs <- unique(pubs)
############ Dataset 1 - Researcher level ############ 

# Create loop
df1 <- data.frame(
  "researcher_id" = character(),
  "active_years" = double(),
  "age" = double(),
  "total_topics" = double(),
  "total_p" = double(),
  "total_abroad" = double(),
  "total_home" = double(),
  "p_int" = double(),
  "t_cits" = double(),
  "cl_p" = double(),
  "cl_p_range" = double(),
  "cl_int" = double(),
  "cl_int_range" = double(),
  "cl_cits" = double(),
  "cl_cits_range" = double()
)

for(i in researchers$researcher_id) {
  # Search for publications
  pubs.list <- subset(pubs, researcher_id== i)
  pubs.list <- unique(pubs.list)
  # Add cluster info
  c.pubs.list <- merge(pubs.list, clusters, by ="cluster_id1", all.x = T)
  rm(pubs.list)
  # Compute variables
  active_years <- length(unique(c.pubs.list$pub_year))
  age <- 1 + (max(c.pubs.list$pub_year) - min(c.pubs.list$pub_year))
  total_topics <- length(unique(c.pubs.list$cluster_id1))
  total_p <- length(unique(c.pubs.list$pub_id))
  p_abroad <- sum(c.pubs.list$foreign_country)
  p_home <- nrow(subset(c.pubs.list, foreign_country == 0))
  pubs.no.co <-
    unique(
      c.pubs.list[, c(
        "pub_id", "int_collab", "n_cits", "p", "p_int_collab", "t_cits")])
  p_int <- sum(pubs.no.co$int_collab)
  t_cits <- sum(pubs.no.co$n_cits)
  cl_p <- median(pubs.no.co$p)
  cl_p_range <- range(cl_p, na.rm = TRUE)
  cl_int <- median(pubs.no.co$p_int_collab)
  cl_int_range <- range(cl_int, na.rm = TRUE)
  cl_cits <- median(pubs.no.co$t_cits)
  cl_cits_range <- range(cl_cits, na.rm = TRUE)
  rm(pubs.no.co)
  # Create vector
  v <-
    c(i,
      active_years,
      age,
      total_topics,
      total_p,
      p_abroad,
      p_home,
      p_int,
      t_cits,
      cl_p,
      cl_p_range,
      cl_int,
      cl_int_range,
      cl_cits,
      cl_cits_range)
  # Add to framework
  df1[nrow(df1) + 1,] <- v
}

# Create mobility/affiliation variable
df1$mob_affil <- ifelse(df1$total_abroad == 0, "non-mobile",
                       ifelse(df1$total_abroad > 0 & (df1$total_abroad + df1$total_home) == df1$total_p, "mobile",
                              ifelse(df1$total_abroad > 0 & df1$total_home == df1$total_p, "mult_affil",
                                     ifelse(df1$total_abroad > 0 & (df1$total_abroad + df1$total_home) > df1$total_p & df1$total_home < df1$total_p, "mult_affil_mob", NA)
                              )
                       )
                )

# Create mobility/affiliation vs non-mobility variable
df1$mob_affil_vs_non <- ifelse(df1$mob_affil == "non-mobile", "non-mobile",
                               ifelse(df1$mob_affil == "mobile", "mobile",
                                      ifelse(df1$mob_affil == "mult_affil", "mobile",
                                             ifelse(df1$mob_affil == "mult_affil_mob", "mobile", NA)
                                             )
                                      )
                               )
                        
# Remove NA values in mob_affil variable, cases with < 3 publications and cases with >= 14 active_years from df1
df1_clean <- filter(df1, ! is.na(mob_affil) & total_p >= 3 & active_years < 14)

# Convert active_years variable to factor
df1_clean$active_years <- as.factor(df1_clean$active_years)

# Create proportion of publications abroad variable
df1_clean$proportion_p_abroad <- df1_clean$total_abroad/df1_clean$total_p

# Create proportion of international topics variable
df1_clean$proportion_cl_int <- df1_clean$cl_int/df1_clean$cl_p

# Merge df1_clean and researchers dataframes by IDs
df1_final <- inner_join(df1_clean, researchers, by = "researcher_id")

## Data grouping for plotting purposes
# Group researchers by mobility/affiliation and country of origin
df1_final_grouped_country <- df1_final %>% group_by(mob_affil, country_code_origin) %>% 
  summarise(researchers_count = n(),.groups = 'drop') %>%
  as.data.frame()

# Group researchers by mobility/affiliation and active years
df1_final_grouped_years <- df1_final %>% group_by(mob_affil, active_years) %>% 
  summarise(researchers_count = n(),.groups = 'drop') %>%
  as.data.frame()

# Group researchers by mobility/affiliation, active years and country
df1_final_grouped_years_country <- df1_final %>% group_by(mob_affil, active_years, country_code_origin) %>% 
  summarise(researchers_count = n(),.groups = 'drop') %>%
  as.data.frame()

# Group researchers by mobility/affiliation and publications
df1_final_grouped_pubs <- df1_final %>% group_by(mob_affil, total_p) %>% 
  summarise(researchers_count = n(),.groups = 'drop') %>%
  as.data.frame()

# Group researchers by mobility/affiliation, publications and country
df1_final_grouped_pubs_country <- df1_final %>% group_by(mob_affil, total_p, country_code_origin) %>% 
  summarise(researchers_count = n(),.groups = 'drop') %>%
  as.data.frame()

## Data filtering for plotting purposes
df1_final_grouped_country_nonmob_filter <- filter(df1_final_grouped_country, mob_affil != "non-mobile")

df1_final_p100_filter <- filter(df1_final, total_p <= 100)

df1_final_t30_filter <- filter(df1_final, total_topics <= 30)

df1_final_p150_filter <- filter(df1_final, total_p <= 150)

df1_final_t40_filter <- filter(df1_final, total_topics <= 40)

df1_final_grouped_years_nonmob_filter <- filter(df1_final_grouped_years, mob_affil != "non-mobile")

df1_final_grouped_years_country_nonmob_filter <- filter(df1_final_grouped_years_country, mob_affil != "non-mobile")

df1_final_grouped_p50_filter <- filter(df1_final_grouped_pubs, total_p <= 50)

df1_final_grouped_p50_nonmob_filter <- filter(df1_final_grouped_pubs, total_p <= 50 & mob_affil != "non-mobile")

df1_final_grouped_p50_country_filter <- filter(df1_final_grouped_pubs_country, total_p <= 50)

df1_final_grouped_p50_country_nonmob_filter <- filter(df1_final_grouped_pubs_country, total_p <= 50 & mob_affil != "non-mobile")

## Plotting
# 0A) grouped barplot: RESEARCHERS by COUNTRY and MOB/AFFIL
ggplot(df1_final_grouped_country, aes(x = country_code_origin, y = researchers_count, fill = mob_affil, color = mob_affil)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  ggtitle("Number of researchers by country of origin and mobility/affiliation") +
  xlab("Country of origin") +
  ylab("Number of researchers")
#ggsave("0A.png")
#drive_update(file = as_id("1Dvg5vAf3GI1J_9pRbQOk9fx-r98KfGAw"), media = "0A.png")

# 0B) grouped barplot: RESEARCHERS by COUNTRY and MOB/AFFIL (all but non-mobile)
ggplot(df1_final_grouped_country_nonmob_filter, aes(x = country_code_origin, y = researchers_count, fill = mob_affil, color = mob_affil)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  ggtitle("Number of researchers by country of origin and mobility/affiliation (all but non-mobile)") +
  xlab("Country of origin") +
  ylab("Number of researchers")
#ggsave("0B.png")
#drive_update(file = as_id("1Slcq8vEm5bXpbEdzWNF3ylBqY2y1P4Di"), media = "0B.png")

# 1A) boxplot: MOB/AFFIL by TOTAL_P (<= 100)
ggplot(df1_final_p100_filter, aes(x = total_p, y = mob_affil, fill = mob_affil, color = mob_affil)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Mobility/affiliation of researchers by number of publications (<=100)") +
  xlab("Number of publications") +
  ylab("Mobility/Affiliation")
#ggsave("1A.png")
#drive_update(file = as_id("10-NnuhBgLTi_5yh25NiCBW_JZYCzJNOj"), media = "1A.png")

# 1B) boxplot: MOB/AFFIL by TOTAL_TOPICS (<= 30)
ggplot(df1_final_t30_filter, aes(x = total_topics, y = mob_affil, fill = mob_affil, color = mob_affil)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Mobility/affiliation of researchers by number of topics (<= 30)") +
  xlab("Number of topics") +
  ylab("Mobility/Affiliation")
#ggsave("1B.png")
#drive_update(file = as_id("1WnNQQx3zIqIUEz_v-CAE-MPrxz-U4TAS"), media = "1B.png")

# 2.2A) boxplot: TOTAL_P (<=150) by ACTIVE_YEARS and MOB/AFFIL
ggplot(df1_final_p150_filter, aes(x = active_years, y = total_p, fill = factor(mob_affil), color = factor(mob_affil))) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Number of publications (<=150) by researchers' active years and mobility/affiliation") +
  xlab("Active years") +
  ylab("Number of publications")
#ggsave("22A.png")
#drive_update(file = as_id("1mWBHZW4XeCvFpSOM4yfp1XibMNaKMdrn"), media = "22A.png")

# 2.2B) boxplot: TOTAL_TOPICS (<= 40) by ACTIVE_YEARS and MOB/AFFIL
ggplot(df1_final_t40_filter, aes(x = active_years, y = total_topics, fill = factor(mob_affil), color = factor(mob_affil))) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Number of topics (1:40) by researchers' active years and mobility/affiliation") +
  xlab("Active years") +
  ylab("Number of topics")
#ggsave("22B.png")
#drive_update(file = as_id("1pqfTG7j5hcU_n6uPJ4tMHitENb25FbCy"), media = "22B.png")

# 2.2C) boxplot: CL_P by ACTIVE_YEARS and MOB/AFFIL
ggplot(df1_final, aes(x = active_years, y = cl_p, fill = factor(mob_affil), color = factor(mob_affil))) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Topics' sizes by researchers' active years and mobility/affiliation") +
  xlab("Active years") +
  ylab("Topic size (median pubs)")
#ggsave("22C.png")
#drive_update(file = as_id("1XxGNlpP471Sbemq-iRBwJ1BMyEA_0DxY"), media = "22C.png")

# 2.2D) boxplot: CL_INT by ACTIVE_YEARS and MOB/AFFIL
ggplot(df1_final, aes(x = active_years, y = proportion_cl_int, fill = factor(mob_affil), color = factor(mob_affil))) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Proportion of international topics by researchers' active years and mobility/affiliation") +
  xlab("Active years") +
  ylab("Proportion of international topics")
#ggsave("22D.png")
#drive_update(file = as_id("1AeOjJQch2L9nlhYWT7t4pTP4GBSWGo7j"), media = "22D.png")

# 5A) boxplots: TOTAL_P (<=150) by ACTIVE_YEARS, COUNTRY and MOB/AFFIL
ggplot(df1_final_p150_filter, aes(x = active_years, y = total_p, fill = factor(mob_affil), color = factor(mob_affil))) +
  geom_boxplot() +
  facet_wrap(~country_code_origin) +
  theme_minimal() +
  ggtitle("Number of publications (<=150) by researchers' active years, country and mobility/affiliation") +
  xlab("Active years") +
  ylab("Number of publications")
#ggsave("5A.png")
#drive_update(file = as_id("1hosOMEARJO_U5YhS8Zyg21flaJCuFnzd"), media = "5A.png")

# 5B) boxplots: TOTAL_TOPICS (<= 40) by ACTIVE_YEARS, COUNTRY and MOB/AFFIL
ggplot(df1_final_t40_filter, aes(x = active_years, y = total_topics, fill = factor(mob_affil), color = factor(mob_affil))) +
  geom_boxplot() +
  facet_wrap(~country_code_origin) +
  theme_minimal() +
  ggtitle("Number of topics (1:40) by researchers' active years, country and mobility/affiliation") +
  xlab("Active years") +
  ylab("Number of topics")
#ggsave("5B.png")
#drive_update(file = as_id("1T4S1gChrspYLRdpUF_YqL_KbIRkOFy13"), media = "5B.png")

# 5C) boxplots: CL_P by ACTIVE_YEARS, COUNTRY and MOB/AFFIL
ggplot(df1_final, aes(x = active_years, y = cl_p, fill = factor(mob_affil), color = factor(mob_affil))) +
  geom_boxplot() +
  facet_wrap(~country_code_origin) +
  theme_minimal() +
  ggtitle("Topics' sizes by researchers' active years, country and mobility/affiliation") +
  xlab("Active years") +
  ylab("Topic size (median pubs)")
#ggsave("5C.png")
#drive_update(file = as_id("16mOMoc0YWUK_zLg4B93x5PY2Zn9hE5Ae"), media = "5C.png")

# 5D) boxplots: CL_INT BY ACTIVE_YEARS, COUNTRY and MOB/AFFIL
ggplot(df1_final, aes(x = active_years, y = proportion_cl_int, fill = factor(mob_affil), color = factor(mob_affil))) +
  geom_boxplot() +
  facet_wrap(~country_code_origin) +
  theme_minimal() +
  ggtitle("Proportion of international topics' by researchers' active years, country and mobility/affiliation") +
  xlab("Active years") +
  ylab("Proportion of international topics")
#ggsave("5D.png")
#drive_update(file = as_id("1izt0UsLAs0tx6TsSVK2OsVJrM5uHENaB"), media = "5D.png")

# 6A) grouped barplot: RESEARCHERS by ACTIVE_YEARS and MOB/AFFIL
ggplot(df1_final_grouped_years, aes(x = active_years, y = researchers_count, fill = mob_affil, color = mob_affil)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  ggtitle("Number of researchers by active years and mobility/affiliation") +
  xlab("Active years") +
  ylab("Number of researchers")
#ggsave("6A.png")
#drive_update(file = as_id("1ybda7E3heFnp716Qg1dbZvKB4vBYjzaH"), media = "6A.png")

# 6B) grouped barplot: RESEARCHERS by ACTIVE_YEARS and MOB/AFFIL (all but non-mobile)
ggplot(df1_final_grouped_years_nonmob_filter, aes(x = active_years, y = researchers_count, fill = mob_affil, color = mob_affil)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  ggtitle("Number of researchers by active years and mobility/affiliation (all but non-mobile)") +
  xlab("Active years") +
  ylab("Number of researchers")
#ggsave("6B.png")
#drive_update(file = as_id("1QnBHwu-o01UXJS1VrMEMpGdf2UiTZ-Ux"), media = "6B.png")

# 6C) grouped barplot: RESEARCHERS by ACTIVE_YEARS, COUNTRY and MOB/AFFIL
ggplot(df1_final_grouped_years_country, aes(x = active_years, y = researchers_count, fill = mob_affil, color = mob_affil)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~country_code_origin) +
  theme_minimal() +
  ggtitle("Number of researchers by active years, country and mobility/affiliation") +
  xlab("Active years") +
  ylab("Number of researchers")
#ggsave("6C.png")
#drive_update(file = as_id("1H586JgVQbYZDpazA6PxMDHfkPyKf2qtJ"), media = "6C.png")

# 6D) grouped barplot: RESEARCHERS by ACTIVE_YEARS, COUNTRY and MOB/AFFIL (all but non-mobile)
ggplot(df1_final_grouped_years_country_nonmob_filter, aes(x = active_years, y = researchers_count, fill = mob_affil, color = mob_affil)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~country_code_origin) +
  theme_minimal() +
  ggtitle("Number of researchers by active years, country and mobility/affiliation (all but non-mobile)") +
  xlab("Active years") +
  ylab("Number of researchers")
#ggsave("6D.png")
#drive_update(file = as_id("1cV6exhxQkjuKwzRjnZ13QWmR_ZYV8akM"), media = "6D.png")

# 7A) grouped barplot: RESEARCHERS by TOTAL_P (<=50) and MOB/AFFIL
ggplot(df1_final_grouped_p50_filter, aes(x = total_p, y = researchers_count, fill = mob_affil, color = mob_affil)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  ggtitle("Number of researchers by number of publications (<=50) and mobility/affiliation") +
  xlab("Number of publications") +
  ylab("Number of researchers")
#ggsave("7A.png")
#drive_update(file = as_id("1Po0fJuv4VmaNvMoNR0-5T5X66jc0f9pK"), media = "7A.png")

# 7B) grouped barplot: RESEARCHERS by TOTAL_P (<=50) and MOB/AFFIL (all but non-mobile)
ggplot(df1_final_grouped_p50_nonmob_filter, aes(x = total_p, y = researchers_count, fill = mob_affil, color = mob_affil)) +
  geom_bar(stat="identity", position="dodge") +
  theme_minimal() +
  ggtitle("Number of researchers by number of publications (<=50) and mobility/affiliation (all but non-mobile)") +
  xlab("Number of publications") +
  ylab("Number of researchers")
#ggsave("7B.png")
#drive_update(file = as_id("1YeV8yHKclm91DAcxuzMNPOJIj_W2dbSG"), media = "7B.png")

# 7C) grouped barplot: RESEARCHERS by TOTAL_P (<=50), COUNTRY and MOB/AFFIL
ggplot(df1_final_grouped_p50_country_filter, aes(x = total_p, y = researchers_count, fill = mob_affil, color = mob_affil)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~country_code_origin) +
  theme_minimal() +
  ggtitle("Number of researchers by number of publications (<=50), country and mobility/affiliation") +
  xlab("Number of publications") +
  ylab("Number of researchers")
#ggsave("7C.png")
#drive_update(file = as_id("1MgU3WiJjsvqisDOLGtJHEdTpNr6imqTB"), media = "7C.png")

# 7D) grouped barplot: RESEARCHERS by TOTAL_P (<=50), COUNTRY and MOB/AFFIL (all but non-mobile)
ggplot(df1_final_grouped_p50_country_nonmob_filter, aes(x = total_p, y = researchers_count, fill = mob_affil, color = mob_affil)) +
  geom_bar(stat="identity", position="dodge") +
  facet_wrap(~country_code_origin) +
  theme_minimal() +
  ggtitle("Number of researchers by number of publications (<=50), country and mobility/affiliation (all but non-mobile)") +
  xlab("Number of publications") +
  ylab("Number of researchers")
#ggsave("7D.png")
#drive_update(file = as_id("1HEPHg4gIpLvzclrKsr5nF22-kvGUS1Bq"), media = "7D.png")

# 8A) joyplot: ACTIVE_YEARS by TOTAL_TOPICS (<=40) and MOB/AFFIL
ggplot(df1_final_t40_filter, aes(x = total_topics, y = active_years, fill = active_years, color = active_years)) +
  geom_density_ridges(alpha=0.6, bandwidth=4) +
  facet_wrap(~mob_affil) +
  theme_minimal() +
  theme(legend.position="none") +
  ggtitle("Researchers' active years by number of topics (<=40) and mobility/affiliation") +
  xlab("Number of topics") +
  ylab("Active years")
#ggsave("8A.png")
#drive_update(file = as_id("1-xdO8b7pNvoGIzyr4NPnya0tp0H2LHP0"), media = "8A.png")

# 8B) joyplot: ACTIVE_YEARS by TOTAL_TOPICS (<=40) and COUNTRY
ggplot(df1_final_t40_filter, aes(x = total_topics, y = active_years, fill = active_years, color = active_years)) +
  geom_density_ridges(alpha=0.6, bandwidth=4) +
  facet_wrap(~country_code_origin) +
  theme_minimal() +
  theme(legend.position="none") +
  ggtitle("Researchers' active years by number of topics (<=40) and country") +
  xlab("Number of topics") +
  ylab("Active years")
#ggsave("8B.png")
#drive_update(file = as_id("1LthxsYPF3_SpDGXcfQzLu49x30ArqS7q"), media = "8B.png")

# 8C) joyplot: ACTIVE_YEARS by TOTAL_TOPICS (<=40) and MOB/AFFIL vs NON-MOBILITY
ggplot(df1_final_t40_filter, aes(x = total_topics, y = active_years, fill = active_years, color = active_years)) +
  geom_density_ridges(alpha=0.6, bandwidth=4) +
  facet_wrap(~mob_affil_vs_non) +
  theme_minimal() +
  theme(legend.position="none") +
  ggtitle("Researchers' active years by number of topics (<=40) and mob/affil vs non-mobility") +
  xlab("Number of topics") +
  ylab("Active years")
#ggsave("8C.png")
#drive_update(file = as_id("1yAW7CDwpbsHVuu8n_s9zDX5_WVxGbE0w"), media = "8C.png")


# Export data to Google Drive
write.csv(df1_final, file = "df1.txt")
drive_update(file = as_id("1w94k-mZ7R66KNRMQvQcrS7TimmI-aDHI"), media = "df1.txt")