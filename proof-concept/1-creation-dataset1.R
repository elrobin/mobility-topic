# Load libraries
library(readr)
library(dplyr)
library(tidyverse)
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
<<<<<<< Updated upstream
df1$mob_affil_vs_non <- ifelse(df1$mob_affil == "non-mobile", "non-mobile",
                               ifelse(df1$mob_affil == "mobile", "mobile",
                                      ifelse(df1$mob_affil == "mult_affil", "mobile",
                                             ifelse(df1$mob_affil == "mult_affil_mob", "mobile", NA)
                                             )
                                      )
                               )
                        
=======
df1$mob_dummy <- 
  ifelse(
    df1$mob_affil == "non-mobile",
    "non-mobile",
    ifelse(
      df1$mob_affil == "mobile",
      "mobile",
      ifelse(
        df1$mob_affil == "mult_affil",
        "mobile",
        ifelse(df1$mob_affil == "mult_affil_mob", "mobile", NA)
      )
    )
  )

>>>>>>> Stashed changes
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

# Export data to Google Drive
write.csv(df1_final, file = "df1.txt")
drive_update(file = as_id("1w94k-mZ7R66KNRMQvQcrS7TimmI-aDHI"), media = "df1.txt")