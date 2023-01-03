# Load libraries
library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggridges)

# Import data
pubs <- read_delim("G:/Mi unidad/1. Work sync/Projects/2 In progress/2021_Cassidy/proof_of_concept/data/pubs.txt", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE)
researchers <- read_delim("G:/Mi unidad/1. Work sync/Projects/2 In progress/2021_Cassidy/proof_of_concept/data/researchers.txt", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE)

clusters <- read_delim("G:/Mi unidad/1. Work sync/Projects/2 In progress/2021_Cassidy/proof_of_concept/data/clusters.txt", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE)
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
  "cl_int" = double(),
  "cl_cits" = double()
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
  cl_p_range <- range(cl_p, na.rm = TRUE) # Añadir rango max-min (¿dentro del loop?)
  cl_int <- median(pubs.no.co$p_int_collab)
  cl_int_range <- range(cl_int, na.rm = TRUE) # Añadir rango max-min (¿dentro del loop?)
  cl_cits <- median(pubs.no.co$t_cits)
  cl_cits_range <- range(cl_cits, na.rm = TRUE) # Añadir rango max-min (¿dentro del loop?)
  # Añadir país de origen
  # Añadir tipo de movilidad
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
      cl_int,
      cl_cits)
  # Add to framework
  df1[nrow(df1) + 1,] <- v
}
# Export
write.csv(df1, file = "G:/Mi unidad/1. Work sync/Projects/2 In progress/2021_Cassidy/proof_of_concept/df1.txt")

