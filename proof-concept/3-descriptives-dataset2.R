###### Descriptives Dataset 2 ######

# Load libraries
library(dplyr)
library(googledrive)
library(tidyverse)
library(readr)

# Import data via Google Drive
drive_auth(email = NA)
drive_auth()

# Import df1 via Google Drive
df1_id <- drive_get(id = '1w94k-mZ7R66KNRMQvQcrS7TimmI-aDHI')
df1 <- df1_id %>%
  drive_read_string(encoding = 'UTF-8') %>%
  read.delim(text = ., header = TRUE, sep = ",", strip.white = TRUE)
df1 <- df1[,-1] # remove index column

# Import df2 via Google Drive
df2_id <- drive_get(id = '1L4MEzeH5JZn9zcyx_GthoNjzrWcSKpC1')
df2 <- df2_id %>%
  drive_read_string(encoding = 'UTF-8') %>%
  read.delim(text = ., header = TRUE, sep = ",", strip.white = TRUE)
df2 <- df2[,-1] # remove index column

# Create proportion of publications home and abroad vars
df2$pp_home_period <- df2$p_home_period/df2$p
df2$pp_abroad_period <- df2$p_abroad_period/df2$p

# Add overall mobility types to df2 and some general vars
df2_mobtypes <-
  merge(x = df2, 
        y = df1[, c("researcher_id",
                             "active_years",
                             "age",
                             "mob_affil",
                             "country_code_origin")],
        by = "researcher_id",
        all.x = TRUE)

# Remove NA values for mobility type and non-productive periods
df2_mobtypes <- filter(df2_mobtypes, ! is.na(mob_affil))
df2_mobtypes <- filter(df2_mobtypes, p != 0)

# Filter researchers to see proportion per period and mobility type
ggplot(df2_mobtypes, aes(x = period)) +
  geom_bar()

# Share of publications abroad by period and mobility type

ggplot(df2_mobtypes, aes(x = period, y = pp_abroad_period, fill = mob_affil)) +
  geom_boxplot()
