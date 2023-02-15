# Load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(googledrive)

# Import data via Google Drive
drive_auth(email = NA)
drive_auth()

df1_id <- drive_get(id = '1w94k-mZ7R66KNRMQvQcrS7TimmI-aDHI')
df1 <- df1_id %>%
  drive_read_string(encoding = 'UTF-8') %>%
  read.delim(text = ., header = TRUE, sep = ",", strip.white = TRUE)

df2_id <- drive_get(id = '1L4MEzeH5JZn9zcyx_GthoNjzrWcSKpC1')
df2 <- df2_id %>%
  drive_read_string(encoding = 'UTF-8') %>%
  read.delim(text = ., header = TRUE, sep = ",", strip.white = TRUE)

# Filter publications
df2_p_filter <- filter(df2, p > 0)

# Replace -1 values to NA
df2_p_filter[df2_p_filter == -1] <- NA

# Merge df1 and df2_p_filter dataframes by IDs
df2_clean <- inner_join(df1, df2_p_filter, by = "researcher_id")

# Create proportion of publications abroad variable
df2_clean$proportion_p_abroad_period <- df2_clean$p_abroad_period/df2_clean$p

# Create proportion of publications home variable
df2_clean$proportion_p_home_period <- df2_clean$p_home_period/df2_clean$p

# Create proportion of new topics variable
df2_clean$proportion_new_topics <- df2_clean$new_topics/df2_clean$total_topics

# Create proportion of lost topics variable
df2_clean$proportion_lost_topics <- df2_clean$lost_topics/df2_clean$total_topics

# Create proportion of same topics variable
df2_clean$proportion_same_topics <- df2_clean$same_topics/df2_clean$total_topics

# Create mobility variable
df2_clean$mobility <- ifelse(df2_clean$proportion_p_abroad_period == 0, "home",
                               ifelse(df2_clean$proportion_p_abroad_period == 1, "abroad",
                                      ifelse(df2_clean$proportion_p_abroad_period > 0 & df2_clean$proportion_p_abroad_period < 1, "transition", NA)
                                             )
                                      )

# Select variables for subset dataframe
variables <- c("researcher_id", "period", "proportion_p_abroad_period", "proportion_p_home_period", "proportion_new_topics", "proportion_lost_topics", "proportion_same_topics", "mobility")
df2_subset <- df2_clean[variables]

# Remove T5 rows
df2_final <- df2_subset[df2_subset$period != "T5", ]

# PLOTS
# 1) Proportion of new topics by proportion of publications abroad per time period
ggplot(df2_final, aes(x = proportion_p_abroad_period, y = proportion_new_topics, color = period)) +
  geom_point() +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal() +
  ggtitle("Proportion of new topics by proportion of publications abroad per time period") +
  xlab("Proportion of publications abroad") +
  ylab("Proportion of new topics")
ggsave("1.png")

# 2) Proportion of same topics by proportion of publications abroad per time period
ggplot(df2_final, aes(x = proportion_p_abroad_period, y = proportion_same_topics, color = period)) +
  geom_point() +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal() +
  ggtitle("Proportion of same topics by proportion of publications abroad per time period") +
  xlab("Proportion of publications abroad") +
  ylab("Proportion of same topics")
ggsave("2.png")

# 3) Proportion of lost topics by proportion of publications abroad per time period
ggplot(df2_final, aes(x = proportion_p_abroad_period, y = proportion_lost_topics, color = period)) +
  geom_point() +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal() +
  ggtitle("Proportion of lost topics by proportion of publications abroad per time period") +
  xlab("Proportion of publications abroad") +
  ylab("Proportion of lost topics")
ggsave("3.png")

# 4) Proportion of new topics by proportion of publications home per time period
ggplot(df2_final, aes(x = proportion_p_home_period, y = proportion_new_topics, color = period)) +
  geom_point() +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal() +
  ggtitle("Proportion of new topics by proportion of publications home per time period") +
  xlab("Proportion of publications home") +
  ylab("Proportion of new topics")
ggsave("4.png")

# 5) Proportion of same topics by proportion of publications home per time period
ggplot(df2_final, aes(x = proportion_p_home_period, y = proportion_same_topics, color = period)) +
  geom_point() +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal() +
  ggtitle("Proportion of same topics by proportion of publications home per time period") +
  xlab("Proportion of publications home") +
  ylab("Proportion of same topics")
ggsave("5.png")

# 6) Proportion of lost topics by proportion of publications home per time period
ggplot(df2_final, aes(x = proportion_p_home_period, y = proportion_lost_topics, color = period)) +
  geom_point() +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal() +
  ggtitle("Proportion of lost topics by proportion of publications home per time period") +
  xlab("Proportion of publications home") +
  ylab("Proportion of lost topics")
ggsave("6.png")

# 7) CONTINUAR
df2_final %>%
  pivot_longer(proportion_new_topics:proportion_same_topics, names_to = "topics", values_to = "proportion") %>%
  ggplot(aes(x = proportion, y = topics, color = mobility)) +
  geom_line(stat = "count") +
  xlim(0, 1) +
  ylim(0, 1) +
  facet_wrap(vars(topics), ncol = 3) +
  labs(x = "Proportion", y = "Topics")
