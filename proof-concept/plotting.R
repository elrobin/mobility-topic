# Load libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(GGally)
library(gridExtra)
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
df2_clean <- inner_join(df1, df2_p_filter, by = "researcher_id", multiple = "all")

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

# Remove T5 rows
df2_clean <- df2_clean[df2_clean$period != "T5", ]

# Select variables for subset dataframe
variables <- c("researcher_id", "period", "proportion_p_abroad_period", "proportion_p_home_period", "proportion_new_topics", "proportion_lost_topics", "proportion_same_topics", "mobility")
df2_final <- df2_clean[variables]

# Select variables for trajectories dataframe
variables2 <- c("researcher_id", "period", "p", "new_topics", "mobility")
df2_trajectories <- df2_clean[variables2]

# Keep only researchers with publications in every period and remove rows with NA values
df2_trajectories <- df2_trajectories[df2_trajectories$researcher_id %in% names(which(table(df2_trajectories$researcher_id) == 4)), ]
#df2_trajectories <- (na.omit(df2_trajectories))

# Create trajectory variable
df2_trajectories$trajectory <- ifelse(df2_trajectories$mobility == "abroad", "A",
                                      ifelse(df2_trajectories$mobility == "transition", "T",
                                             ifelse(df2_trajectories$mobility == "home", "H", NA)
                                             )
                                      )

# Group trajectory values into one cell per ID
df2_trajectories_summary <- df2_trajectories %>%
                            group_by(researcher_id) %>%
                            summarise(trajectory = paste0(trajectory, collapse = " "))

# Copy trajectory values as many times as there are rows with the same ID
df2_trajectories_merged <- merge(df2_trajectories, df2_trajectories_summary, by = "researcher_id", all.x = TRUE)

# Select special cases
df2_trajectories_subset <- subset(df2_trajectories_merged, trajectory.y %in% c("H H H H", "H T A A", "H T H H", "H H H T", "H A A A", "H A H H", "H H H A"))

## Data grouping for plotting purposes
# Group researchers by proportion of new topics and mobility
df2_final_grouped_new_topics <- df2_final %>% group_by(proportion_new_topics, mobility) %>% 
  summarise(count = n(),.groups = 'drop') %>%
  as.data.frame()

# Remove NA rows
df2_final_grouped_new_topics <- (na.omit(df2_final_grouped_new_topics))

# Group researchers by proportion of same topics and mobility
df2_final_grouped_same_topics <- df2_final %>% group_by(proportion_same_topics, mobility) %>% 
  summarise(count = n(),.groups = 'drop') %>%
  as.data.frame()

# Remove NA rows
df2_final_grouped_same_topics <- (na.omit(df2_final_grouped_same_topics))

# Group researchers by proportion of lost topics and mobility
df2_final_grouped_lost_topics <- df2_final %>% group_by(proportion_lost_topics, mobility) %>% 
  summarise(count = n(),.groups = 'drop') %>%
  as.data.frame()

# Remove NA rows
df2_final_grouped_lost_topics <- (na.omit(df2_final_grouped_lost_topics))

# Group researchers by proportion of new topics and period
df2_final_grouped_new_topics_period <- df2_final %>% group_by(proportion_new_topics, period) %>% 
  summarise(count = n(),.groups = 'drop') %>%
  as.data.frame()

# Remove NA rows
df2_final_grouped_new_topics_period <- (na.omit(df2_final_grouped_new_topics_period))

# Group researchers by proportion of same topics and period
df2_final_grouped_same_topics_period <- df2_final %>% group_by(proportion_same_topics, period) %>% 
  summarise(count = n(),.groups = 'drop') %>%
  as.data.frame()

# Remove NA rows
df2_final_grouped_same_topics_period <- (na.omit(df2_final_grouped_same_topics_period))

# Group researchers by proportion of lost topics and period
df2_final_grouped_lost_topics_period <- df2_final %>% group_by(proportion_lost_topics, period) %>% 
  summarise(count = n(),.groups = 'drop') %>%
  as.data.frame()

# Remove NA rows
df2_final_grouped_lost_topics_period <- (na.omit(df2_final_grouped_lost_topics_period))

# Group researchers by period and mobility
df2_final_grouped_period_mobility <- df2_final %>% group_by(period, mobility) %>% 
  summarise(count = n(),.groups = 'drop') %>%
  as.data.frame()

# Remove NA rows
df2_final_grouped_period_mobility <- (na.omit(df2_final_grouped_period_mobility))

## Plots
# 0) df2 correlogram per period
ggpairs(df2_final, columns = 3:7, ggplot2::aes(colour=period), title = "df2 correlogram per period") +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal()
ggsave("0.png")

# 0A) df2 correlogram
ggcorr(df2_final[, -c(1, 2, 8)], method = c("pairwise", "pearson")) +
  theme_minimal() +
  ggtitle("df2 correlogram")
ggsave("0A.jpg")

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

# 1A) Proportion of new topics by proportion of publications abroad per time period (facet wrap presentation)
ggplot(df2_final, aes(x = proportion_p_abroad_period, y = proportion_new_topics, color = period)) +
  geom_point() +
  facet_wrap(~period) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal() +
  ggtitle("Proportion of new topics by proportion of publications abroad per time period") +
  xlab("Proportion of publications abroad") +
  ylab("Proportion of new topics")
ggsave("1A.jpg")

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

# 2A) Proportion of same topics by proportion of publications abroad per time period (facet wrap presentation)
ggplot(df2_final, aes(x = proportion_p_abroad_period, y = proportion_same_topics, color = period)) +
  geom_point() +
  facet_wrap(~period) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal() +
  ggtitle("Proportion of same topics by proportion of publications abroad per time period") +
  xlab("Proportion of publications abroad") +
  ylab("Proportion of same topics")
ggsave("2A.jpg")

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

# 3A) Proportion of lost topics by proportion of publications abroad per time period (facet wrap presentation)
ggplot(df2_final, aes(x = proportion_p_abroad_period, y = proportion_lost_topics, color = period)) +
  geom_point() +
  facet_wrap(~period) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal() +
  ggtitle("Proportion of lost topics by proportion of publications abroad per time period") +
  xlab("Proportion of publications abroad") +
  ylab("Proportion of lost topics")
ggsave("3A.jpg")

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

# 4A) Proportion of new topics by proportion of publications home per time period (facet wrap presentation)
ggplot(df2_final, aes(x = proportion_p_home_period, y = proportion_new_topics, color = period)) +
  geom_point() +
  facet_wrap(~period) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal() +
  ggtitle("Proportion of new topics by proportion of publications home per time period") +
  xlab("Proportion of publications home") +
  ylab("Proportion of new topics")
ggsave("4A.jpg")

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

# 5A) Proportion of same topics by proportion of publications home per time period (facet wrap presentation)
ggplot(df2_final, aes(x = proportion_p_home_period, y = proportion_same_topics, color = period)) +
  geom_point() +
  facet_wrap(~period) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal() +
  ggtitle("Proportion of same topics by proportion of publications home per time period") +
  xlab("Proportion of publications home") +
  ylab("Proportion of same topics")
ggsave("5A.jpg")

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

# 6A) Proportion of lost topics by proportion of publications home per time period (facet wrap presentation)
ggplot(df2_final, aes(x = proportion_p_home_period, y = proportion_lost_topics, color = period)) +
  geom_point() +
  facet_wrap(~period) +
  xlim(0, 1) +
  ylim(0, 1) +
  theme_minimal() +
  ggtitle("Proportion of lost topics by proportion of publications home per time period") +
  xlab("Proportion of publications home") +
  ylab("Proportion of lost topics")
ggsave("6A.jpg")

# 7) Proportion of new, same and lost topics per mobility type
plot7A <- ggplot(df2_final_grouped_new_topics, aes(x = proportion_new_topics, y = count, color = mobility)) +
            geom_line() +
            ylim(0, 25000) +
            theme_minimal() +
            #ggtitle("Proportion of new topics per mobility type") +
            xlab("Proportion of new topics") +
            ylab("Count")

plot7B <- ggplot(df2_final_grouped_same_topics, aes(x = proportion_same_topics, y = count, color = mobility)) +
            geom_line() +
            ylim(0, 25000) +
            theme_minimal() +
            #ggtitle("Proportion of same topics per mobility type") +
            xlab("Proportion of same topics") +
            ylab("Count")

plot7C <- ggplot(df2_final_grouped_lost_topics, aes(x = proportion_lost_topics, y = count, color = mobility)) +
            geom_line() +
            ylim(0, 25000) +
            theme_minimal() +
            #ggtitle("Proportion of lost topics per mobility type") +
            xlab("Proportion of lost topics") +
            ylab("Count")

grid.arrange(plot7A, plot7B, plot7C, nrow = 1)
grid7 <- arrangeGrob(plot7A, plot7B, plot7C, nrow=1)
ggsave("7.png", grid7)

# 8) Proportion of new, same and lost topics per time period
plot8A <- ggplot(df2_final_grouped_new_topics_period, aes(x = proportion_new_topics, y = count, color = period)) +
  geom_line() +
  #ylim(0, 25000) +
  theme_minimal() +
  #ggtitle("Proportion of new topics per time period") +
  xlab("Proportion of new topics") +
  ylab("Count")

plot8B <- ggplot(df2_final_grouped_same_topics_period, aes(x = proportion_same_topics, y = count, color = period)) +
  geom_line() +
  #ylim(0, 25000) +
  theme_minimal() +
  #ggtitle("Proportion of same topics per time period") +
  xlab("Proportion of same topics") +
  ylab("Count")

plot8C <- ggplot(df2_final_grouped_lost_topics_period, aes(x = proportion_lost_topics, y = count, color = period)) +
  geom_line() +
  #ylim(0, 25000) +
  theme_minimal() +
  #ggtitle("Proportion of lost topics per time period") +
  xlab("Proportion of lost topics") +
  ylab("Count")

grid.arrange(plot8A, plot8B, plot8C, nrow = 1)
grid8 <- arrangeGrob(plot8A, plot8B, plot8C, nrow=1)
ggsave("8.png", grid8)

# 9) Trajectories
ggplot(df2_final_grouped_period_mobility, aes(x = period, y = count, fill = mobility, color = mobility)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Trajectories") +
  xlab("Period") +
  ylab("Count")

ggplot(df2_trajectories, aes(x = period, y = new_topics, fill = mobility, color = mobility)) +
  geom_boxplot() +
  theme_minimal() +
  ggtitle("Trajectories trial 2") +
  xlab("Period") +
  ylab("Number of new topics")

# 9C) Success!
ggplot(df2_trajectories_subset, aes(x = period, y = new_topics, fill = trajectory.y, color = trajectory.y)) +
  geom_boxplot() +
  theme_minimal() +
  ylim(0, 20) +
  ggtitle("Trajectories trial 3") +
  xlab("Period") +
  ylab("Number of new topics")
ggsave("9C.jpg")
