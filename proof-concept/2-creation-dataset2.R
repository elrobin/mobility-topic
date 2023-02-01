# Load libraries
library(readr)
library(dplyr)
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

                      ############ Dataset 2 - Researcher-Period ############ 

# Import df1 via Google Drive
df1_id <- drive_get(id = '1w94k-mZ7R66KNRMQvQcrS7TimmI-aDHI')
df1 <- df1_id %>%
  drive_read_string(encoding = 'UTF-8') %>%
  read.delim(text = ., header = TRUE, sep = ",", strip.white = TRUE)

# Only work with researchers with at least 3 publications
f.res.list <- subset(df1[,-1], total_p>2)

# Create empty dataframe
df2 <- data.frame(
  "researcher_id" = character(),
  "period" = character(),
  "topics" = double(),
  "p" = double(),
  "p_abroad_period" = double(), # Publications abroad per period
  "p_home_period" = double(), # Publications home per period
  "p_int_period" = double(), # Publications with international collaboration per period
  "t_cits_period" = double(), # Citations per period
  "cl_p_period" = double(), # Median publications in topics per period
  "cl_int_period" = double(), # Median publications in topics with int. collaboration per period
  "cl_cits_period" = double(), # Median citations in topics per period
  "same_topics" = double(), # Maintained topics compared with previous period
  "new_topics" = double(), # new topics compared with previous period
  "lost_topics" = double() # dissolved topics compared with previous period
)
period <- c("T1", "T2", "T3", "T4", "T5")

# Create periods
for(i in f.res.list$researcher_id){
  pub.list <- subset(pubs, researcher_id == i) # Example for tryouts ur.01000000175.26
  pub.list <- 
    merge(pub.list, f.res.list, by = "researcher_id", all.x = T)
  pub.list <- pub.list %>%
    mutate(period = ifelse(
      pub.list$pub_year - pub.list$first_year < 3,
      "T1",
      ifelse(
        pub.list$pub_year - pub.list$first_year > 2 &
          pub.list$pub_year - pub.list$first_year < 6,
        "T2",
        ifelse(
          pub.list$pub_year - pub.list$first_year > 5 &
            pub.list$pub_year - pub.list$first_year < 9,
          "T3",
          ifelse(
            pub.list$pub_year - pub.list$first_year > 8 &
              pub.list$pub_year - pub.list$first_year < 12,
            "T4",
            "T5"
          )
        )
      )
    ))

  # Compute churning vars
  t1df <- subset(pub.list, period == "T1")
  T1 <- unique(t1df$cluster_id1)
  t2df <- subset(pub.list, period == "T2")
  T2 <- unique(t2df$cluster_id1)
  t3df <- subset(pub.list, period == "T3")
  T3 <- unique(t3df$cluster_id1)
  t4df <- subset(pub.list, period == "T4")
  T4 <- unique(t4df$cluster_id1)
  t5df <- subset(pub.list, period == "T5")
  T5 <- unique(t5df$cluster_id1)
  #remove unnecessary df
  rm(list = c("t1df", "t2df", "t3df", "t4df", "t5df"))
  # Create vector which will be 
  #   v1 m_cl Maintained topics
  same_topics1 <- -1
  if (length(T2) == 0) {
    same_topics2 <- -1
  } else {
    same_topics2 <- length(intersect(T1, T2))
  }
  if (length(T2)==0 | length(T3) == 0) {
    same_topics3 <- -1
  } else {
    same_topics3 <- length(intersect(T2, T3))
  }
  if (length(T4)==0 | length(T3) == 0) {
    same_topics4 <- -1
  } else{
    same_topics4 <- length(intersect(T3, T4))
  }
  if (length(T4)==0 | length(T5) == 0) {
    same_topics5 <- -1
  } else {
    same_topics5 <- length(intersect(T4, T5))
  }
  same_topics_all <- c(same_topics1, same_topics2, same_topics3, same_topics4, same_topics5)
  rm(same_topics1, same_topics2, same_topics3, same_topics4, same_topics5)
  #   v2 n_cl New topics
  new_topics1 <- -1
  if (length(T2) == 0) {
    new_topics2 <- -1
  } else{
    new_topics2 <-
      length(T2) - length(intersect(T1, T2))
  }
  if (length(T2)==0 | length(T3) == 0) {
    new_topics3 <- -1
  } else{
    new_topics3 <- length(T3)-length(intersect(T2, T3))
  }
  if (length(T4)==0 | length(T3) == 0) {
    new_topics4 <- -1
  } else{
    new_topics4 <- length(T4)-length(intersect(T3, T4))
  }
  if (length(T4)==0 | length(T5) == 0) {
    new_topics5 <- -1
  } else {
    new_topics5 <- length(T5)-length(intersect(T4, T5))
  }
  new_topics_all <- c(new_topics1, new_topics2, new_topics3, new_topics4, new_topics5)
  rm(new_topics1, new_topics2, new_topics3, new_topics4, new_topics5)
  #   v3 d_cl Dissolved ties
  lost_topics1 <- -1
  if (length(T2) == 0) {
    lost_topics2 <- -1
  } else {
    lost_topics2 <-
      length(T1) - length(intersect(T1, T2))
  }
  if (length(T3) == 0 | length(T2)==0) {
    lost_topics3 <- -1
  } else {
    lost_topics3 <-
      length(T2) - length(intersect(T2, T3))
  }
  if (length(T4) == 0 | length(T3)==0) {
    lost_topics4 <- -1
  } else {
    lost_topics4 <-
      length(T3) - length(intersect(T3, T4))
  }
  if (length(T4)==0 | length(T5) == 0) {
    lost_topics5 <- -1
  } else {
    lost_topics5 <-
      length(T4) - length(intersect(T4, T5))
  }
  lost_topics_all <- c(lost_topics1, lost_topics2, lost_topics3, lost_topics4, lost_topics5)
  rm(lost_topics1, lost_topics2, lost_topics3, lost_topics4, lost_topics5)
  ties <-
    cbind.data.frame(period, new_topics_all, same_topics_all, lost_topics_all)
  
  # Calculations per time period
  for(j in period) {
    period.data <- subset(pub.list, period == j) 
    # Compute variables
    topics <- length(unique(period.data$cluster_id1))
    p <- length(unique(period.data$pub_id))
    p_abroad_period <- sum(period.data$foreign_country)
    p_home_period <- nrow(subset(period.data, foreign_country == 0))
    pubs.no.co <- 
      unique(
        period.data[, c(
          "pub_id", "int_collab", "n_cits", "cl_p", "cl_int", "cl_cits")])
    p_int_period <- sum(pubs.no.co$int_collab)
    t_cits_period <- sum(pubs.no.co$n_cits)
     cl_p_period <-
      ifelse(length(unique(pubs.no.co$cl_p)) == 1,
             pubs.no.co$cl_p,
             median(pubs.no.co$cl_p))
    cl_int_period <-
      ifelse(length(unique(pubs.no.co$cl_int)) == 1,
             pubs.no.co$cl_int,
             median(pubs.no.co$cl_int))
    cl_cits_period <- 
      ifelse(length(unique(pubs.no.co$cl_cits)) == 1,
             pubs.no.co$cl_cits,
             median(pubs.no.co$cl_cits))
    rm(pubs.no.co)
    period_val <- j
    same_topics <- ties[which(ties$period == j, arr.ind = T),3]
    new_topics <- ties[which(ties$period == j, arr.ind = T),2]
    lost_topics <- ties[which(ties$period == j, arr.ind = T),4]
    # Create vector
    v <-
      c(i,
        period_val,
        topics,
        p,
        p_abroad_period,
        p_home_period,
        p_int_period,
        t_cits_period,
        cl_p_period,
        cl_int_period,
        cl_cits_period,
        same_topics,
        new_topics,
        lost_topics)
    # Add to framework
    df2[nrow(df2) + 1,] <- v
    
  }
}

df2.str <- df2 # Fix format
df2.str[,-1] <- mutate_all(df2.str[,-1], function(x) as.numeric(x))
df2 <- df2.str
# Export data to Google Drive
write.csv(df2, file = "C:/Users/elrobinster/Downloads/df2.txt") # Add local path
drive_update(media = "C:/Users/elrobinster/Downloads/df2.txt", 
             path = as_id("1xIBEu0WX-Xq4aqbkM4C46RRDQoPO981r"))
