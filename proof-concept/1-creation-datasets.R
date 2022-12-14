# Load libraries
library(readr)
library(dplyr)

# Import data
pubs <- read_delim("G:/Mi unidad/1. Work sync/Projects/2 In progress/2021_Cassidy/proof_of_concept/pubs.txt", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE)
researchers <- read_delim("G:/Mi unidad/1. Work sync/Projects/2 In progress/2021_Cassidy/proof_of_concept/researchers.txt", 
                   delim = "\t", escape_double = FALSE, 
                   trim_ws = TRUE)

clusters <- read_delim("G:/Mi unidad/1. Work sync/Projects/2 In progress/2021_Cassidy/proof_of_concept/clusters.txt", 
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
  age <- max(c.pubs.list$pub_year) - min(c.pubs.list$pub_year)
  total_topics <- length(unique(c.pubs.list$cluster_id1))
  total_p <- length(c.pubs.list$pub_id)
  p_abroad <- sum(c.pubs.list$foreign_country)
  p_home <- nrow(subset(c.pubs.list, foreign_country == 0))
  pubs.no.co <- unique(c.pubs.list[, c("pub_id", "int_collab", "n_cits")])
  p_int <- sum(pubs.no.co$int_collab)
  t_cits <- sum(pubs.no.co$n_cits)
  rm(pubs.no.co)
  cl_p <- median(c.pubs.list$p)
  cl_int <- median(c.pubs.list$p_int_collab)
  cl_cits <- median(c.pubs.list$t_cits)
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

############ Dataset 2 - Researcher-Period ############ 

# Only work with researchers with at least 3 publications
f.res.list <- subset(df1, total_p>2)

f.res.list <-
  merge(f.res.list, researchers, by = "researcher_id", all.x = T)
fakelist <- f.res.list[1:3,]

# Create empty dataframe
df2 <- data.frame(
  "researcher_id" = character(),
  "period" = character(),
  "topics" = double(),
  "p" = double(),
  "tp_abroad" = double(),
  "tp_home" = double(),
  "tp_int" = double(),
  "tt_cits" = double(),
  "tcl_p" = double(),
  "tcl_int" = double(),
  "tcl_cits" = double()
)
period <- c("T1", "T2", "T3", "T4", "T5")

# Create periods
for(i in fakelist$researcher_id){
  pub.list <- subset(pubs, researcher_id == i)
  pub.list <- 
    merge(pub.list, fakelist, by = "researcher_id", all.x = T)
  pub.list$period <- ifelse(
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
  )
  # Add cluster info
  c.pubs.list <- merge(pub.list, clusters, by ="cluster_id1", all.x = T)
  rm(pub.list)

  # Calculations per time period
  for(j in period) {
    period.data <- subset(c.pubs.list, period == j) 
    # Compute variables
    topics <- length(unique(period.data$cluster_id1))
    p <- length(period.data$pub_id)
    tp_abroad <- sum(period.data$foreign_country)
    tp_home <- nrow(subset(period.data, foreign_country == 0))
    pubs.no.co <- unique(period.data[, c("pub_id", "int_collab", "n_cits")])
    tp_int <- sum(pubs.no.co$int_collab)
    tt_cits <- sum(pubs.no.co$t_cits.x)
    rm(pubs.no.co)
    tcl_p <- median(period.data$p)
    tcl_int <- median(period.data$p_int_collab)
    tcl_cits <- median(period.data$t_cits.y)
    # Create vector
    v <-
      c(i,
        j,
        topics,
        p,
        tp_abroad,
        tp_home,
        tp_int,
        tt_cits,
        tcl_p,
        tcl_int,
        tcl_cits)
    # Add to framework
    df2[nrow(df2) + 1,] <- v
    
  }
  # Compute churning vars
  t1df <- subset(c.pubs.list, period == "T1")
  T1 <- unique(t1df$cluster_id1)
  t2df <- subset(c.pubs.list, period == "T2")
  T2 <- unique(t2df$cluster_id1)
  t3df <- subset(c.pubs.list, period == "T3")
  T3 <- unique(t3df$cluster_id1)
  t4df <- subset(c.pubs.list, period == "T4")
  T4 <- unique(t4df$cluster_id1)
  t5df <- subset(c.pubs.list, period == "T5")
  T5 <- unique(t5df$cluster_id1)
  #remove unnecessary df
  rm(list = c("t1df", "t2df", "t3df", "t4df", "t5df"))
  # Create vector which will be 
#   v1 m_cl Maintained topics
  m_cl1 <- -1
  if (length(T2) == 0) {
    m_cl2 <- -1
  } else {
    m_cl2 <- length(intersect(T1, T2))
  }
  if (length(T2)==0 | length(T3) == 0) {
    m_cl3 <- -1
  } else {
    m_cl3 <- length(intersect(T2, T3))
  }
  if (length(T4)==0 | length(T3) == 0) {
    m_cl4 <- -1
  } else{
    m_cl4 <- length(intersect(T3, T4))
  }
  if (length(T4)==0 | length(T5) == 0) {
    m_cl5 <- -1
  } else {
    m_cl5 <- length(intersect(T4, T5))
  }
  m_cl <- c(m_cl1, m_cl2, m_cl3, m_cl4, m_cl5)
  rm(m_cl1, m_cl2, m_cl3, m_cl4, m_cl5)
#   v2 n_cl New topics
  n_cl1 <- -1
  if (length(T2) == 0) {
    n_cl2 <- -1
  } else{
    n_cl2 <-
      length(T2) - length(intersect(T1, T2))
  }
  if (length(T2)==0 | length(T3) == 0) {
    n_cl3 <- -1
  } else{
    n_cl3 <- length(T3)-length(intersect(T2, T3))
  }
  if (length(T4)==0 | length(T3) == 0) {
    n_cl4 <- -1
  } else{
    n_cl4 <- length(T4)-length(intersect(T3, T4))
  }
  if (length(T4)==0 | length(T5) == 0) {
    n_cl5 <- -1
  } else {
    n_cl5 <- length(T5)-length(intersect(T4, T5))
  }
  n_cl <- c(n_cl1, n_cl2, n_cl3, n_cl4, n_cl5)
  rm(n_cl1, n_cl2, n_cl3, n_cl4, n_cl5)
  #   v3 d_cl Dissolved ties
  d_cl1 <- -1
  if (length(T2) == 0) {
    d_cl2 <- -1
  } else {
    d_cl2 <-
      length(T1) - length(intersect(T1, T2))
  }
  if (length(T3) == 0 | length(T2)==0) {
    d_cl3 <- -1
  } else {
    d_cl3 <-
      length(T2) - length(intersect(T2, T3))
  }
  if (length(T4) == 0 | length(T3)==0) {
    d_cl4 <- -1
  } else {
    d_cl4 <-
      length(T3) - length(intersect(T3, T4))
  }
  if (length(T4)==0 | length(T5) == 0) {
    d_cl5 <- -1
  } else {
    d_cl5 <-
      length(T4) - length(intersect(T4, T5))
  }
  d_cl <- c(d_cl1, d_cl2, d_cl3, d_cl4, d_cl5)
  rm(d_cl1, d_cl2, d_cl3, d_cl4, d_cl5)
  #   v4 s_cl Dormant ties
  s_cl1 <- -1
  s_cl2 <- -1
  if(length(T2)==0 | length(T3)==0) {
    s_cl3 <- -1
  } else {
    s_cl3 <- length(intersect(T1, T3))
  }
  if(length(T1)==0 | length(T2)==0 | length(T4)==0) {
    s_cl4 <- -1
  } else {
    T1T2 <- unique(c(T1, T2))
    s_cl4 <- length(intersect(T1T2, T4))
  }
  if(length(T1)==0 | length(T2)==0 | length(T3)==0  | length(T5)==0) {
    s_cl5 <- -1
  } else {
    T1T2T3 <- unique(c(T1T2, T3))
    s_cl5 <- length(intersect(T1T2T3, T5))
  }
  s_cl <- c(s_cl1, s_cl2, s_cl3, s_cl4, s_cl5)
  rm(s_cl1, s_cl2, s_cl3, s_cl4, s_cl5)
  # Add columns to dataframe
  df2$m_cl <- m_cl
  df2$n_cl <- n_cl ## OJO ## Aquí hay que restar al final los s_cl para que sean nuevos de verdad
  df2$d_cl <- d_cl
  df2$s_cl <- s_cl
 }
