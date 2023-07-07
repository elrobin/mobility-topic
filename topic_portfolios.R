# Import all data
library(readr)
portfolios <-
  read_delim(
    "G:/Mi unidad/1. Work sync/Projects/2 In progress/2021_Cassidy/mobility-topics/June2023/portfolios_co_region_subregion.csv",
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )
countries <-
  read_delim(
    "G:/Mi unidad/1. Work sync/Projects/2 In progress/2021_Cassidy/mobility-topics/June2023/countries.csv",
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )
for_division <-
  read_delim(
    "G:/Mi unidad/1. Work sync/Projects/2 In progress/2021_Cassidy/mobility-topics/June2023/for_division.csv",
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE
  )

# Add totals to countries
library(dplyr)
portfolios <- portfolios %>%
  filter(group == "country") %>%
  inner_join(countries, by.x ="group", by.y = "country_code")
