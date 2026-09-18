library(tidyverse)
library(tidycensus)

API_key <- Sys.getenv("CENSUS_API_KEY")
census_api_key(API_key, install = TRUE)


y2022 <- load_variables(2022, "acs5", cache = TRUE)

View(y2022)

# Helper function to get 18–29 totals for a given table
get_18_29 <- function(table_prefix) {
  vars <- c(
    paste0(table_prefix, "_007"), # male 18–19
    paste0(table_prefix, "_008"), # male 20-24
    paste0(table_prefix, "_009"), # male 25-29
    paste0(table_prefix, "_022"), # female 18–19
    paste0(table_prefix, "_023"), # female 20-24
    paste0(table_prefix, "_024") # female 25-29
  )
  
  get_acs(
    geography = "us",
    variables = vars,
    year = 2022,
    survey = "acs5"
  ) %>%
    summarise(total = sum(estimate)) %>%
    pull(total)
}

white_vars <- c(
  male_18_19 = "B01001A_007",
  male_20_24 = "B01001A_008",
  male_25_29 = "B01001A_009",
  female_18_19 = "B01001A_022",
  female_20_24 = "B01001A_023",
  female_25_29 = "B01001A_024"
)

white_estimates <- get_acs(
  geography = "us",
  variables = white_vars,
  year = 2022,
  survey = "acs5"
)  %>% mutate( race = "white")


# Race table prefixes
race_tables <- c(
  white = "B01001A",
  black = "B01001B",
  aian = "B01001C",
  asian = "B01001D",
  nhpi = "B01001E",
  other = "B01001F",
  mixed = "B01001G"
)


# Race_hispanic table prefixes
race_hispanic_tables <- c(
  white_nh = "B01001H",
  hispanic = "B01001I",
  black_nh = "B01001B",
  aian = "B01001C",
  asian = "B01001D",
  nhpi = "B01001E",
  other = "B01001F",
  mixed = "B01001G"
)

# Pull all races
race_totals <- map_dbl(race_tables, get_18_29)

# Convert to dataframe
race_df <- tibble(
  race = names(race_totals),
  total = race_totals) %>%
  mutate(
  race = ifelse(race %in% c("aian", "nhpi"), "other", race)
  ) %>%
  group_by(race) %>%
  summarise(total = sum(total)) %>%
  ungroup() %>%
  mutate(percent = round(total / sum(total)*100, 1),
         n = 1000 * total / sum(total))

race_df
