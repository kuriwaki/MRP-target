library(tidyverse)
library(foreach)
library(glue)
library(tidycensus)

source("00_functions.R")

# Codes (currently duplicate with 02_format) ---

ages_regex  <- as.character(glue("({str_c(ages, collapse = '|')})"))
edu_regex   <- as.character(glue("({str_c(education, collapse = '|')})"))
races_regex <- as.character(glue("({str_c(races, collapse = '|')})"))

# Prep -----


# get vars ----
vars_raw <- load_variables(2017, "acs1")

vars <- vars_raw %>%
  mutate(variable = name) %>%
  separate(name, sep = "_", into = c("table", "num")) %>%
  select(variable, table, concept, num, label, everything()) %>%
  filter(str_detect(label, "Total")) %>%
  mutate(label = str_remove(label, "Estimate!!Total")) %>%
  mutate(gender = str_extract(label, "(Male|Female)"),
         age = str_extract(label, ages_regex),
         educ = str_extract(label, edu_regex),
         race = coalesce(str_extract(label, regex(races_regex, ignore_case = TRUE)),
                         str_extract(concept, regex(races_regex, ignore_case = TRUE))))

# partition vars to pull
edu_vars <- vars %>%
  filter(!is.na(gender), !is.na(age), !is.na(educ)) %>%
  filter(str_detect(table, "^B")) %>%
  pull(variable)

race_vars <- vars %>%
  filter(str_detect(table, "B01001[B-I]")) %>%
  filter(!is.na(gender), !is.na(age), !is.na(race)) %>%
  pull(variable)


# Easier ------

# Pull all data ----
pop_cd <- foreach(y = 2017, .combine = "bind_rows") %do% {
  get_acs(geography = "congressional district",
          year = y,
          survey = "acs1",
          variable = c(edu_vars, race_vars),
          geometry = FALSE) %>%
    mutate(year = y)
}  %>%
  std_acs() %>%
  mutate(
    cdid = GEOID,
    cd = cd_name(NAME),
    count = replace_na(count, 0)
  )


pop_st <- foreach(y = 2017, .combine = "bind_rows") %do% {
  get_acs(geography = "state",
          year = y,
          survey = "acs1",
          variable = c(edu_vars, race_vars),
          geometry = FALSE) %>%
    mutate(year = y)
}  %>%
  std_acs() %>%
  rename(
    stid = GEOID,
    state = NAME
  ) %>%
  mutate(count = replace_na(count, 0))

pop_us <- foreach(y = 2017, .combine = "bind_rows") %do% {
  get_acs(geography = "us",
          year = y,
          survey = "acs1",
          variable = c(edu_vars, race_vars),
          geometry = FALSE) %>%
    mutate(year = y)
} %>%
  std_acs()


write_rds(vars, "data/input/acs/variable-descriptions.Rds")
write_rds(pop_us, "data/input/acs/by-us_acs_counts.Rds")
write_rds(pop_st, "data/input/acs/by-st_acs_counts.Rds")
write_rds(pop_cd, "data/input/acs/by-cd_acs_counts.Rds")


