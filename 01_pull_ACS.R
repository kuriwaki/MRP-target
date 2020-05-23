library(tidyverse)
library(foreach)
library(glue)
library(ccesMRPprep)
library(tidycensus)

source("00_functions.R")


# Prep -----

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


pop_st <- foreach(y = c(2008, 2012, 2016), .combine = "bind_rows") %do% {
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


