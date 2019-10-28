library(tidyverse)
library(foreach)
library(glue)
library(tidycensus)

# Codes (currently duplicate with 02_format) ---
ages  <- c("18 to 24 years",
           "25 to 34 years",
           "35 to 44 years",
           "45 to 64 years",
           "65 years and over")
education <- c("Less than 9th grade",
               "9th to 12th grade, no diploma",
               "High school graduate \\(includes equivalency\\)",
               "Some college, no degree",
               "Associate's degree",
               "Bachelor's degree",
               "Graduate or professional degree")



# Prep -----
st_df <- tibble(st = state.abb, state = state.name) %>%
  add_row(st = "DC", state = "District of Columbia")

cd_name <- function(vec, st_to_state = st_df) {
  distnum <- vec %>% str_extract("([0-9]+|at Large)") %>%
    str_replace("at Large", "1")
  cong <- vec %>% str_extract("1[01][0-9]")
  states <- vec %>% str_extract("(?<=,\\s)[A-z\\s]+")
  st <- map_chr(states, function(x) st_to_state$st[x == st_to_state$state])

  return(as.character(glue("{st}-{distnum}")))
}

# get vars ----
vars <- load_variables(2017, "acs5") %>%
  filter(str_detect(label, "Total")) %>%
  mutate(label = str_remove(label, "Estimate!!Total")) %>%
  mutate(gender = str_extract(label, "(Male|Female)"),
         age = str_extract(label, glue("({str_c(ages, collapse = '|')})")),
         educ = str_extract(label, glue("({str_c(education, collapse = '|')})")))

vars %>% filter(str_detect(label, regex("education", ignore_case = TRUE))) %>%
  count(concept)

# Pull all data ----

std_acs <- function(df) {
  df %>%
    filter(!str_detect(NAME, "Puerto Rico")) %>%
    rename(count = estimate,
           count_moe = moe)
}

pop_cd <- foreach(y = 2012:2017, .combine = "bind_rows") %do% {
  get_acs(geography = "congressional district",
          year = y,
          survey = "acs1",
          variable = str_c("B15001_", str_pad(1:83, width = 3, side = "left", pad = "0")),
          geometry = FALSE) %>%
    mutate(year = y)
}  %>%
  std_acs() %>%
  transmute(
    year, variable,
    cdid = GEOID,
    cd = cd_name(NAME),
    count, count_moe
  )


pop_st <- foreach(y = 2012:2017, .combine = "bind_rows") %do% {
  get_acs(geography = "state",
          year = y,
          survey = "acs1",
          variable = str_c("B15001_", str_pad(1:83, width = 3, side = "left", pad = "0")),
          geometry = FALSE) %>%
    mutate(year = y)
}  %>%
  std_acs() %>%
  transmute(
    year, variable,
    stid = GEOID,
    state = NAME,
    count, count_moe
  )

pop_all <- foreach(y = 2012:2017, .combine = "bind_rows") %do% {
  get_acs(geography = "us",
          year = y,
          survey = "acs1",
          variable = str_c("B15001_", str_pad(1:83, width = 3, side = "left", pad = "0")),
          geometry = FALSE) %>%
    mutate(year = y)
} %>%
  std_acs()