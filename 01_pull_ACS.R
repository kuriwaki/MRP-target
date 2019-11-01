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
races <- c("White alone",
           "Black or African American alone",
           "American Indian and Alaska Native alone",
           "Asian alone",
           "Native Hawaiian and Other Pacific Islander alone",
           "Some other race alone",
           "Two or more races" #,
           # "Two or more races!!Two races including Some other race",
           # "Two or more races!!Two races excluding Some other race, and three or more races"
)



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
vars_raw <- load_variables(2017, "acs1")

vars <- vars_raw %>%
  filter(str_detect(label, "Total")) %>%
  mutate(label = str_remove(label, "Estimate!!Total")) %>%
  mutate(gender = str_extract(label, "(Male|Female)"),
         age = str_extract(label, glue("({str_c(ages, collapse = '|')})")),
         educ = str_extract(label, glue("({str_c(education, collapse = '|')})")),
         race = str_extract(label, regex(glue("({str_c(races, collapse = '|')})"), ignore_case = TRUE)))

vars %>%
  count(race)

# select
demog_vars <- str_c("B15001_", str_pad(1:83, width = 3, side = "left", pad = "0"))
race_vars <- str_c("B03002_", str_pad(3:12, width = 3, side = "left", pad = "0")) # proper partition


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
          variable = c(str_c("B15001_", str_pad(1:83, width = 3, side = "left", pad = "0")),
                       c("B03002_012", str_c("B02001_", str_pad(2:10, width = 3, pad =  "0", side = "left")))),
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
          variable = c(str_c("B15001_", str_pad(1:83, width = 3, side = "left", pad = "0")),
                       c("B03002_012", str_c("B02001_", str_pad(2:10, width = 3, pad =  "0", side = "left")))),
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
          variable = c(str_c("B15001_", str_pad(1:83, width = 3, side = "left", pad = "0")),
                       c("B03002_012", str_c("B02001_", str_pad(2:10, width = 3, pad =  "0", side = "left")))),
          geometry = FALSE) %>%
    mutate(year = y)
} %>%
  std_acs()


write_rds(vars, "data/input/acs/variable-descriptions.Rds")
write_rds(pop_all, "data/input/acs/by-all_acs_counts.Rds")
write_rds(pop_st, "data/input/acs/by-st_acs_counts.Rds")
write_rds(pop_cd, "data/input/acs/by-cd_acs_counts.Rds")


n17 <-  get_acs(geography = "us",
                year = 2017,
                survey = "acs1",
                variable = c(race_vars, demog_vars))

n17 %>%
  filter(str_detect(variable, "B15001_001")) %>%
  arrange(-estimate) %>%
  pull(estimate) %>%
  sum()

pop_all %>%
  filter(str_detect(variable, "B02001"), year == 2017) %>%
  left_join(select(vars_raw, label, name), by = c("variable" = "name")) %>%
  pull(label) %>%
  dput()
