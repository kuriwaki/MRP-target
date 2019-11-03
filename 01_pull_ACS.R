library(tidyverse)
library(foreach)
library(glue)
library(tidycensus)

# Codes (currently duplicate with 02_format) ---
ages  <- c("18 to 24 years",
           "25 to 34 years",
           "35 to 44 years",
           "45 to 64 years",
           "65 years and over",
           "18 and 19 years",
           "20 to 24 years",
           "25 to 29 years",
           "30 to 34 years",
           "35 to 44 years",
           "45 to 54 years",
           "55 to 64 years",
           "65 to 74 years",
           "75 to 84 years",
           "85 years and over")
education <- c("Less than 9th grade",
               "9th to 12th grade, no diploma",
               "High school graduate \\(includes equivalency\\)",
               "Some college, no degree",
               "Associate's degree",
               "Bachelor's degree",
               "Graduate or professional degree")
races <- c("White alone, not Hispanic or Latino",
           "Hispanic or Latino",
           "Black or African American alone",
           "American Indian and Alaska Native alone",
           "Asian alone",
           "Native Hawaiian and Other Pacific Islander alone",
           "Some other race alone",
           "Two or more races" #,
           # "Two or more races!!Two races including Some other race",
           # "Two or more races!!Two races excluding Some other race, and three or more races"
)

ages_regex  <- as.character(glue("({str_c(ages, collapse = '|')})"))
edu_regex   <- as.character(glue("({str_c(education, collapse = '|')})"))
races_regex <- as.character(glue("({str_c(races, collapse = '|')})"))



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
std_acs <- function(tbl, var_df = vars) {
  std_df <- tbl %>%
    filter(!str_detect(NAME, "Puerto Rico")) %>%
    rename(count = estimate,
           count_moe = moe)

  inner_join(var_df, std_df, by = "variable") %>%
    select(year, everything())
}

# Pull all data ----
pop_cd <- foreach(y = 2012:2017, .combine = "bind_rows") %do% {
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


pop_st <- foreach(y = 2012:2017, .combine = "bind_rows") %do% {
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

pop_all <- foreach(y = 2012:2017, .combine = "bind_rows") %do% {
  get_acs(geography = "us",
          year = y,
          survey = "acs1",
          variable = c(edu_vars, race_vars),
          geometry = FALSE) %>%
    mutate(year = y)
} %>%
  std_acs()


write_rds(vars, "data/input/acs/variable-descriptions.Rds")
write_rds(pop_all, "data/input/acs/by-all_acs_counts.Rds")
write_rds(pop_st, "data/input/acs/by-st_acs_counts.Rds")
write_rds(pop_cd, "data/input/acs/by-cd_acs_counts.Rds")


ky5 <- get_acs(geography = "congressional district",
               state = "KY",
               year = 2017,
               survey = "acs1",
               variable = c(race_vars),
               geometry = FALSE) %>%
  mutate(
    cdid = GEOID,
    cd = cd_name(NAME)
    # count = replace_na(count, 0)
  )

view(ky5)

