library(tidyverse)
library(foreach)
library(glue)
library(tidycensus)


# each user needs to get a Census key. See ??census_api_key
# census_api_key("", install = TRUE)

# Prep -----
st_df <- tibble(st = state.abb, state = state.name) %>%
  add_row(st = "DC", state = "District of Columbia")

cd_name <- function(vec, st_to_state = st_df) {
  distnum <- vec %>% str_extract("([0-9]+|at Large)") %>%
    str_replace("at Large", "1")
  cong <- vec %>% str_extract("1[01][0-9]")
  states <- vec %>% str_extract("(?<=,\\s)[A-z\\s]+")
  st <- map_chr(states, function(x) st_to_state$st[x == st_to_state$state])

  return(as.character(glue("{st}-{distnum} ({cong})")))
}


# Pull all data ----

pop_all <- foreach(y = 2010:2017, .combine = "bind_rows") %do% {
  get_acs(geography = "congressional district",
               year = y,
               variable = str_c("B15001_", str_pad(1:83, width = 3, side = "left", pad = "0")),
               geometry = FALSE) %>%
    filter(!str_detect(NAME, "Puerto Rico")) %>%
    transmute(year = y,
           cdid = GEOID,
           cd = cd_name(NAME),
           variable,
           count = estimate,
           count_moe = moe)
}

# Recode variable df ------
ages  <- c("25 to 34 years",
           "45 to 64 years",
           "65 years and over",
           "18 to 24 years",
           "35 to 44 years")
education <- c("Graduate or professional degree",
               "Bachelor's degree",
               "Associate's degree",
               "Some college no degree",
               "High school graduate \\(includes equivalency\\)",
               "9th to 12th grade no diploma")

vars <- load_variables(2017, "acs5", cache = TRUE) %>%
  filter(str_detect(name, "B15001_")) %>%
  mutate(gender = str_extract(label, "(Male|Female)"),
         age = str_extract(label, glue("({str_c(ages, collapse = '|')})")),
         educ = str_extract(label, glue("({str_c(education, collapse = '|')})")))


# Clean dataset as the combination of the two ----
cell_vars <- vars %>%
  filter(!is.na(gender), !is.na(age), !is.na(educ)) %>%
  select(variable = name, gender, age, educ)

pop_frac <- pop_all %>%
  group_by(year, cdid, cd) %>%
  mutate(sum_dist = sum(count*(variable == "B15001_001"))) %>% # sum
  mutate(frac_dist = count / sum_dist) %>%
  ungroup() %>%
  inner_join(cell_vars, by = "variable") %>%
  select(year:variable, gender:educ, everything())

write_rds(pop_frac, "data/output/by-CD_ACS_gender-age-education.Rds")
