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

  return(as.character(glue("{st}-{distnum}")))
}


# Pull all data ----

pop_cd <- foreach(y = 2012:2017, .combine = "bind_rows") %do% {
  get_acs(geography = "congressional district",
          year = y,
          survey = "acs1",
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

pop_st <- foreach(y = 2012:2017, .combine = "bind_rows") %do% {
  get_acs(geography = "state",
          year = y,
          survey = "acs1",
          variable = str_c("B15001_", str_pad(1:83, width = 3, side = "left", pad = "0")),
          geometry = FALSE) %>%
    filter(!str_detect(NAME, "Puerto Rico")) %>%
    transmute(year = y,
           stid = GEOID,
           state = NAME,
           variable,
           count = estimate,
           count_moe = moe)
}

# Recode variable df ------
ages  <- c("18 to 24 years",
           "25 to 34 years",
           "35 to 44 years",
           "45 to 64 years",
           "65 years and over")
age_lbl <- setNames(1:5L, ages)
age_key <- tibble(1:5L,
                  age_chr = ages,
                  age = labelled(1:5L, age_lbl))

education <- c("9th to 12th grade no diploma",
               "High school graduate \\(includes equivalency\\)",
               "Some college no degree",
               "Associate's degree",
               "Bachelor's degree",
               "Graduate or professional degree")

educ_lbl <- setNames(1:6L,
              c("No HS", "High School Graduate", "Some College",
              "2-Year", "4-Year", "Post-Grad"))
educ_key  <- tibble(num = 1:6L,
                    education = replace(education,  num ==  2L, "High school graduate (includes equivalency)"),
                    educ_chr = names(educ_lbl),
                    educ = labelled(1:6L, educ_lbl))

gender_key <- tibble(num = 1:2L,
                     gender_chr = c("Male", "Female"),
                     gender = labelled(1:2L, c(Male = 1, Female = 2)))


# get vars ----
vars <- load_variables(2017, "acs1", cache = TRUE) %>%
  filter(str_detect(name, "B15001_")) %>%
  mutate(gender = str_extract(label, "(Male|Female)"),
         age = str_extract(label, glue("({str_c(ages, collapse = '|')})")),
         educ = str_extract(label, glue("({str_c(education, collapse = '|')})")))


# Clean dataset as the combination of the two ----
cell_vars <- vars %>%
  filter(!is.na(gender), !is.na(age), !is.na(educ)) %>%
  select(variable = name, gender, age, educ)

clean_strat <- function(grp_tab, codes = cell_vars) {
  stopifnot(is.grouped_df(grp_tab))

  grp_tab %>%
    mutate(sum_dist = sum(count*(variable == "B15001_001"))) %>% # sum
    mutate(frac_dist = count / sum_dist) %>%
    ungroup() %>%
    inner_join(cell_vars, by = "variable") %>%
    rename(education = educ, age_chr = age, gender_chr = gender) %>%
    mutate(education = str_replace_all(education, "\\(includes equivalency\\)", "(includes equivalency)")) %>%
    inner_join(select(educ_key, education, educ), by = "education") %>%
    inner_join(select(age_key, age_chr, age), by = "age_chr") %>%
    inner_join(select(gender_key, gender_chr, gender), by = "gender_chr") %>%
    select(-education, -age_chr, -gender_chr) %>%
    select(year:variable, gender, age, educ, everything())
}

all_frac <- group_by(pop_st, year, variable) %>%
  summarize(count = sum(count)) %>%
  clean_strat()

st_frac <- clean_strat(group_by(pop_st, year, stid, state))
cd_frac <- clean_strat(group_by(pop_cd, year, cdid, cd))

write_rds(all_frac, "data/output/by-national_ACS_gender-age-education.Rds")
write_rds(cd_frac, "data/output/by-CD_ACS_gender-age-education.Rds")
write_rds(st_frac, "data/output/by-ST_ACS_gender-age-education.Rds")
save(age_key, gender_key, educ_key, educ_lbl, age_lbl,
     file = "data/output/variable-labels.Rdata")
