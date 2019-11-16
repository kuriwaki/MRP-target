library(tidyverse)
library(glue)
library(labelled)

source("00_functions.R")

vars <- read_rds("data/input/acs/variable-descriptions.Rds")
pop_all <- read_rds("data/input/acs/by-all_acs_counts.Rds")
pop_st <- read_rds("data/input/acs/by-st_acs_counts.Rds")
pop_cd <- read_rds("data/input/acs/by-cd_acs_counts.Rds")

cc18 <- read_rds("data/input/by-person_cces-2018.Rds")

# Recode variable df ------
ages5  <- c("18 to 24 years",
           "25 to 34 years",
           "35 to 44 years",
           "45 to 64 years",
           "65 years and over")
age5_lbl <- setNames(1:5L, ages5)
age5_key <- tibble(age_chr = ages5,
                  age = labelled(1:5L, age5_lbl))

ages10 <- c("18 and 19 years",
           "20 to 24 years",
           "25 to 29 years",
           "30 to 34 years",
           "35 to 44 years",
           "45 to 54 years",
           "55 to 64 years",
           "65 to 74 years",
           "75 to 84 years",
           "85 years and over")
age10_key <- tibble(age_chr = ages10) %>%
  mutate(age_num = case_when(
    age_chr %in% ages10[1:2] ~ 1L,
    age_chr %in% ages10[3:4] ~ 2L,
    age_chr %in% ages10[5] ~ 3L,
    age_chr %in% ages10[6:7] ~ 4L,
    age_chr %in% ages10[8:10] ~ 5L
  )) %>%
  mutate(age = labelled(age_num, age5_lbl))

# Education ----
educ_lbl <- setNames(1L:7L,
                     c("Less than 9",
                       "No HS",
                       "High School Graduate",
                       "Some College",
                       "2-Year",
                       "4-Year",
                       "Post-Grad"))
educ_lbl_clps <- setNames(1L:4L,
                          c("HS or Less", "Some College", "4-Year", "Post-Grad"))


## CCES lumps the first two, and let's also lump the 2-year
cces_edlbl <- tibble(cces_label = names(educ_lbl)[2:7],
                     educ = labelled(c(1, 1, 2, 2, 3, 4), educ_lbl_clps))

educ_key  <- tribble(
  ~num, ~educ_chr, ~cces_label,
  1L, "Less than 9th grade", "No HS",
  2L, "9th to 12th grade no diploma", "No HS",
  3L, "High school graduate (includes equivalency)", "High School Graduate",
  4L, "Some college no degree", "Some College",
  5L, "Associate's degree", "2-Year",
  6L, "Bachelor's degree", "4-Year",
  7L, "Graduate or professional degree", "Post-Grad"
  ) %>%
  left_join(cces_edlbl, by = "cces_label") %>%
  select(-num)

# Gender ----
gender_key <- tibble(gender_chr = c("Male", "Female"),
                     gender = labelled(1:2L, c(Male = 1, Female = 2)))

# Race -----
my_racelbl <- setNames(1L:5L, c("White", "Black", "Hispanic", "Asian", "All Other"))

race_cces_key <- distinct(cc18, race) %>%
  mutate(race_cces_chr = as.character(as_factor(race))) %>%
  rename(race_cces = race)

race_key <- tribble(
  ~race_5, ~race_cces_chr, ~race_acs,
  1L, "White", "WHITE ALONE, NOT HISPANIC OR LATINO",
  2L, "Black", "BLACK OR AFRICAN AMERICAN ALONE",
  3L, "Hispanic", "HISPANIC OR LATINO",
  4L, "Asian", "ASIAN ALONE",
  4L, "Asian", "NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE",
  5L, "Native American", "AMERICAN INDIAN AND ALASKA NATIVE ALONE",
  5L, "Mixed", "TWO OR MORE RACES",
  5L, "Other", "SOME OTHER RACE ALONE",
  5L, "Middle Eastern", NA,
) %>%
  left_join(race_cces_key, by = "race_cces_chr") %>%
  mutate(race = labelled(race_5, my_racelbl))
race_key


# Clean dataset as the combination of the two ----
educ_cells <- vars %>%
  filter(!is.na(gender), !is.na(age), !is.na(educ)) %>%
  rename(gender_chr = gender, age_chr = age, educ_chr = educ) %>%
  left_join(gender_key, by = "gender_chr") %>%
  left_join(age5_key, by = "age_chr") %>%
  left_join(educ_key, by = "educ_chr") %>%
  select(variable, gender, age, educ)

race_cells <- vars %>%
  filter(!is.na(gender), !is.na(age), !is.na(race)) %>%
  rename(gender_chr = gender, age_chr = age, race_acs = race) %>%
  left_join(gender_key, by = "gender_chr") %>%
  left_join(age10_key, by = "age_chr") %>%
  left_join(select(race_key, race_acs, race), by = "race_acs") %>%
  select(variable, gender, age, race, race_acs)

clean_strat <- function(grp_tab, geo, codes = cell_vars) {
  stopifnot(is.grouped_df(grp_tab)) # grouped by vars for geo

  # grouping vars
  if ("educ" %in% colnames(codes))
    groups <- c("gender", "age", "educ")
  if ("race" %in% colnames(codes))
    groups <- c("gender", "age", "race")

  groups <- syms(groups)

  # add cces labels
  keyd_tab <- grp_tab %>%
    select(variable, year, count) %>%
    inner_join(codes)

  sum_geo <- keyd_tab %>%
    summarize(count_geo = sum(count))

  keyd_tab %>%
    group_by(!!!groups, add = TRUE) %>%  # drop variable code (let our labels preceed)
    summarize(count = sum(count)) %>% # count by somewhat coarser category
    ungroup() %>%
    left_join(sum_geo) %>%  # add totals
    mutate(acs_frac = count / count_geo) %>%
    filter(!is.na(acs_frac)) %>%
    mutate(geo = geo) %>%
    select(year, geo, matches("(st|cd)"), gender, age, matches("race|educ"), everything())
}


us_educ_frac <- group_by(pop_st, year, variable) %>%
  summarize(count = sum(count)) %>%
  clean_strat("us", educ_cells)

us_race_frac <- group_by(pop_st, year, variable) %>%
  summarize(count = sum(count, na.rm = TRUE)) %>%
  clean_strat("us", race_cells)

st_race_frac <- clean_strat(group_by(pop_st, year, stid, state), "st", race_cells)
st_educ_frac <- clean_strat(group_by(pop_st, year, stid, state), "st", educ_cells)

cd_race_frac <- clean_strat(group_by(pop_cd, year, cdid, cd), "cd", race_cells)
cd_educ_frac <- clean_strat(group_by(pop_cd, year, cdid, cd), "cd", educ_cells)


# Save --
write_rds(us_educ_frac, "data/output/by-us_ACS_gender-age-education.Rds")
write_rds(cd_educ_frac, "data/output/by-cd_ACS_gender-age-education.Rds")
write_rds(st_educ_frac, "data/output/by-st_ACS_gender-age-education.Rds")
write_rds(us_race_frac, "data/output/by-us_ACS_gender-age-race.Rds")
write_rds(cd_race_frac, "data/output/by-cd_ACS_gender-age-race.Rds")
write_rds(st_race_frac, "data/output/by-st_ACS_gender-age-race.Rds")

save(age5_key, age10_key, gender_key, educ_key, race_key,
     file = "data/output/variable-labels.Rdata")
