library(tidyverse)
library(glue)
library(labelled)
library(rcces)

source("00_functions.R")

vars <- read_rds("data/input/acs/variable-descriptions.Rds")
pop_all <- read_rds("data/input/acs/by-all_acs_counts.Rds")
pop_st <- read_rds("data/input/acs/by-st_acs_counts.Rds")
pop_cd <- read_rds("data/input/acs/by-cd_acs_counts.Rds")



# Clean dataset as the combination of the two ----
educ_cells <- vars %>%
  filter(!is.na(gender), !is.na(age), !is.na(educ)) %>%
  rename(gender_chr = gender, age_chr = age, educ_chr = educ) %>%
  left_join(gender_key, by = "gender_chr") %>%
  left_join(age5_key, by = "age_chr") %>%
  left_join(educ_key2, by = "educ_chr") %>%
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


