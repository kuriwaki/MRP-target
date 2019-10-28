library(tidyverse)
library(glue)
library(labelled)


# each user needs to get a Census key. See ??census_api_key
# census_api_key("", install = TRUE)



# Recode variable df ------
ages  <- c("18 to 24 years",
           "25 to 34 years",
           "35 to 44 years",
           "45 to 64 years",
           "65 years and over")
age_lbl <- setNames(1:5L, ages)
age_key <- tibble(age_chr = ages,
                  age = labelled(1:5L, age_lbl))

education <- c("Less than 9th grade",
               "9th to 12th grade, no diploma",
               "High school graduate \\(includes equivalency\\)",
               "Some college, no degree",
               "Associate's degree",
               "Bachelor's degree",
               "Graduate or professional degree")

educ_lbl <- setNames(1L:7L,
              c("Less than 9", "No HS", "High School Graduate", "Some College",
              "2-Year", "4-Year", "Post-Grad"))
educ_lbl_cces <- setNames(1L:6L, names(educ_lbl)[2:7])

# CCES lumps the first two
cces_lbl <- tibble(cces_label = names(educ_lbl)[2:7],
                   educ = labelled(1:6L, educ_lbl_cces))

educ_key  <- tibble(num = 1:7L,
                    educ_chr = replace(education, num ==  3L, "High school graduate (includes equivalency)"),
                    educ_fct = factor(names(educ_lbl), levels = names(educ_lbl))) %>%
  mutate(cces_label = as.character(fct_collapse(educ_fct, `No HS` = c("Less than 9", "No HS")))) %>%
  left_join(cces_lbl, by = "cces_label") %>%
  select(-num)

gender_key <- tibble(gender_chr = c("Male", "Female"),
                     gender = labelled(1:2L, c(Male = 1, Female = 2)))



# Clean dataset as the combination of the two ----
cell_vars <- vars %>%
  filter(!is.na(gender), !is.na(age), !is.na(educ)) %>%
  rename(variable = name, gender_chr = gender, age_chr = age, educ_chr = educ) %>%
  left_join(gender_key, by = "gender_chr") %>%
  left_join(age_key, by = "age_chr") %>%
  left_join(educ_key, by = "educ_chr") %>%
  select(variable, gender, age, educ)

clean_strat <- function(grp_tab, geo, codes = cell_vars) {
  stopifnot(is.grouped_df(grp_tab)) # grouped by vars for geo

  grp_tab %>%
    mutate(count_geo = sum(count*(variable == "B15001_001"))) %>% # sum geo
    inner_join(codes, by = "variable") %>% # drop marginal cells, so cells form partition
    group_by(year, gender, age, educ, count_geo, add = TRUE) %>%  # drop variable code (let our labels preceed)
    summarize(count = sum(count)) %>%
    ungroup() %>%
    mutate(frac_geo = count / count_geo) %>%
    mutate(geo = geo) %>%
    select(year, geo, matches("(st|cd)"), gender, age, educ, everything())
}


all_frac <- group_by(pop_st, year, variable) %>%
  summarize(count = sum(count)) %>%
  clean_strat("nat")
us_frac <- pop_all %>%
  group_by(year) %>%
  clean_strat("nat")
st_frac <- clean_strat(group_by(pop_st, year, stid, state), "st")
cd_frac <- clean_strat(group_by(pop_cd, year, cdid, cd), "cd")


# Save --
write_rds(all_frac, "data/output/by-national_ACS_gender-age-education.Rds")
write_rds(cd_frac, "data/output/by-CD_ACS_gender-age-education.Rds")
write_rds(st_frac, "data/output/by-ST_ACS_gender-age-education.Rds")
save(age_key, gender_key, educ_key, educ_lbl, age_lbl,
     file = "data/output/variable-labels.Rdata")
