library(tidyverse)
library(haven)
library(iterake)
library(foreach)


# data ----
cc18_uw <- read_rds("data/input/by-person_cces-2018.Rds")

st_frac_educ <- read_rds("data/output/by-st_ACS_gender-age-education.Rds")
st_frac_race <- read_rds("data/output/by-st_ACS_gender-age-race.Rds")


# States ---------
# year
targets_st <- st_frac_educ %>%
  filter(year == 2017)
u_states <- unique(targets_st$state)
u_stids  <- unique(targets_st$stid)

# state partitioned weights -----
tgt_st_educ <- st_frac_educ %>%
  filter(year == 2017)
tgt_st_race <- st_frac_race %>%
  filter(year == 2017)
u_states <- unique(targets_st$state)
u_stids  <- unique(targets_st$stid)




# subset at each state, and go state by state ---
large_states <- c("California", "Texas", "Florida", "New York", "Pennsylvania")

st_par_w <- foreach(st_i = u_states, .combine = "bind_rows") %do% {

  # subset survey sample to state
  cc_i <- filter(cc18_uw, state == st_i) %>%
    mutate_if(is.labelled, zap_labels) %>%
    rename(sex = gender, edu = educ, rac = race) %>%
    mutate(sex_age = str_c(sex, "_", age),
           sex_edu = str_c(sex, "_", edu),
           sex_rac = str_c(sex, "_", rac),
           age_edu = str_c(age, "_", edu),
           age_rac = str_c(age, "_", rac))

  # subset target distributions to state
  tgt_edu_i <- tgt_st_educ %>%
    filter(state == st_i) %>%
    mutate_if(is.labelled, zap_labels) %>%
    rename(sex = gender, edu = educ)
  tgt_rac_i <- tgt_st_race %>%
    filter(state == st_i) %>%
    mutate_if(is.labelled, zap_labels) %>%
    rename(sex = gender, rac = race)

  # tab marginal distribtions of the target
  prop_sex <- tgt_edu_i %>%
    group_by(sex) %>%
    summarize(count = sum(count))

  prop_age <- tgt_edu_i %>%
    group_by(age) %>%
    summarize(count = sum(count))

  prop_edu <- tgt_edu_i %>%
    group_by(edu) %>%
    summarize(count = sum(count))

  prop_rac <- tgt_rac_i %>%
    group_by(rac) %>%
    summarize(count = sum(count))

  # tab joint distributions of the target
  prop_sex_age <- tgt_edu_i %>%
    group_by(sex, age) %>%
    summarize(count = sum(count)) %>%
    unite(sex_age, sex, age)

  prop_sex_edu <- tgt_edu_i %>%
    group_by(sex, edu) %>%
    summarize(count = sum(count)) %>%
    unite(sex_edu, sex, edu)

  prop_sex_rac <- tgt_rac_i %>%
    group_by(sex, rac) %>%
    summarize(count = sum(count)) %>%
    unite(sex_rac, sex, rac)

  prop_age_edu <- tgt_edu_i %>%
    group_by(age, edu) %>%
    summarize(count = sum(count)) %>%
    unite(age_edu, age, edu)

  prop_age_rac <- tgt_rac_i %>%
    group_by(age, rac) %>%
    summarize(count = sum(count)) %>%
    unite(age_rac, age, rac)

  # adapt target if there's zero people of that race
  race_i <- count(cc_i, rac)
  prop_rac <-  semi_join(prop_rac, race_i, by = "rac")

  # if small state, only match on marginals
  if (!st_i %in% large_states) {
  uni <- universe(
    data = cc_i,
    category(name = "sex", prop_sex$sex, targets = prop_sex$count, sum.1 = TRUE),
    category(name = "age", prop_age$age, targets = prop_age$count, sum.1 = TRUE),
    category(name = "edu", prop_edu$edu, targets = prop_edu$count, sum.1 = TRUE),
    category(name = "rac", prop_rac$rac, targets = prop_rac$count, sum.1 = TRUE)
    )
  }

  # if large state, can match on joints?
  if (st_i %in% large_states) {

    # adapt target if there's zero people of that category
    race_i <- count(cc_i, rac)
    prop_rac <-  semi_join(prop_rac, race_i, by = "rac")

    uni <- universe(
      data = cc_i,
      category(name = "sex", prop_sex$sex, targets = prop_sex$count, sum.1 = TRUE),
      category(name = "age", prop_age$age, targets = prop_age$count, sum.1 = TRUE),
      category(name = "edu", prop_edu$edu, targets = prop_edu$count, sum.1 = TRUE),
      category(name = "rac", prop_rac$rac, targets = prop_rac$count, sum.1 = TRUE),
      category(name = "sex_age", prop_sex_age$sex_age, targets = prop_sex_age$count, sum.1 = TRUE),
      category(name = "sex_edu", prop_sex_edu$sex_edu, targets = prop_sex_edu$count, sum.1 = TRUE),
      category(name = "sex_rac", prop_sex_rac$sex_rac, targets = prop_sex_rac$count, sum.1 = TRUE),
      category(name = "age_edu", prop_age_edu$age_edu, targets = prop_age_edu$count, sum.1 = TRUE),
      category(name = "age_rac", prop_age_rac$age_rac, targets = prop_age_rac$count, sum.1 = TRUE)
    )
  }

  if (st_i %in% large_states) cat(st_i, "....\n")
  df_wgt <- iterake(universe = uni,
                    max.iter = 100,
                    max.wgt = 10,
                    wgt.name = "weight_st",
                    summary = st_i %in% large_states)

  select(df_wgt, case_id, weight, weight_st)
}



write_rds(st_par_w, "data/output/weights-state.Rds")


