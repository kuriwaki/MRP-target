library(tidyverse)
library(iterake)
library(foreach)

# data ----
cc18_uw <- read_rds("data/input/by-person_cces-2018.Rds")

st_frac_educ <- read_rds("data/output/by-st_ACS_gender-age-education.Rds")
st_frac_race <- read_rds("data/output/by-st_ACS_gender-age-race.Rds")

# cd_clinton <- gs_read(gs_title("wasserman_2018-2016_CD")) %>%
  # mutate(clinton_2pty = (margin_clinton + 1)/2)

data(g2016)

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

st_par_w <- foreach(st_i = u_states, .combine = "bind_rows") %do% {

  # subset survey sample to state
  cc_i <- filter(cc18_uw, state == st_i) %>% mutate_if(is.labelled, zap_labels)

  # subset target distributions to state
  tgt_edu_i <- tgt_st_educ %>%
    filter(state == st_i) %>%
    mutate_if(is.labelled, zap_labels)
  tgt_race_i <- tgt_st_race %>%
    filter(state == st_i) %>%
    mutate_if(is.labelled, zap_labels)

  # tab marginal distribtions of the target
  prop_sex <- tgt_edu_i %>%
    group_by(gender) %>%
    summarize(count = sum(count))

  prop_age <- tgt_edu_i %>%
    group_by(age) %>%
    summarize(count = sum(count))

  prop_edu <- tgt_edu_i %>%
    group_by(educ) %>%
    summarize(count = sum(count))

  prop_rac <- tgt_race_i %>%
    group_by(race) %>%
    summarize(count = sum(count))

  # adapt target if there's zero people of that race
  race_i <- count(cc_i, race)
  prop_rac <-  semi_join(prop_rac, race_i, by = "race")

  uni <- universe(
    data = cc_i,
    category(name = "gender", prop_sex$gender, targets = prop_sex$count, sum.1 = TRUE),
    category(name = "age",    prop_age$age,    targets = prop_age$count, sum.1 = TRUE),
    category(name = "educ",   prop_edu$educ,   targets = prop_edu$count, sum.1 = TRUE),
    category(name = "race",   prop_rac$race,   targets = prop_rac$count, sum.1 = TRUE)
    )

  df_wgt <- iterake(universe = uni,
                    max.iter = 100,
                    max.wgt = 15,
                    wgt.name = "weight_st",
                    summary = FALSE)

  select(df_wgt, case_id, weight, weight_st)
}

write_rds(st_par_w, "data/output/weights-state.Rds")


