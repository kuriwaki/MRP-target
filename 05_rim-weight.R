library(tidyverse)
library(iterake)
library(foreach)
library(haven)
library(googlesheets)
library(ddi)

# data ----

cc18_uw <- read_rds("data/input/by-person_cces-2018.Rds") %>%
  mutate(g_a_e = str_c(gender, age, educ))

cd_frac <- read_rds("data/output/by-cd_ACS_gender-age-education.Rds")
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


if (FALSE) {
w_df_st <- matrix(NA,
               nrow = nrow(cc18_uw),
               ncol = length(u_stids),
               dimnames = list(NULL, u_stids)) %>%
  as_tibble()

# loop over states ---
for (st_i in u_stids) {

  target_i <- targets_st %>%
    filter(stid == st_i) %>%
    mutate(g_a_e = str_c(gender, age, educ))

  uni <- universe(
    data = cc18_uw,
    category(
      name = "g_a_e",
      buckets = target_i$g_a_e,
      targets = target_i$frac_geo
    )
  )

  df_wgt <- iterake(universe = uni,
                    max.iter = 100,
                    max.wgt = 15,
                    wgt.name = "weight_st")

  w_df_st[, st_i] <- df_wgt$weight_st
}


# CDs ---------
# container
# year
targets_cd <- cd_frac %>%
  filter(year == 2017)
u_cds <- unique(targets_cd$cd)
u_cdids  <- unique(targets_cd$cdid)

w_df_cd <- matrix(NA,
               nrow = nrow(cc18),
               ncol = length(u_cdids),
               dimnames = list(NULL, u_cdids)) %>%
  as_tibble()


# loop over CDs ---
for (dist_i in u_cdids) {

  target_i <- targets_cd %>%
    filter(cdid == dist_i) %>%
    mutate(g_a_e = str_c(gender, age, educ))

  uni <- universe(
    data = cc18,
    category(
      name = "g_a_e",
      buckets = target_i$g_a_e,
      targets = target_i$frac_geo
    )
  )

  df_wgt <- iterake(universe = uni,
                    max.iter = 100,
                    max.wgt = 15,
                    wgt.name = "weight_cd")

  w_df_cd[, dist_i] <- df_wgt$weight_cd
}

} # end multicolumn weighting

# state partitioned weights -----
tgt_st_educ <- st_frac_educ %>%
  filter(year == 2017)
tgt_st_race <- st_frac_race %>%
  filter(year == 2017)
u_states <- unique(targets_st$state)
u_stids  <- unique(targets_st$stid)

# subset at each state ---
st_par_w <- foreach(st_i = u_states, .combine = "bind_rows") %do% {
  cc_i <- filter(cc18_uw, state == st_i) %>% mutate_if(is.labelled, zap_labels)

  tgt_edu_i <- tgt_st_educ %>%
    filter(state == st_i) %>%
    mutate_if(is.labelled, zap_labels)
  tgt_race_i <- tgt_st_race %>%
    filter(state == st_i) %>%
    mutate_if(is.labelled, zap_labels)

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

cc18 %>%
  left_join(st_par_w) %>%
  filter(state == "Michigan") %>%
  count(gender, wt = weight_st) %>%
  mutate(frac = n / sum(n))

targets_st %>%
  filter(state == "Michigan") %>%
  group_by(gender) %>%
  summarize(n = sum(count)) %>%
  ungroup() %>%
  mutate(frac = n / sum(n))



# long, store ------

w_long_st <- w_df_st %>%
  mutate(row_id = 1:n(),
         geo = "st") %>%
  pivot_longer(-c(row_id, geo),
               names_to = "geoid",
               values_to = "weight")

w_long_cd <- w_df_cd %>%
  mutate(row_id = 1:n(),
         geo = "cd") %>%
  pivot_longer(-c(row_id, geo),
               names_to = "geoid",
               values_to = "weight",
               values_drop_na = TRUE)

w_df <- bind_rows(w_long_st, w_long_cd)
write_rds(w_df, "data/output/weights-rim_long.Rds")
