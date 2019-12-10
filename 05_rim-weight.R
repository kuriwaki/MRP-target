library(tidyverse)
library(haven)
library(iterake)
library(foreach)


# data ----
cc18_uw <- read_rds("data/input/by-person_cces-2018.Rds")

st_frac_wt <- read_rds("data/output/by-st_ACS-indiv_weighted.Rds")
st_frac_uw <- read_rds("data/output/by-st_ACS-indiv_unweighted.Rds")
st_frac_wt <- read_rds("data/output/by-st_ACS-indiv_weighted.Rds")
st_frac_uw <- read_rds("data/output/by-st_ACS-indiv_unweighted.Rds")
us_frac_educ <- read_rds("data/output/by-us_ACS_gender-age-education.Rds")
us_frac_race <- read_rds("data/output/by-us_ACS_gender-age-race.Rds")


# States ---------
# year
targets_st <- st_frac
u_states <- unique(targets_st$state)

# state partitioned weights -----
u_states <- unique(targets_st$state)
u_stids  <- unique(targets_st$stid)


# Cces preprocess
cc_df <-  cc18_uw %>%
  mutate_if(is.labelled, zap_labels) %>%
  rename(sex = gender, edu = educ, rac = race) %>%
  mutate(sex_age = str_c(sex, "_", age),
         sex_edu = str_c(sex, "_", edu),
         sex_rac = str_c(sex, "_", rac),
         age_edu = str_c(age, "_", edu),
         age_rac = str_c(age, "_", rac),
         edu_rac = str_c(edu, "_", rac))


# subset at each state, and go state by state ---
large_states <- count(cc18_uw, state, sort = TRUE) %>% pull(state) %>% .[1:7]
use_wt <- TRUE

if (use_wt) {
  targets_df <- st_frac_wt
}

if (!use_wt) {
  targets_df <- st_frac_uw
}

st_par_w <- foreach(st_i = u_states, .combine = "bind_rows") %do% {

  # subset survey sample to state
  cc_i <- filter(cc_df, state == st_i)

  # subset target distributions to state
  tgt_i <- targets_df %>%
    filter(state == st_i) %>%
    mutate_if(is.labelled, zap_labels) %>%
    rename(sex = gender, edu = educ, rac = race)

  # tab marginal distribtions of the target
  prop_sex <- tgt_i %>%
    group_by(sex) %>%
    summarize(count = sum(count))

  prop_age <- tgt_i %>%
    group_by(age) %>%
    summarize(count = sum(count))

  prop_edu <- tgt_i %>%
    group_by(edu) %>%
    summarize(count = sum(count))

  prop_rac <- tgt_i %>%
    group_by(rac) %>%
    summarize(count = sum(count))

  # tab joint distributions of the target
  prop_sex_age <- tgt_i %>%
    group_by(sex, age) %>%
    summarize(count = sum(count)) %>%
    unite(sex_age, sex, age)

  prop_sex_edu <- tgt_i %>%
    group_by(sex, edu) %>%
    summarize(count = sum(count)) %>%
    unite(sex_edu, sex, edu)

  prop_sex_rac <- tgt_i %>%
    group_by(sex, rac) %>%
    summarize(count = sum(count)) %>%
    unite(sex_rac, sex, rac)

  prop_age_edu <- tgt_i %>%
    group_by(age, edu) %>%
    summarize(count = sum(count)) %>%
    unite(age_edu, age, edu)

  prop_age_rac <- tgt_i %>%
    group_by(age, rac) %>%
    summarize(count = sum(count)) %>%
    unite(age_rac, age, rac)

  prop_edu_rac <- tgt_i %>%
    group_by(edu, rac) %>%
    summarize(count = sum(count)) %>%
    unite(edu_rac, edu, rac)

  # adapt target if there's zero people of that race (Alaska = 4)
  race_i <- count(cc_i, rac)
  if (nrow(race_i) < 5) {
    cat(st_i, " has missing race....\n")
    prop_rac <-  semi_join(prop_rac, race_i, by = "rac")
  }


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
      category(name = "age_rac", prop_age_rac$age_rac, targets = prop_age_rac$count, sum.1 = TRUE),
      category(name = "edu_rac", prop_edu_rac$edu_rac, targets = prop_edu_rac$count, sum.1 = TRUE)
    )
  }

  if (st_i %in% large_states) cat(st_i, "....\n")
  df_wgt <- iterake(universe = uni,
                    max.iter = 100,
                    max.wgt = 15,
                    wgt.name = "weight_st",
                    summary = st_i %in% large_states)

  select(df_wgt, case_id, st, weight, weight_st)
}

# REPEAT but after setting use_wt to FALSE
rim_wt <- st_par_w
st_rim <- left_join(rim_uw, rim_wt, by = c("case_id", "st", "weight")) %>%
  rename(weight_st_wacs = weight_st.x,
         weight_st_uacs = weight_st.y)

write_rds(st_rim, "data/output/weights-state.Rds")



# REPEAT but for one nation ------
tgt_edu_i <- tgt_st_educ %>%
  mutate_if(is.labelled, zap_labels) %>%
  rename(sex = gender, edu = educ)
tgt_rac_i <- tgt_st_race %>%
  mutate_if(is.labelled, zap_labels) %>%
  rename(sex = gender, rac = race)

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

uni <- universe(
  data = cc_df,
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

df_wgt <- iterake(universe = uni,
                  max.iter = 100,
                  max.wgt = 15,
                  wgt.name = "weight_us")

nation_rim <- select(df_wgt, case_id, weight_us)


# Save ----
st_par_w %>%
  left_join(nation_rim) %>%
write_rds("data/output/weights-state.Rds")


wts_df <- read_rds("data/output/weights-state.Rds") %>%
  inner_join(select(cc18_uw, case_id, st, educ, race, age, gender))

weights_std <- wts_df %>%
  group_by(st) %>%
  mutate(weight_yg = weight / mean(weight))

weights_samp <- weights_std %>%
  sample_frac(0.05) %>%
  ungroup() %>%
  mutate_if(is.labelled, as_factor)

gg_us_st <- ggplot(weights_samp, aes(x = weight_us, y = weight_st, color = race)) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(alpha = 0.6, size = 0.5) +
  scale_x_continuous(limits = c(0, 15)) +
  scale_y_continuous(limits = c(0, 15)) +
  coord_equal() +
  scale_color_viridis_d() +
  coord_capped_cart(bottom = "both", left = "both") +
  labs(x = "US Rim Weights",
       y = "State Rim Weights",
       color = "Respondent Race")

gg_yg_st <- gg_us_st + aes(x = weight_yg) +
  labs(x = "YouGov Post-stratification weights",
       caption = "Each point is a respondent. All weights are standardized to mean 1 within state. 10% random sample.")


gg_us_st + gg_yg_st + plot_layout(guides = "collect")


