library(tidyverse)
library(iterake)
library(foreach)
library(haven)
library(googlesheets)
library(ddi)

# data ----

cc18 <- read_rds("data/input/by-person_cces-2018.Rds") %>%
  mutate(g_a_e = str_c(gender, age, educ))

cd_frac <- read_rds("data/output/by-cd_ACS_gender-age-education.Rds")
st_frac <- read_rds("data/output/by-st_ACS_gender-age-education.Rds")


# cd_clinton <- gs_read(gs_title("wasserman_2018-2016_CD")) %>%
  # mutate(clinton_2pty = (margin_clinton + 1)/2)

data(g2016)

# States ---------
# year
targets_st <- st_frac %>%
  filter(year == 2017)
u_states <- unique(targets_st$state)
u_stids  <- unique(targets_st$stid)


if (FALSE) {
w_df_st <- matrix(NA,
               nrow = nrow(cc18),
               ncol = length(u_stids),
               dimnames = list(NULL, u_stids)) %>%
  as_tibble()

# loop over states ---
for (st_i in u_stids) {

  target_i <- targets_st %>%
    filter(stid == st_i) %>%
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
targets_st <- st_frac %>%
  filter(year == 2017)
u_states <- unique(targets_st$state)
u_stids  <- unique(targets_st$stid)

# subset at each state ---
st_par_w <- foreach(st_i = u_states, .combine = "bind_rows") %do% {

  target_i <- targets_st %>%
    filter(state == st_i) %>%
    mutate_if(is.labelled, zap_labels)

  prop_i_gender <- target_i %>%
    group_by(gender) %>%
    summarize(count = sum(count))

  prop_i_age <- target_i %>%
    group_by(age) %>%
    summarize(count = sum(count))

  prop_i_educ <- target_i %>%
    group_by(educ) %>%
    summarize(count = sum(count))

  uni <- universe(
    data = filter(cc18, state == st_i) %>% mutate_if(is.labelled, zap_labels),
    category(name = "gender", prop_i_gender$gender, targets = prop_i_gender$count, sum.1 = TRUE),
    category(name = "age", prop_i_age$age, targets = prop_i_age$count, sum.1 = TRUE),
    category(name = "educ", prop_i_educ$educ, targets = prop_i_educ$count, sum.1 = TRUE)
    )

  df_wgt <- iterake(universe = uni,
                    max.iter = 100,
                    max.wgt = 15,
                    wgt.name = "weight_st",
                    summary = FALSE)

  select(df_wgt, case_id, weight, weight_st)
}

st_par_w %>%
  ggplot(aes(weight, weight_st)) +
  geom_point(size = 0.1) +
  coord_equal()

write_rds(st_par_w, "data/output/weights-state.Rds")

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
