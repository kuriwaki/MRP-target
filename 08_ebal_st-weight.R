library(tidyverse)
library(haven)
library(foreach)
library(ebal)

st_frac <- read_rds("data/output/by-st_ACS_gender-age-education.Rds")
cc18_uw <- read_rds("data/input/by-person_cces-2018.Rds")


by_st_ed <- st_frac %>%
  group_by(state) %>%
  summarize(edu = sum(count*(educ >= 3))/unique(count_geo)) %>%
  left_join(st_df)

pop_st_edu <- by_st_ed$edu
names(pop_st_edu) <- str_c("edu_", by_st_ed$st)

# CCES
cc18_fmt <- cc18_uw %>%
  mutate(st,
         edu = educ >= 3,
         st_match = st) %>%
  group_by(st) %>%
  mutate(n_st = n()) %>%
  ungroup()

n_svy <- nrow(cc18_fmt)

st_expand <- crossing(distinct(cc18_fmt, case_id, st, n_st, edu),
                      distinct(cc18_fmt, st_match))

st_wide <- st_expand %>%
  mutate(interaction = as.integer((st_match == st) & edu),
         int_scaled = interaction * (n_svy/n_st)) %>%
  pivot_wider(id_cols = c(case_id, st),
              names_from = st_match,
              values_from = int_scaled,
              names_prefix = "edu_")

# test
test <- TRUE
if (test) {
  four_vals_dum <- st_wide %>%
    select(matches("edu_A")) %>%
    colMeans()

  four_vals_cces <- group_by(cc18_fmt, st) %>%
    filter(str_detect(st, "^A")) %>%
    summarize(edu = mean(edu)) %>%
    pull(edu)

  stopifnot(all(round(four_vals_dum, 5) == round(four_vals_cces, 5)))
}

# matrix setup ----
sel_df <- st_wide %>%
  mutate(sel = 1) %>%
  add_row(!!!c(pop_st_edu, sel = 0))

sel_mat <- select(sel_df, matches("edu_"))
trt_vec <- pull(sel_df, sel)

# ebal
ebalance(Treatment = trt_vec, X = select(sel_mat, -51))
