library(tidyverse)
library(haven)


acs_17 <- read_rds("data/input/cleaned_acs17.Rds")

tab_e_uw <- count(acs_17, state, gender, age, educ, name = "count")
tab_e_wt <- count(acs_17, state, gender, age, educ, name = "count", wt = perwt)
tab_r_uw <- count(acs_17, state, gender, age, race, name = "count")
tab_r_wt <- count(acs_17, state, gender, age, race, name = "count", wt = perwt)
tab_b_uw <- count(acs_17, state, gender, age, race, educ, name = "count")
tab_b_wt <- count(acs_17, state, gender, age, race, educ, name = "count", wt = perwt)


tab_uw_stsum <- count(acs_17, state, name = "count_geo")
tab_wt_stsum <- count(acs_17, state, name = "count_geo", wt = perwt)

frac_e_uw <- left_join(tab_e_uw, tab_uw_stsum, by = "state") %>%
  mutate(acs_frac = count / count_geo)
frac_r_uw <- left_join(tab_r_uw, tab_uw_stsum, by = "state") %>%
  mutate(acs_frac = count / count_geo)
frac_e_wt <- left_join(tab_e_wt, tab_wt_stsum, by = "state") %>%
  mutate(acs_frac = count / count_geo)
frac_r_wt <- left_join(tab_r_wt, tab_wt_stsum, by = "state") %>%
  mutate(acs_frac = count / count_geo)
frac_b_uw <- left_join(tab_b_uw, tab_uw_stsum, by = "state") %>%
  mutate(acs_frac = count / count_geo)
frac_b_wt <- left_join(tab_b_wt, tab_wt_stsum, by = "state") %>%
  mutate(acs_frac = count / count_geo)



write_rds(frac_e_uw, "data/input/acs/by-st_acs_counts.Rds")
write_rds(frac_b_wt, "data/output/by-st_ACS-indiv_weighted.Rds")
write_rds(frac_b_uw, "data/output/by-st_ACS-indiv_unweighted.Rds")
