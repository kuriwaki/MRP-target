library(tidyverse)

acs_17_slim <- read_rds("data/input/cleaned_acs17.Rds")
cc18_uw <- read_rds("data/input/by-person_cces-2018.Rds")

st_df <- distinct(cc18_uw, state, st)

ac17_ord <- acs_17_slim %>%
  left_join(st_df, by = "state") %>%
  select(-year) %>%
  select(state, st, perwt, everything())

cc18_ord <- cc18_uw %>%
  select(case_id, weight_yg = weight, state, st, citizen,
         age, educ, gender, race,
         everything())


# Export parallel
write_rds(ac17_ord, "data/output/cces-states/acs-cces/acs17_cleaned.Rds")
write_rds(cc18_ord, "data/output/cces-states/acs-cces/cces18_cleaned.Rds")
