library(tidyverse)

acs_17_slim <- read_rds("data/input/cleaned_acs17.Rds")
cc18_uw <- read_rds("data/input/CCES/by-person_cces-2018.Rds")


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
write_rds(cc18_ord, "data/input/CCES/cces18_cleaned.Rds")

# temp
cc18_out <- read_rds("data/input/by-person_cces-2018.Rds")


write_rds(select(cc18_out, year, case_id, vv_party_gen),
          "~/Dropbox/survey-weighting/data/CCES/cc18_vv-party-gen.rds")
write_rds(select(cc18_out, year, case_id, newsint),
          "~/Dropbox/survey-weighting/data/CCES/cc18_newsint.rds")
