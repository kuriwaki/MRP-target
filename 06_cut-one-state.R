library(tidyverse)

cc18_raw <- read_rds("data/input/by-person_cces-2018.Rds")
resp_18 <- read_rds("data/input/by-question_cces-2018.Rds")

weights  <- read_rds("data/output/weights-state.Rds")
st_frac_educ <- read_rds("data/output/by-st_ACS_gender-age-education.Rds")
st_frac_race <- read_rds("data/output/by-st_ACS_gender-age-race.Rds")


# outcome ----
sanc_df <- filter(resp_18, q_label == "WitholdSanctuaryFunding") %>%
  select(case_id, sanc = response)

# clean ----
cc18_df <- left_join(cc18_raw, weights, by = c("case_id", "weight")) %>%
  left_join(sanc_df) %>%
  mutate(citizen = as.integer(citizen == 1),
         turnout = as.integer(vv_turnout_gvm == "Voted"),
         sanc = recode(sanc, Y = 1L, N = 0L),
         vv_turnout_gvm = NULL) %>%
  select(year, case_id, weight_us = weight, weight_st, state, cd,
         gender, age, educ, race, citizen, turnout, sanc)


tx_18 <- cc18_df %>%
  filter(state == "Texas") %>%
  rename(weight_tx = weight_st) %>%
  arrange(cd)

tx_acs_educ <- st_frac_educ %>%
  filter(state == "Texas", year == 2017)
tx_acs_race <- st_frac_race %>%
  filter(state == "Texas", year == 2017)

write_rds(tx_18, "data/output/cces/sample-TX/cces_2018-TX.Rds")
write_rds(tx_acs_educ, "data/output/cces/sample-TX/acs_2017-TX_educ.Rds")
write_rds(tx_acs_race, "data/output/cces/sample-TX/acs_2017-TX-race.Rds")





"`cces_2017-TX.Rds` is the 2018 CCES dataset from Texas.
There are two sets of weights. `cces_us` is the original weights YouGov weights.
They are meant to be applied to the whole CCES sample (not just Texas) to be nationally
representative. `cces_tx` are weights I made to match to the Texas state population.
I try to mimic the YouGov first step weighting as much as possible.
The target is the ACS counts calibrated by ACS (not just raw counts).
The moment conditions are on gender, age bin, education, race, and the 4-choose-2
pairwise interaction of those demographics, _except_ for education-race
interactions (because ACS didn't report that). I use the iterake package to
compute my weights (https://github.com/kuriwaki/MRP-target).


`cd`` is the congressional district as determined by YouGov. There are two possible
outcomes of interest. One is `turnout``, from the voter file match. This is useful
for validation and comparison to some ground truth. The other are Yes/No issue
questions. I chose the 'withold funding to sanctuary cities' one (`sanc`).

I also attached the ACS-calibrarted counts for Texas 2017. Each dataset is a
partition of the adult population in Texas in 2017. `count` is the number of people
in each cell as determined by ACS. I have a separate table for gender x age x race
and gender x age x education due to data availability.
" %>%
  write_lines("data/output/cces/sample-TX/README.md")
