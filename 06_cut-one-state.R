library(tidyverse)
library(fs)

cc18_raw <- read_rds("data/input/by-person_cces-2018.Rds")
resp_18 <- read_rds("data/input/by-question_cces-2018.Rds")
weights  <- read_rds("data/output/weights-state.Rds")
cd_votes <- read_rds("data/output/by-cd_CVAP-turnout.Rds")

cd_descrip <- read_rds("data/input/by-cd_info.Rds")

cd_frac_educ <- read_rds("data/output/by-cd_ACS_gender-age-education.Rds") %>% filter(year == 2017)
cd_frac_race <- read_rds("data/output/by-cd_ACS_gender-age-race.Rds") %>% filter(year == 2017)

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
  left_join(select(cd_descrip, cd, pct_trump)) %>%
  select(year, case_id, weight_us = weight, matches("weight_st"), state, cd, dist,
         gender, age, educ, race, citizen, pid3, pid3_leaner, faminc, marstat,
         trump_vshare_cd = pct_trump,
         turnout, sanc)


tx_18 <- cc18_df %>%
  filter(state == "Texas") %>%
  rename(weight_tx_u = weight_st_uacs,
         weight_tx_w = weight_st_wacs) %>%
  arrange(cd)

tx_acs_st_educ <- st_frac_educ %>%
  filter(state == "Texas", year == 2017)
tx_acs_st_race <- st_frac_race %>%
  filter(state == "Texas", year == 2017)

tx_acs_cd_educ <- cd_frac_educ %>%
  filter(str_detect(cd, "TX"), year == 2017)
tx_acs_cd_race <- cd_frac_race %>%
  filter(str_detect(cd, "TX"), year == 2017)

tx_votes <- cd_votes %>%
  filter(str_detect(cd, "TX")) %>%
  mutate(sen_votes_est = (8371655) * (totalvotes/sum(totalvotes))) %>%
  transmute(cd = cd,
            ush_total_votes = totalvotes,
            sen_votes_est = sen_votes_est,
            ush_turnout_cvap = totalvotes / cvap_count,
            sen_turnout_cvap = sen_votes_est / cvap_count,
            ush_turnout_vap = totalvotes / vap_count,
            sen_turnout_vap = sen_votes_est / vap_count
            ) %>%
  left_join(cd_descrip, by = "cd")


# save

proj_path <- "data/output/cces-states/sample-TX"

write_rds(tx_18, path(proj_path, "cces_2018-TX.Rds"))
write_rds(tx_votes, path(proj_path, "cd-stats_2018-TX.Rds"))
write_rds(tx_acs_st_educ, path(proj_path, "acs_2017-st-TX_educ.Rds"))
write_rds(tx_acs_st_race, path(proj_path, "acs_2017-st-TX-race.Rds"))
write_rds(tx_acs_cd_educ, path(proj_path, "acs_2017-cd-TX_educ.Rds"))
write_rds(tx_acs_cd_race, path(proj_path, "acs_2017-cd-TX-race.Rds"))




"`cces_2017-TX.Rds` is the 2018 CCES dataset from Texas.
There are two sets of weights. `cces_us` is the original weights YouGov weights.
They are meant to be applied to the whole CCES sample (not just Texas) to be nationally
representative. `cces_tx_*` are weights I made to match to the Texas state population.
I try to mimic the YouGov first step weighting as much as possible.
The target is the ACS counts. `cces_tx_u` uses ACS unweighted counts (as if the ACS)
were the population and `cces_tx_w` uses ACS weighted counts (trying to approximate
the population acknowledging the ACS is a sample too).
The moment conditions are on gender, age bin, education, race, and the 4-choose-2
pairwise interaction of those demographics. I use the iterake package to
compute my weights (https://github.com/kuriwaki/MRP-target).


`cd` is the congressional district as determined by YouGov. There are two possible
outcomes of interest. One is `turnout`, from the voter file match. This is useful
for validation and comparison to some ground truth. The other are Yes/No issue
questions. I chose the 'withold funding to sanctuary cities' one (`sanc`).

Ground-truth turnout (`cd-stats_2018-TX.Rds`) is tricky because the denominator is unclear. Instead of
registered or eligible voters, I recommend using VAP (voting age population) or
CVAP (citizen voting age population) as a denominator. CVAP is used more than
VAP inp practice, but I coul only get VAP for post-stratificatin cells.
There are two numerators, votes for the US House (`ush_*`) and
_estimated_ votes for the US Senate (`sen_`). The Senate is more desirable because
it was the top of the ticket office and it was contested everywhere.  Unfortunately,
I don't have the turnout at the CD-level, so I assume a uniform shift and estimate the
Senate numerator of each district to be a fraction of the total votes cast in Senate
as a fraction of the House votes. This dataset also includes information about the
voteshare of Trump, Romney, and McCain, as well as handy placenames.


I also attached the ACS-calibrarted counts for Texas 2017. Each dataset is a
partition of the adult population in Texas in 2017. In each table,  `count` is the
number of people in each cell as determined by ACS.  There are 4 tables, 2 (by-state,
and by-CD) times 2(gender x age x education partition and gender x age x race
partition).


`turnout-ests_by-cd.Rds` lists various survey estimates from simple weighted data and
MRP. The MRP model I estimate is using age, education, gender, and CD-level Trump voteshare
(rescaled to mean 0). I do not exclude citizens. I then post-stratify to the VAP
cell counts from the ACS. `tunrout-ests_by-respondent.Rds` are the predicted probabilities
of success from the stan_glmer model (posterior means from 4000 draws). Therefore,
they are basically the predicted values of the demographic cells before they get post-stratified.
case_id is the unique identifier for CCES.

" %>%
  str_replace_all("(?<=[\\.a-z1-9])\n", " ") %>%
  write_lines(path(proj_path, "README.md"))
