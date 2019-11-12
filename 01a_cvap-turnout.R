library(tidyverse)
library(tidycensus)
library(foreach)
library(glue)
library(labelled)

source("00_functions.R")

# data -----
vars <- read_rds("data/input/acs/variable-descriptions.Rds")
pop_cd <- read_rds("data/input/acs/by-cd_acs_counts.Rds")

# CD pull---
cd_cvap_tc <- get_acs(geography = "congressional district",
                      year = 2017,
                      survey = "acs1",
                      variable = c("B29001_001"),
                      geometry = FALSE) %>%
  mutate(year = 2017) %>%
  std_acs() %>%
  transmute(cd = cd_name(NAME), variable = variable, tc_count = count)


# format ---
cvap <- read_csv("data/input/CVAP_2013-2017_ACS_csv_files/CD.csv", col_types = cols()) %>%
  filter(LNTITLE == "Total") %>%
  select(GEONAME:LNNUMBER, matches("CVAP")) %>%
  filter(!str_detect(GEONAME, "Puerto Rico")) %>%
  mutate(cd = cd_name(GEONAME)) %>%
  select(cd, matches("CVAP"))

cvap_diff <- left_join(cvap, cd_cvap_tc)

# ggplot(cvap_diff, aes(CVAP_EST, tc_count)) + geom_point()

elec <- read_csv("data/input/1976-2018-house.csv", col_types = cols()) %>%
  filter(year == 2018)

cd_votes <- elec %>%
  distinct(state, state_po, district, totalvotes) %>%
  mutate(district = replace(district, district == "0", "1")) %>%
  transmute(cd = str_c(state_po, "-", district), totalvotes) %>%
  left_join(cvap_diff, by = "cd") %>%
  mutate(turnout_cvap = totalvotes / CVAP_EST)




# save ---
write_rds(cd_votes, "data/output/by-cd_CVAP-turnout.Rds")
