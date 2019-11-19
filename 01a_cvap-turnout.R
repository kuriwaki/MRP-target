library(tidyverse)
library(tidycensus)
library(foreach)
library(glue)
library(labelled)

source("00_functions.R")

# data -----
vars <- read_rds("data/input/acs/variable-descriptions.Rds")
pop_cd <- read_rds("data/output/by-cd_ACS_gender-age-education.Rds")


# CD pull---
cd_cvap_tc <- get_acs(geography = "congressional district",
                      year = 2017,
                      survey = "acs1",
                      variable = c("B29001_001"),
                      geometry = FALSE) %>%
  mutate(year = 2017) %>%
  std_acs() %>%
  transmute(cd = cd_name(NAME),
            variable = variable,
            cvap_count = count)

cd_vap <- pop_cd %>%
  filter(year == 2017) %>%
  group_by(cd) %>%
  summarize(vap_count = sum(count))


# format ---
# cvap <- read_csv("data/input/CVAP_2013-2017_ACS_csv_files/CD.csv", col_types = cols()) %>%
#   filter(LNTITLE == "Total") %>%
#   select(GEONAME:LNNUMBER, matches("CVAP")) %>%
#   filter(!str_detect(GEONAME, "Puerto Rico")) %>%
#   mutate(cd = cd_name(GEONAME)) %>%
#   select(cd, matches("CVAP"))

elec <- read_csv("data/input/1976-2018-house.csv", col_types = cols()) %>%
  filter(year == 2018)

cd_votes <- elec %>%
  distinct(state, state_po, district, totalvotes) %>%
  mutate(district = replace(district, district == "0", "1")) %>%
  transmute(cd = str_c(state_po, "-", str_pad(district, width = 2, pad = "0")),
            totalvotes = totalvotes) %>%
  left_join(cd_cvap_tc, by = "cd") %>%
  left_join(cd_vap, by = "cd") %>%
  mutate(turnout_cvap = totalvotes / cvap_count,
         turnout_vap = totalvotes / vap_count)


# save ---
write_rds(cd_votes, "data/output/by-cd_CVAP-turnout.Rds")
