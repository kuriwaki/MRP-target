library(tidyverse)
library(haven)

source("00_functions.R")
load("data/output/variable-labels.Rdata")

age5_key

acs17 <- read_dta("data/input/ipums/acs2017.dta") %>%
  filter(age >= 18)


count(acs17, age) %>%
  filter(age %in% 64:68)

acs_age <- acs17 %>%
  rename(age_num = age) %>%
  mutate(age_chr = case_when(
    age_num %in% 18:24 ~ "18 to 24 years",
    age_num %in% 25:34 ~ "25 to 34 years",
    age_num %in% 35:44 ~ "35 to 44 years",
    age_num %in% 45:64 ~ "45 to 64 years",
    age_num >= 65 ~ "65 years and over"
  )) %>%
  left_join(age5_key)

acs_a_e <- acs_age %>%
  rename(educ_acs = educ) %>%
  mutate(cces_label = case_when(
    educd %in% 2:61 ~ "No HS",
    educd %in% 63:64 ~ "High School Graduate",
    educd %in% 65:81 ~ "Some College",
    educd %in% 65:81 ~ "Some College",
    educd %in% 101 ~ "4-Year",
    educd %in% 114:116 ~ "Post-Grad"
  )) %>%
  left_join(select(educ_key, cces_label, educ))


acs_g_a_e <- acs_a_e %>%
  mutate(gender_chr = str_to_title(as.character(as_factor(sex)))) %>%
  left_join(gender_key)

acs_r_g_a_e <- acs_g_a_e %>%
  mutate(race_5 = case_when(
    race %in% 1 & hispan == 0 ~ 1L, # "White",
    race %in% 2 & hispan == 0 ~ 2L, # "Black",
    hispan > 0 ~ 3L, # "Hispanic",
    race %in% 4:6 & hispan == 0 ~ 4L, # "Asian",
    TRUE ~ 5L, # "All Other"
  )) %>%
  rename(race_acs = race) %>%
  left_join(select(race_key, race_5, race))

acs_17_slim <- acs_r_g_a_e %>%
  mutate(state = str_to_title(as_factor(stateicp)),
         state = recode(state, `District Of Columbia` = "District of Columbia")) %>%
  select(year, perwt, state, citizen, age, educ, gender, race) %>%
  mutate(citizen = as.integer(citizen %in% 0:2))

write_rds(acs_17_slim, "data/input/cleaned_acs17.Rds")

acs_17_tx <- acs_17_slim %>%
  filter(state == "Texas")

acs_17_tx %>%
  write_rds("data/output/cces/sample-TX/acs_2017-TX.Rds")
