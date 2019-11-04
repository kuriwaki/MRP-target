library(tidyverse)
library(fs)

# Data -------
dir <- "~/Dropbox/CCES_representation/"

response <- readRDS(path(dir, "data/output/intermediate/by-response_response.Rds"))
person   <- readRDS(path(dir, "data/output/intermediate/by-person_covariates.Rds")) %>%
  filter(year == 2018)

load("data/output/variable-labels.Rdata")

ages  <- c("18 to 24 years",
           "25 to 34 years",
           "35 to 44 years",
           "45 to 64 years",
           "65 years and over")
age_lbl <- setNames(1:5L, ages)

resp_18 <- inner_join(person, response, by = c("year", "case_id")) %>%
  select_if(~any(!is.na(.x))) %>%
  select(year:weight, state:dist, matches("pid3"),
         gender:marstat, matches("vv"), qID:response) %>%
  mutate(age_bin = case_when(age %in% 18:24 ~ 1L,
                             age %in% 25:34 ~ 2L,
                             age %in% 35:44 ~ 3L,
                             age %in% 45:64 ~ 4L,
                             age >=   65    ~ 5L,
                             TRUE ~ NA_integer_),
         age = labelled(age_bin, age_lbl)) %>%
  mutate(race = as.character(as_factor(race))) %>%
  rename(race_cces_chr = race)

cc18 <- resp_18 %>%
  select(year:marstat, race_cces_chr) %>%
  distinct()

cc18_race <- cc18 %>%
  left_join(distinct(race_key, race_cces_chr, race))



write_rds(resp_18, "data/input/by-question_cces-2018.Rds")
write_rds(cc18_race, "data/input/by-person_cces-2018.Rds")
