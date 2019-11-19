library(tidyverse)
library(haven)
library(fs)

# Data -------
dir <- "~/Dropbox/CCES_representation/"

response <- readRDS(path(dir, "data/output/intermediate/by-response_response.Rds"))
person   <- readRDS(path(dir, "data/output/intermediate/by-person_covariates.Rds")) %>%
  filter(year == 2018)
cc18_all <- read_dta("~/Dropbox/cces_cumulative/data/source/cces/2018_cc.dta")

citizen <- cc18_all %>%
  transmute(case_id = as.integer(caseid), citizen = cit1)

load("data/output/variable-labels.Rdata")

ages  <- c("18 to 24 years",
           "25 to 34 years",
           "35 to 44 years",
           "45 to 64 years",
           "65 years and over")
age_lbl <- setNames(1:5L, ages)


resp_18 <- inner_join(person, response, by = c("year", "case_id")) %>%
  left_join(citizen) %>%
  select_if(~any(!is.na(.x))) %>%
  select(year:weight, state:dist, matches("pid3"),
         gender:marstat, citizen, matches("vv"), qID:response) %>%
  mutate(age_bin = case_when(age %in% 18:24 ~ 1L,
                             age %in% 25:34 ~ 2L,
                             age %in% 35:44 ~ 3L,
                             age %in% 45:64 ~ 4L,
                             age >=   65    ~ 5L,
                             TRUE ~ NA_integer_),
         age = labelled(age_bin, age_lbl)) %>%
  mutate(race = as.character(as_factor(race))) %>%
  rename(race_cces_chr = race) %>%
  rename(cces_label = educ) %>%
  mutate(cces_label = as.character(as_factor(cces_label)))

cc18 <- resp_18 %>%
  select(year:marstat, citizen, vv_turnout_gvm, race_cces_chr, cces_label) %>%
  distinct()

cc18_fmt <- cc18 %>%
  left_join(distinct(race_key, race_cces_chr, race), by = "race_cces_chr") %>%
  left_join(distinct(select(educ_key, cces_label, educ)), by = "cces_label") %>%
  mutate(cd = as.character(glue("{st}-{str_pad(dist, width = 2, pad = '0')}")))


write_rds(resp_18, "data/input/by-question_cces-2018.Rds")
write_rds(cc18_fmt, "data/input/by-person_cces-2018.Rds")
