library(tidyverse)
library(haven)
library(rcces)
library(fs)

# Data -------
dir <- "~/Dropbox/CCES_representation/"

response <- readRDS(path(dir, "data/output/intermediate/by-response_response.Rds"))
person   <- readRDS(path(dir, "data/output/intermediate/by-person_covariates.Rds")) %>%
  filter(year == 2018)
cc18_all <- read_dta("~/Dropbox/cces_cumulative/data/source/cces/2018_cc.dta")

citizen <- cc18_all %>%
  transmute(case_id = as.integer(caseid), citizen = cit1)

# joined
cc18_fmt <- person %>%
  left_join(citizen) %>%
  ccc_std_demographics()

# merge back on to response data
resp_18 <- inner_join(response, cc18_fmt)


write_rds(cc18_fmt, "data/input/by-person_cces-2018.Rds")
write_rds(resp_18, "data/input/by-question_cces-2018.Rds")
