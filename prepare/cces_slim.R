library(tidyverse)
library(fs)

# Data -------
dir <- "~/Dropbox/CCES_representation/"

response <- readRDS(path(dir, "data/output/intermediate/by-response_response.Rds"))
person   <- readRDS(path(dir, "data/output/intermediate/by-person_covariates.Rds")) %>%
  filter(year == 2018)


resp_18 <- inner_join(person, response, by = c("year", "case_id")) %>%
  select_if(~any(!is.na(.x))) %>%
  select(year:weight, state:dist, matches("pid3"),
         gender:marstat, matches("vv"), qID:response)


write_rds(resp_18, "data/input/by-question_cces-2018.Rds")
