library(tidyverse)
library(haven)
library(rcces)
library(fs)

# Data -------
dir <- "~/Dropbox/CCES_representation/"

response <- readRDS(path(dir, "data/output/intermediate/by-response_response.Rds"))
person   <- readRDS(path(dir, "data/output/intermediate/by-person_covariates.Rds")) %>%
  filter(year == 2018)

ccc_dir <- "~/Dropbox/cces_cumulative/data/release"
ccc <- read_rds(path(ccc_dir, "cumulative_2006_2019_addon.Rds"))

# joined
cc18_fmt <- ccc %>%
  filter(year == 2018) %>%
  ccc_std_demographics()


# Save ----
write_rds(cc18_fmt, "data/input/by-person_cces-2018.Rds")

# merge back on to response data
resp_18 <- inner_join(response, cc18_fmt)
write_rds(resp_18, "data/input/by-question_cces-2018.Rds")
