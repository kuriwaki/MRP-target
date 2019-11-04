library(tidyverse)
library(parallel)
library(brms)
library(fs)
library(glue)

resp_18 <- read_rds("data/input/by-question_cces-2018.Rds")
cd_frac_educ <- read_rds("data/output/by-cd_ACS_gender-age-education.Rds")

transform_vars <- function(tbl) {
  tbl %>%
  mutate(male = -0.5 + 1 * (as_factor(gender) == "Male"),
         educ = as_factor(educ),
         age = as_factor(age))
}

sanc_cces <- filter(resp_18, q_label == "WitholdSanctuaryFunding") %>%
  mutate(sanc = as.integer(response == "Y")) %>%
  select(case_id, sanc)

wide_cces <- filter(resp_18, q_label == "AHCA") %>%
  mutate(ahca = as.integer(response == "Y")) %>%
  transform_vars() %>%
  select(-gender) %>%
  left_join(sanc_cces)

cd_strat <- cd_frac_educ %>%
  transform_vars() %>%
  rename(n = count_geo)

st_binomial = wide_cces %>%
  group_by(male, educ, age, st) %>%
  summarise(n_ahca = sum(!is.na(ahca)),
            n_sanc = sum(!is.na(sanc)),
            ahca = sum(ahca, na.rm = TRUE),
            sanc = sum(sanc, na.rm = TRUE)) %>%
  arrange(desc(n_ahca)) %>%
  ungroup()

cd_binomial = wide_cces %>%
  group_by(male, educ, age, cd) %>%
  summarise(n_ahca = sum(!is.na(ahca)),
            n_sanc = sum(!is.na(sanc)),
            ahca = sum(ahca, na.rm = TRUE),
            sanc = sum(sanc, na.rm = TRUE)) %>%
  arrange(desc(n_ahca)) %>%
  filter(n_sanc > 0, n_ahca > 0) %>%
  ungroup()

outcome_forms <- list(
  ahca = ahca | trials(n_ahca) ~ male +
    (1 + male | cd)  + (1 + male | educ) + (1 + male | age) +
    (1 | cd:age) + (1 | cd:educ)  + (1 | educ:age),
  sanc = sanc | trials(n_sanc) ~ male +
    (1 + male | cd)  + (1 + male | educ) + (1 + male | age) +
    (1 | cd:age) + (1 | cd:educ)  + (1 | educ:age)
)


for (i in length(outcome_forms)) {

  fit = brm(outcome_forms[[i]],
            data = cd_binomial,
            family = "binomial",
            cores = 3,
            prior = set_prior("normal(0, 1)", class = "b") +
              set_prior("normal(0, 1)", class = "Intercept") +
              set_prior("normal(0, 1)", class = "sd"))

  write_rds(fit,
            path("data/output/stan",
                 glue("by-cd_{names(outcome_forms)[i]}_g-a-e_brm.Rds")))
}

