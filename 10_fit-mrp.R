library(tidyverse)
library(parallel)
library(rstanarm)
library(fs)
library(glue)

source("00_functions.R")

resp_18 <- read_rds("data/input/CCES/by-question_cces-2018.Rds")
person_18 <- read_rds("data/input/CCES/by-person_cces-2018.Rds")
cd_results <- read_rds("data/input/by-cd_info.Rds")
cd_frac_educ <- read_rds("data/output/by-cd_ACS_gender-age-education.Rds")


wide_18 <- resp_18 %>%
  filter(citizen == 1) %>%
  select(case_id, q_label, response) %>%
  pivot_wider(id_cols = case_id,
              names_from = q_label,
              values_from = response)


wide_cces <- wide_18  %>%
  inner_join(person_18, by = "case_id") %>%
  inner_join(select(cd_results, cd, trump_vshare_cd = pct_trump), by = "cd") %>%
  mutate(trump_vshare_std = scale(trump_vshare_cd, center = TRUE, scale = FALSE)[, 1]) %>%
  transform_vars() %>%
  transmute(case_id,
            cd, male, age, educ, trump_vshare_std,
            ahca = as.integer(AHCA == "Y"),
            visa = as.integer(EndVisaLottery == "Y"),
            budg = as.integer(BudgetBipartisan == "Y"),
            immr = as.integer(ImmigrationRyan == "Y"),
            tcja = as.integer(TaxCutJobsAct == "Y"),
            sanc = as.integer(WitholdSanctuaryFunding == "Y"),
            turn = as.integer(vv_turnout_gvm == "Voted"))

cd_strat <- cd_frac_educ %>%
  transform_vars() %>%
  rename(n = count_geo)

cd_binomial <- wide_cces %>%
  group_by(male, educ, age, cd, trump_vshare_std) %>%
  summarise(
    n_ahca  = sum(!is.na(ahca)),
    n_visa  = sum(!is.na(visa)),
    n_budg  = sum(!is.na(budg)),
    n_immr  = sum(!is.na(immr)),
    n_tcja  = sum(!is.na(tcja)),
    n_sanc  = sum(!is.na(sanc)),
    n_turn = sum(!is.na(turn)),
    ahca = sum(ahca, na.rm = TRUE),
    visa = sum(visa, na.rm = TRUE),
    budg = sum(budg, na.rm = TRUE),
    immr = sum(immr, na.rm = TRUE),
    tcja = sum(tcja, na.rm = TRUE),
    sanc = sum(sanc, na.rm = TRUE),
    turn = sum(turn, na.rm = TRUE)
    ) %>%
  ungroup()

ff_base <- "cbind(ahca, n_ahca - ahca) ~ male + trump_vshare_std +
  (1 + male + trump_vshare_std | cd)  + (1 + male | educ) + (1 + male | age) +
  (1 | cd:age) + (1 | cd:educ)  + (1 | educ:age)"


outcomes <- c("turn", "ahca", "sanc", "immr", "tcja", "budg", "visa")


fit_outcome <- function(outcome, data = cd_binomial, base_formula = ff_base, sd = "default"){
  var <- enquo(outcome)
  var_name <- quo_name(var)
  nvar_name <- str_c("n_", var_name)

  ff_outcome <- as.formula(str_replace_all(base_formula, "ahca", outcome))
  print(ff_outcome)
  data_nzero <- data %>%
    filter(!!sym(nvar_name) != 0)

  if (is.numeric(sd)) {
    fit <- stan_glmer(ff_outcome,
                      data = data_nzero,
                      family = binomial,
                      prior = normal(location = 0, scale = sd),
                      prior_intercept = normal(location = 0, scale = sd),
                      prior_aux = normal(location = 0, scale = sd),
                      adapt_delta = 0.95,
                      chains = 4,
                      cores = 4,
                      prior_PD = FALSE,
                      seed = 02138)
  }
  if (sd == "hanretty") {
    fit <- stan_glmer(ff_outcome,
                      data = data_nzero,
                      family = binomial,
                      prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                      prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                      adapt_delta = 0.95,
                      chains = 4,
                      cores = 4,
                      prior_PD = FALSE,
                      seed = 02138)
  }

  if (sd == "default") {
    fit <- stan_glmer(ff_outcome,
                      data = data_nzero,
                      family = binomial,
                      adapt_delta = 0.95,
                      chains = 4,
                      cores = 4,
                      prior_PD = FALSE,
                      seed = 02138)
  }

  write_rds(fit, path("data/output/reg/stan_glmer",
                      glue("sd-{str_pad(sd, 2, pad = '0')}/{outcome}/by-cd_{outcome}_g-a-e-t_glmer.Rds")))
  write_rds(data_nzero, path("data/output/reg/stan_glmer",
                             glue("sd-{str_pad(sd, 2, pad = '0')}/{outcome}/by-cd_{outcome}_g-a-e-t_df.Rds")))
}

fit_outcome("ahca", sd = "hanretty")
fit_outcome("ahca", sd = 1)
fit_outcome("ahca", sd = "default")

