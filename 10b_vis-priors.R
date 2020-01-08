library(tidyverse)
library(parallel)
library(rstanarm)
library(fs)
library(glue)
library(patchwork)

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
    n_turn = sum(!is.na(turn)),
    turn = sum(turn, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(n_turn > 0)

ff_outcome <- "cbind(turn, n_turn - turn) ~ male + trump_vshare_std +
  (1 + male + trump_vshare_std | cd)  + (1 + male | educ) + (1 + male | age) +
  (1 | cd:age) + (1 | cd:educ)  + (1 | educ:age)"

ff_outcome <- "cbind(turn, n_turn - turn) ~ male + (1 | cd) + (1 | educ) + (1 | age)"


priors_default <- stan_glmer(ff_outcome,
                             data = cd_binomial,
                             family = binomial,
                             adapt_delta = 0.95,
                             chains = 1,
                             prior_PD = TRUE)

priors_norm_01 <- update(priors_default,
                         prior_intercept = normal(0, 1),
                         prior = normal(0, 1))

priors_student <- update(priors_default,
                         prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                         prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE))


linpred_default <- posterior_linpred(priors_default, transform = TRUE)
linpred_norm_01 <- posterior_linpred(priors_norm_01, transform = TRUE)
linpred_student <- posterior_linpred(priors_student, transform = TRUE)
hist(linpred_default[, ], breaks = 1000)
hist(linpred_norm_01[, ], breaks = 100)
hist(linpred_student[, ], breaks = 1000)
dim(linpred_default)

# plotting the coefficient estimates (implied  by the prior)
int_d <- plot(priors_default, "hist", pars = "(Intercept)", binwidth = 1)  +
   scale_x_continuous(limits = c(-80, 80)) +
   labs(title = "Prior for Intercept",
        y = "rstanarm\nDefault") +
  theme(title = element_text(hjust = 0.5, size = 8))
int_n <- plot(priors_norm_01, "hist", pars = "(Intercept)", binwidth = 1) +
   scale_x_continuous(limits = c(-80, 80)) +
   labs(title = "Prior for Intercept",
        y = "Normal(0, 1)") +
  theme(title = element_text(hjust = 0.5, size = 8))
int_s <- plot(priors_student, "hist", pars = "(Intercept)", binwidth = 1) +
   scale_x_continuous(limits = c(-80, 80)) +
   labs(title = "Prior for Intercept",
        y = "Hanretty (2018)") +
  theme(title = element_text(hjust = 0.5, size = 8))

vsh_d <- plot(priors_default, "hist", pars = "trump_vshare_std", binwidth = 1)  +
  scale_x_continuous(limits = c(-80, 80)) +
  labs(title = "Prior for Mean 0 Continuous Variable theoretically [-0.5, 0.5]") +
  theme(title = element_text(hjust = 0.5, size = 8))
vsh_n <- plot(priors_norm_01, "hist", pars = "trump_vshare_std", binwidth = 1)  +
  scale_x_continuous(limits = c(-50, 50)) +
  labs(title = "Prior for Mean 0 Continuous Variable") +
  theme(title = element_text(hjust = 0.5, size = 8))
vsh_s <- plot(priors_student, "hist", pars = "trump_vshare_std", binwidth = 1) +
  scale_x_continuous(limits = c(-50, 50)) +
  labs(title = "Prior for Mean 0 Continuous Variable") +
  theme(title = element_text(hjust = 0.5, size = 8))

bin_d <- plot(priors_default, "hist", pars = "male", binwidth = 1)  +
  scale_x_continuous(limits = c(-15, 15)) +
  labs(title = "Prior for Mean 0 Binary Variable {-0.5, 0.5}") +
  theme(title = element_text(hjust = 0.5, size = 8))
bin_n <- plot(priors_norm_01, "hist", pars = "male", binwidth = 1)  +
  scale_x_continuous(limits = c(-15, 15)) +
  labs(title = "Prior for Mean 0 Binary Variable") +
  theme(title = element_text(hjust = 0.5, size = 8))
bin_s <- plot(priors_student, "hist", pars = "male", binwidth = 1) +
  scale_x_continuous(limits = c(-15, 15)) +
  labs(title = "Prior for Mean 0 Binary Variable") +
  theme(title = element_text(hjust = 0.5, size = 8))


int_d + bin_d + vsh_d +
  int_n + bin_n + vsh_n +
  int_s + bin_s + vsh_s +
  plot_layout(ncol = 3, nro = 3, byrow = TRUE)

ggsave("figures/priors-implications.pdf", w = 8, h = 6)
