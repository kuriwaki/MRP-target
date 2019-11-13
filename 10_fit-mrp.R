library(tidyverse)
library(parallel)
library(rstanarm)
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

wide_18 <- resp_18 %>%
  select(case_id, weight, cd, gender, age:marstat, vv_turnout_gvm, q_label, response) %>%
  pivot_wider(id_cols = case_id:vv_turnout_gvm,
              names_from = q_label,
              values_from = response)

wide_cces <- wide_18  %>%
  transform_vars() %>%
  transmute(case_id,
            cd, male, age, educ,
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
  group_by(male, educ, age, cd) %>%
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

ff_base <- "cbind(ahca, n_ahca - ahca) ~ male +
  (1 + male | cd)  + (1 + male | educ) + (1 + male | age) +
  (1 | cd:age) + (1 | cd:educ)  + (1 | educ:age)"


outcomes <- c("turn", "ahca", "sanc", "immr", "tcja", "budg", "visa")


fit_outcome <- function(outcome, data = cd_binomial, base_formula = ff_base, sd = 1){
  var <- enquo(outcome)
  var_name <- quo_name(var)
  nvar_name <- str_c("n_", var_name)

  ff_outcome <- as.formula(str_replace_all(base_formula, "ahca", outcome))
  print(ff_outcome)
  data_nzero <- data %>%
    filter(!!sym(nvar_name) != 0)

  fit <- stan_glmer(ff_outcome,
                    data = data_nzero,
                    family = binomial,
                    chains = 4,
                    cores = 4,
                    seed = 02138)

  write_rds(fit, path("data/output/stan_glmer", glue("sd-{str_pad(sd, 2, pad = '0')}/by-cd_{outcome}_g-a-e_brm.Rds")))
  write_rds(data_nzero, path("data/output/stan_glmer", glue("sd-{str_pad(sd, 2, pad = '0')}/by-cd_{outcome}_g-a-e_df.Rds")))
}

fit_outcome("turn", sd = 1)
# fit_outcome("turn", sd = 2)
# fit_outcome("turn", sd = 5)
fit_outcome("sanc")
fit_outcome("ahca")
# fit_outcome("budg")
# fit_outcome("visa")
# fit_outcome("immr")
# fit_outcome("tcja")
# fit_outcome("turn", sd = 10)

