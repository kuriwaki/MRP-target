library(tidyverse)
library(foreach)
library(brms)
library(glue)


# copy from 10 --
transform_vars <- function(tbl) {
  tbl %>%
    mutate(male = -0.5 + 1 * (as_factor(gender) == "Male"),
           educ = as_factor(educ),
           age = as_factor(age))
}

pstrat = function(df, predicted, ...) {
  predicted_quo = rlang::enquo(predicted)
  group_vars = rlang::enquos(...)

  df %>%
    group_by(!!!group_vars) %>%
    summarize(!!predicted_quo := sum(!!predicted_quo * n / sum(n))) %>%
    ungroup()
}

# data ---
cd_strat_raw <- read_rds("data/output/by-cd_ACS_gender-age-education.Rds") %>%
  transform_vars()


# model ---
outcomes <- c("ahca", "sanc", "budg", "immr", "visa", "tcja")


for (y in outcomes) {
  var_name <- glue("n_{y}")

  fit <- read_rds(glue("data/output/stan/by-cd_{y}_g-a-e_brm.Rds"))

  # prediced ----
  cd_strat <- cd_strat_raw %>%
    filter(count > 0) %>%
    rename(!!sym(var_name) := count)

  # wide predictions
  p_draws <- fitted(fit,
                    newdata = cd_strat,
                    allow_new_levels = TRUE,
                    summary = FALSE)

  cell_mean   <- colMeans(p_draws)
  cell_median <- apply(p_draws, 2, median)
  cell_p025   <- apply(p_draws, 2, quantile, 0.025)
  cell_p975   <- apply(p_draws, 2, quantile, 0.975)

  var_stats <- tibble(
    i = 1:ncol(p_draws),
    mean = cell_mean,
    median = cell_median,
    p025 = cell_p025,
    p975 = cell_p975
  )

  cd_preds <- bind_cols(cd_strat, var_stats) %>%
    rename(count = n_sanc)

  write_rds(cd_preds,
            glue("data/output/by-cell_{y}_g-a-e_brm-preds.Rds"))
}