library(tidyverse)
library(foreach)
library(brms)
library(doParallel)


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
fit <- read_rds("data/output/stan/by-cd_sanc_g-a-e_brm.Rds")

# prediced
cd_strat <- cd_strat_raw %>%
  rename(n_sanc = count) %>%
  filter(n_sanc > 0)


#  melt all predictions into long
pred_cell_i <- function(i, model = fit, strat = cd_strat) {
  p_i <- as.vector(fitted(model,
                          newdata = strat[i, ],
                          allow_new_levels = TRUE,
                          summary = FALSE))
  tribble(
    ~var, ~mean, ~median, ~sd, ~p025, ~p975, ~N,
    i, mean(p_i), median(p_i), sd(p_i),
    quantile(p_i, 0.025), quantile(p_i, 0.975),
    strat$count_geo[i]
  )
}

preds_sum <- mclapply(1:nrow(cd_strat), pred_cell_i, mc.cores = 4) %>% bind_rows()


write_rds(cd_df_sanc, "data/output/mrp/by-cd_sanc-estimates.Rds")