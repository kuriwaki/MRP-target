library(tidyverse)
library(foreach)
library(brms)
library(glue)
library(fs)


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
  transform_vars() %>%
  filter(year == 2017)


# model ---
outcomes <- c("ahca", "budg", "immr", "visa", "tcja", "sanc", "turn")


for (sd in c("01", "02", "05")[1]) {
  cellfiles <- dir_ls(glue("data/output/stan_glmer/sd-{sd}"), recurse = TRUE)
  outcomes_s <- unique(str_extract(cellfiles, str_c("(", str_c(outcomes, collapse = "|"), ")")))

  for (y in outcomes_s[1]) { #c(outcomes_s)) {
    var_name <- glue("n_{y}")

    fit <- read_rds(glue("data/output/stan_glmer/sd-{sd}/by-cd_{y}_g-a-e_brm.Rds"))

    # prediced ----
    all_strat <- cd_strat_raw %>%
      filter(count > 0) %>%
      mutate(!!sym(var_name) := count) %>%
      select(cd, male, age, educ, matches("n_"))

    # wide predictions by CD
    for (cd_i in unique(all_strat$cd)) {
      cd_strat <- filter(all_strat, cd == cd_i)

      # no longer works with stan_glmer
      p_draws <- posterior_predict(fit,
                        newdata = cd_strat,
                        allow_new_levels = TRUE,
                        summary = FALSE) %>%
        t() %>%
        as_tibble() %>%
        mutate(cell = 1:n()) %>%
        bind_cols(cd_strat, .) %>%
        pivot_longer(cols = matches("V"), names_to = "iter") %>%
        mutate(iter = parse_number(iter))

      cd_est <- group_by(p_draws, cd, iter) %>%
        summarize(p_mrp = sum(value) / sum(.data[[var_name]]))

      write_rds(cd_est,
                glue("data/output/cd-estimates/stan_glmer/sd-{sd}/{y}/{cd_i}_gae_brm-preds.Rds"))
    }
  }
}
