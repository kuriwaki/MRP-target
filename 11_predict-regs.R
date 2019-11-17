library(tidyverse)
library(foreach)
library(brms)
library(glue)
library(fs)

source("00_functions.R")

# data ---
cd_strat_raw <- read_rds("data/output/by-cd_ACS_gender-age-education.Rds") %>%
  transform_vars() %>%
  filter(year == 2017)


# model ---
outcomes <- c("ahca", "budg", "immr", "visa", "tcja", "sanc", "turn")


for (sd in c("hanretty", "01", "default")) {
  cellfiles <- dir_ls(glue("data/output/stan_glmer/sd-{sd}"), recurse = TRUE)
  outcomes_s <- unique(str_extract(cellfiles, str_c("(", str_c(outcomes, collapse = "|"), ")")))

  for (y in "turn") {
    var_name <- glue("n_{y}")

    fit <- read_rds(glue("data/output/stan_glmer/sd-{sd}/by-cd_{y}_g-a-e_glmer.Rds"))

    # prediced ----
    all_strat <- cd_strat_raw %>%
      filter(count > 0) %>%
      mutate(!!sym(var_name) := count) %>%
      select(cd, male, age, educ, matches("n_"))

    # wide predictions by CD
    cds_loop <- unique(all_strat$cd)
    if (sd == "hanretty")
      cds_loop <- cds_loop[-str_which(cds_loop, "(CA|TX|AK|AL)")]

    for (cd_i in cds_loop) {
      cd_strat <- filter(all_strat, cd == cd_i)

      # no longer works with stan_glmer
      p_draws <- posterior_linpred(fit,
                        newdata = cd_strat,
                        transform = TRUE,
                        allow_new_levels = TRUE,
                        summary = FALSE) %>%
        t() %>%
        as_tibble() %>%
        mutate(cell = 1:n()) %>%
        bind_cols(cd_strat, .) %>%
        pivot_longer(cols = matches("V"), names_to = "iter") %>%
        mutate(iter = parse_number(iter))

      cd_est <- group_by(p_draws, cd, iter) %>%
        summarize(p_mrp = sum(value*.data[[var_name]]) / sum(.data[[var_name]]))

      write_rds(cd_est,
                glue("data/output/CDs/stan_glmer/sd-{sd}/{y}/{cd_i}_gae_glmer-preds.Rds"))
    }
  }
}
