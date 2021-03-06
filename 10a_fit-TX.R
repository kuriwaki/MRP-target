library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(forcats)
library(labelled)
library(rstanarm)
library(fs)
library(glue)
library(foreach)
library(ggrepel)

source("00_functions.R")


cces_tx <- read_rds("data/output/cces/sample-TX/cces_2018-TX.Rds") %>%
  filter(citizen == 1)
cd_votes <- read_rds("data/output/cces/sample-TX/cd-stats_2018-TX.Rds")

cces_count <- cces_tx %>%
  mutate(trump_vshare_std = scale(trump_vshare_cd, center = TRUE, scale = FALSE)[, 1]) %>%
  transform_vars() %>%
  group_by(cd, trump_vshare_std, age, male, educ) %>%
  summarize(
    turn = sum(turnout),
    n_turn = sum(!is.na(turnout))
  ) %>%
  ungroup()


ff_base <- "cbind(turn, n_turn - turn) ~ male + trump_vshare_std +
  (1 + male + trump_vshare_std | cd)  + (1 + male | educ) + (1 + male | age) +
  (1 | cd:age) + (1 | cd:educ)  + (1 | educ:age)"


fit <- stan_glmer(ff_base,
                  data = ungroup(filter(cces_count, n_turn > 0)),
                  family = binomial,
                  prior_intercept = rstanarm::student_t(5, 0, 10, autoscale = FALSE),
                  prior = rstanarm::student_t(5, 0, 2.5, autoscale = FALSE),
                  chains = 4,
                  cores = 4,
                  prior_PD = FALSE,
                  seed = 02138)

write_rds(fit, "data/output/glmer-turn_TX_gaet.Rds")


# data ---
cd_strat_raw <- read_rds("data/output/by-cd_ACS_gender-age-education.Rds") %>%
  transform_vars() %>%
  filter(year == 2017, str_detect(cd, "TX"))


# model ---
var_name <- glue("n_turn")

# predicted, by CD ----
all_strat <- cd_strat_raw %>%
  filter(count > 0) %>%
  mutate(!!sym(var_name) := count) %>%
  select(cd, male, age, educ, matches("n_")) %>%
  left_join(select(cd_votes, cd, pct_trump)) %>%
  mutate(trump_vshare_std = pct_trump - mean(cces_tx$trump_vshare_cd))

# wide predictions by CD
cds_loop <- unique(all_strat$cd)

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
    pivot_longer(cols = matches("^V"), names_to = "iter") %>%
    mutate(iter = parse_number(iter))

  cd_est <- group_by(p_draws, cd, iter) %>%
    summarize(p_mrp = sum(value*.data[[var_name]]) / sum(.data[[var_name]]))

  write_rds(cd_est,
            glue("data/output/CDs/stan_glmer/TX/{cd_i}-preds.Rds"))
}

## predicted, by cell -----
cell_preds <- posterior_linpred(fit,
                  newdata = all_strat,
                  transform = TRUE,
                  allow_new_levels = TRUE,
                  summary = TRUE)
stopifnot(ncol(cell_preds) == nrow(all_strat))

cell_df <- cell_preds %>%
  t() %>%
  as_tibble() %>%
  mutate(cell = 1:n()) %>%
  bind_cols(all_strat, .) %>%
  pivot_longer(cols = matches("^V"), names_to = "iter") %>%
  mutate(iter = parse_number(iter))

cell_phat <- cell_df %>%
  group_by(cd, male, age, educ) %>%
  summarize(p_mrp = mean(value)) %>%
  ungroup()

cces_phat <- cces_tx %>%
  transform_vars() %>%
  left_join(cell_phat) %>%
  select(case_id, turnout_phat = p_mrp)


# cd-estimates --------
ests <- dir_ls(glue("data/output/CDs/stan_glmer/TX/"))

cells_est <- foreach(cd = ests, .combine = "bind_rows") %do% {read_rds(cd)}

mrp_ests <- cells_est %>%
    group_by(cd) %>%
    summarize(p_mrp_est = mean(p_mrp),
              p_mrp_025 = quantile(p_mrp, 0.025),
              p_mrp_050 = quantile(p_mrp, 0.050),
              p_mrp_950 = quantile(p_mrp, 0.950),
              p_mrp_975 = quantile(p_mrp, 0.975)) %>%
    select(cd, matches("n_"), matches("p_"), matches("se_")) %>%
  inner_join(cd_votes)


# survey estimates
p_est <- cces_tx %>%
  group_by(cd) %>%
  summarize(p_raw = mean(turnout, na.rm = TRUE),
            p_yg = weighted.mean(turnout, weight_us, na.rm = TRUE),
            p_st = weighted.mean(turnout, weight_tx, na.rm = TRUE))

n_eff <- cces_tx %>%
  summarize(n_raw = sum(!is.na(turnout)),
            n_yg  = sum(!is.na(turnout)) / (1 + var(weight_us / mean(weight_us))),
            n_st  = sum(!is.na(turnout)) / (1 + var(weight_tx / mean(weight_tx))),
  )

se_p <- function(p, n) sqrt(p*(1 - p) / n)
ests_df <- p_est %>%
  mutate(n_raw = n_eff$n_raw,
         n_yg = n_eff$n_yg,
         n_st = n_eff$n_st) %>%
  mutate(se_raw = se_p(p_raw, n_raw),
         se_yg = se_p(p_yg, n_yg),
         se_st = se_p(p_st, n_st)) %>%
  left_join(mrp_ests)


write_rds(ests_df, "data/output/cces/sample-TX/turnout-ests_by-cd.Rds")
write_rds(cces_phat, "data/output/cces/sample-TX/turnout-ests_by-respondent.Rds")



mrp_ests %>%
  summarize(cvg_95 = mean(p_mrp_025 <= sen_turnout_cvap & p_mrp_975 >= sen_turnout_cvap),
            cvg_90 = mean(p_mrp_050 <= sen_turnout_cvap & p_mrp_950 >= sen_turnout_cvap),
            rmse_v = sqrt(mean((ush_turnout_cvap - p_mrp_est)^2)),
            rmse_c = sqrt(mean((ush_turnout_vap - p_mrp_est)^2)),
            )


ggplot(mrp_ests, aes(x = sen_turnout_cvap, y = p_mrp_est, color = pct_trump)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point() +
  geom_errorbar(aes(ymin = p_mrp_025, ymax = p_mrp_975), alpha = 0.4) +
  theme_bw() +
  coord_equal() +
  geom_text_repel(aes(label = place), size = 3) +
  scale_color_viridis_c(begin = 0.1, end = 0.9, option = "A", direction = 1) +
  # guides(color =  FALSE)  +
  labs(x = "Turnout (Percent of VAP)",
       y = "MRP Estimate of Turnout")

