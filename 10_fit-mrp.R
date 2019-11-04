library(tidyverse)
library(brms)
library(parallel)

resp_18 <- read_rds("data/input/by-question_cces-2018.Rds")
cd_frac_educ <- read_rds("data/output/by-cd_ACS_gender-age-education.Rds")
st_frac_educ <- read_rds("data/output/by-st_ACS_gender-age-education.Rds")

transform_vars <- function(tbl) {
  tbl %>%
  mutate(male = -0.5 + 1 * (as_factor(gender) == "Male"),
         educ = as_factor(educ),
         age = as_factor(age))
}

ahca_cces <- filter(resp_18, q_label == "AHCA") %>%
  filter(response %in% c("Y", "N")) %>%
  mutate(ahca = as.integer(response == "Y")) %>%
  transform_vars() %>%
  select(-gender)

st_strat <- st_frac_educ %>%
  transform_vars() %>%
  rename(n = count_geo)

cd_strat <- cd_frac_educ %>%
  transform_vars() %>%
  rename(n = count_geo)

st_binomial = ahca_cces %>%
  group_by(male, educ, age, st) %>%
  summarise(n = n(),
            ahca = sum(ahca)) %>%
  arrange(desc(n)) %>%
  ungroup()

cd_binomial = ahca_cces %>%
  group_by(male, educ, age, cd) %>%
  summarise(n = n(),
            ahca = sum(ahca)) %>%
  arrange(desc(n)) %>%
  ungroup()

fit = brm(ahca | trials(n) ~ male +
            (1 + male | cd)  + (1 + male | educ) + (1 + male | age) +
            (1 | cd:age) + (1 | cd:educ)  + (1 | educ:age),
          data = cd_binomial,
          family = "binomial",
          cores = 2,
          prior = set_prior("normal(0, 1)", class = "b") +
            set_prior("normal(0, 1)", class = "Intercept") +
            set_prior("normal(0, 1)", class = "sd"))

write_rds(fit, "data/output/stan/by-cd_ahca_gender-age-educ_brm.Rds")

predicted_d = fitted(fit, newdata = cd_strat, allow_new_levels = TRUE, summary = FALSE)

pstrat = function(df, predicted, ...) {
  predicted_quo = rlang::enquo(predicted)
  group_vars = rlang::enquos(...)

  df %>%
    group_by(!!!group_vars) %>%
    summarize(!!predicted_quo := sum(!!predicted_quo * n / sum(n))) %>%
    ungroup()
}

cd_df_ahca = mclapply(1:nrow(predicted_d), function(i) {
  cd_strat %>%
    mutate(predicted = (predicted_d[i, ] / n)) %>%
    pstrat(predicted, state) %>%
    mutate(rep = i)
}, mc.cores = 4) %>%
  bind_rows()

# write_rds(df_ahca, "data/output/mrp/by-state_ahca-estimates.Rds")
write_rds(cd_df_ahca, "data/output/mrp/by-cd_ahca-estimates.Rds")
