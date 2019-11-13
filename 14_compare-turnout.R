library(tidyverse)
library(yardstick)
library(scales)
library(ggrepel)

#' reorder models
reord_m <- function(tbl) {
  mutate(tbl, model = fct_relevel(model, "raw", "yg", "st", "mrp"))
}

#' reorder models and name
name_models <- function(string) {
  recode_factor(string,
                raw = "Raw Average",
                st = "State Rim Weights",
                yg = "YouGov National Weights",
                mrp = "MRP")
}

# Data ----
cces_cd  <- read_rds("data/output/by-cd-issue_all-estimates_sd-02.Rds")
cd_votes <- read_rds("data/output/by-cd_CVAP-turnout.Rds")

# Melted versions
rel_all <- pivot_longer(cces_cd,
                        -c(q_label, cd, matches("p_mrp_[0-9+]")),
                        names_pattern = "(n|p|se)_(raw|yg|st|mrp_est)",
                        names_to = c(".value", "model")) %>%
  mutate(model = recode(model, mrp_est = "mrp")) %>%
  reord_m()


# turnout ---

cd_df <- left_join(rel_all, cd_votes, by = "cd") %>%
  filter(q_label == "Turnout") %>%
  filter(turnout_cvap > 0.01) %>%
  mutate(p_lb = p - qnorm(0.975)*se,
         p_ub = p + qnorm(0.975)*se) %>%
  mutate(p_lb = coalesce(p_lb, p_mrp_025),
         p_ub = coalesce(p_ub, p_mrp_975))


# diagnostics
rmse_cd <- cd_df %>%
  group_by(q_label, model) %>%
  rmse(turnout_cvap, p) %>%
  mutate(rmse_txt = str_c("RMSE: ", format(.estimate*100, digits = 1, nsmall = 1), "pp"))

bias_cd <- cd_df %>%
  group_by(q_label, model) %>%
  mae(turnout_cvap, p)  %>%
  mutate(mad_txt = str_c("MAD: ", format(.estimate*100, digits = 1, nsmall = 1), "pp"))

avgbias_cd <- cd_df %>%
  group_by(q_label, model) %>%
  summarize(overall = abs(mean(turnout_cvap - p))) %>%
  mutate(bias_txt = str_c("Overall Bias: ", format(overall*100, digits = 1, nsmall = 1), "pp"))

cvg_cd <- cd_df %>%
  group_by(q_label, model) %>%
  summarize(coverage = mean((turnout_cvap < p_ub & turnout_cvap > p_lb))) %>%
  filter(!is.na(coverage)) %>%
  mutate(cvg_txt = str_c("Coverage: ", percent(coverage, accuracy = 1)))

cd_bind <- left_join(rmse_cd, bias_cd, by = c("q_label", "model")) %>%
  left_join(avgbias_cd) %>%
  left_join(cvg_cd) %>%
  transmute(q_label, model, txt = str_c(rmse_txt, "\n", mad_txt, "\n", replace_na(cvg_txt, "")))


ggplot(cd_df, aes(turnout_cvap, p)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(size = 0.5, alpha = 0.25) +
  geom_errorbar(aes(ymin = p_lb, ymax = p_ub), width = 0, alpha = 0.10) +
  facet_wrap(~ model, labeller = labeller(model = name_models)) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1),  minor_breaks = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1),  minor_breaks = NULL) +
  geom_text(data = cd_bind, aes(x = 0.40, y = 0.80, label = txt), size = 3)  +
  coord_equal() +
  theme_classic() +
  labs(x = "CVAP Turnout (House Vote / 2017 ACS CVAP Count)",
       y = "Estimate")
