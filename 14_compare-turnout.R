library(tidyverse)
library(yardstick)
library(scales)
library(ggrepel)

#' reorder models
reord_m <- function(tbl) {
  mutate(tbl, model = fct_relevel(model, "p_raw", "p_yg", "p_st", "p_mrp"))
}

#' reorder models and name
name_models <- function(string) {
  recode_factor(string,
                p_raw = "Raw Average",
                p_st = "State Rim Weights",
                p_yg = "YouGov Weights",
                p_mrp = "MRP")
}

# Data ----
cces_cd <- read_rds("data/output/by-cd-issue_all-estimates_sd-01.Rds")
cd_votes <- read_rds("data/output/by-cd_CVAP-turnout.Rds")

# Melted versions
rel_all <- pivot_longer(cces_cd, -c(q_label, cd, n), names_to = "model", values_to = "p_est") %>% reord_m()


# turnout ---
rel_all %>%
  group_by(model) %>%
  summarize(var = var(p_est))

cd_df <- left_join(rel_all, cd_votes, by = "cd") %>%
  filter(q_label == "Turnout") %>%
  filter(turnout_cvap > 0.01)

# diagnostics
rmse_cd <- cd_df %>%
  group_by(q_label, model) %>%
  rmse(turnout_cvap, p_est) %>%
  mutate(txt = str_c("RMSE: ", format(.estimate*100, digits = 1, nsmall = 1), "pp"))

bias_cd <- cd_df %>%
  group_by(q_label, model) %>%
  mae(turnout_cvap, p_est)  %>%
  mutate(txt = str_c("MAD: ", format(.estimate*100, digits = 1, nsmall = 1), "pp"))

cd_df %>%
  group_by(model) %>%
  summarize(overall = abs(mean(turnout_cvap - p_est)))

cd_bind <- left_join(rmse_raw, bias_raw, by = c("q_label", "model")) %>%
  transmute(q_label, model, txt = str_c(txt.x, "\n", txt.y))


ggplot(cd_df, aes(turnout_cvap, p_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(size = 0.5, alpha = 0.5) +
  facet_wrap(~ model, labeller = labeller(model = name_models)) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), minor_breaks = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), minor_breaks = NULL) +
  geom_text(data = cd_bind, aes(x = 0.30, y = 0.80, label = txt), size = 3)  +
  coord_equal() +
  labs(x = "Turnout / CVAP",
       y = "Estimate")
