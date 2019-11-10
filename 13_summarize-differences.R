library(tidyverse)
library(yardstick)
library(scales)

#' reorder models
reord_m <- function(tbl) {
  mutate(tbl, model = fct_relevel(model, "p_raw", "p_yg", "p_st", "p_mrp"))
}

#' copy from rcces
my.parse <- function(char, drop_year = TRUE) {
  pattern <- "([a-z])([A-X2])"
  replacement <- "\\1 \\2"

  char <- gsub(pattern, replacement, char,  perl = T, ignore.case = F)
  char <- gsub("ACA2", "ACA 2", char)
  char <- gsub("SCHIP2", "SCHIP 2", char)
  if (drop_year) char <- gsub("(20[0-1][1-9])", "", char,  perl = T, ignore.case = F)

  return(char)
}

#' reorder models and name
name_models <- function(string) {
  recode_factor(string,
                p_raw = "Raw Average",
                p_yg = "YouGov Weights",
                p_st = "State Rim Weights",
                p_mrp = "MRP")
}

# Data ----
cces_cd <- read_rds("data/output/by-cd-issue_all-estimates.Rds")

# Melted versions
rel_mrp <- pivot_longer(cces_cd, -c(q_label, cd, n, p_mrp), names_to = "model", values_to = "p_est") %>% reord_m()
rel_raw <- pivot_longer(cces_cd, -c(q_label, cd, n, p_raw), names_to = "model", values_to = "p_est") %>% reord_m()
rel_all <- pivot_longer(cces_cd, -c(q_label, cd, n), names_to = "model", values_to = "p_est") %>% reord_m()

rmse_mrp <- rel_mrp %>%
  group_by(q_label, model) %>%
  rmse(p_mrp, p_est) %>%
  mutate(txt = str_c("RMSD:", format(.estimate*100, digits = 1, nsmall = 1), "pp"))

bias_mrp <- rel_mrp %>%
  group_by(q_label, model) %>%
  mae(p_mrp, p_est)  %>%
  mutate(txt = str_c("MAD:", format(.estimate*100, digits = 1, nsmall = 1), "pp"))

mrp_bind <- left_join(rmse_mrp, bias_mrp, by = c("q_label", "model")) %>%
  transmute(q_label, model, txt = str_c(txt.x, "\n", txt.y))

rmse_raw <- rel_raw %>%
  group_by(q_label, model) %>%
  rmse(p_raw, p_est) %>%
  mutate(txt = str_c("RMSD: ", format(.estimate*100, digits = 1, nsmall = 1)), "pp")

bias_raw <- rel_raw %>%
  group_by(q_label, model) %>%
  mae(p_raw, p_est)  %>%
  mutate(txt = str_c("MAD: ", format(.estimate*100, digits = 1, nsmall = 1)), "pp")

raw_bind <- left_join(rmse_raw, bias_raw, by = c("q_label", "model")) %>%
  transmute(q_label, model, txt = str_c(txt.x, "\n", txt.y))

# Plot ----


gg_diff_base <- rel_raw %>%
  ggplot(aes(x = p_raw, y = p_est)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(size = 0.5, alpha = 0.25) +
  scale_x_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), minor_breaks = NULL) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1), minor_breaks = NULL) +
  coord_equal() +
  theme_bw() +
  labs(x = "Raw Average",
       y = "Model Estimates")

gg_diff_mrp  <- gg_diff_base +
  facet_grid(q_label ~ model, labeller = labeller(model = name_models, q_label = my.parse)) +
  geom_text(data = raw_bind, aes(x = 0.30, y = 0.80, label = txt), size = 3)


gg_diff_mrp <- gg_diff_base %+% rel_mrp +
  aes(x = p_mrp) +
  facet_grid(q_label ~ model, labeller = labeller(model = name_models, q_label = my.parse)) +
  geom_text(data = mrp_bind, aes(x = 0.30, y = 0.80, label = txt), size = 3) +
  labs(x = "MRP Estimate")


