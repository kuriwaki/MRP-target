library(tidyverse)
library(yardstick)
library(scales)
library(ggrepel)

#' reorder models
reord_m <- function(tbl) {
  mutate(tbl, model = fct_relevel(model, "p_raw", "p_yg", "p_st", "p_mrp"))
}

#' copy from rcces
my.parse <- function(char, drop_year = TRUE) {
  pattern <- "([a-z])([A-X2])"
  replacement <- "\\1 \\2"

  char <- gsub(pattern, replacement, char,  perl = TRUE, ignore.case = FALSE)
  char <- gsub("ACA2", "ACA 2", char)
  char <- gsub("SCHIP2", "SCHIP 2", char)
  if (drop_year) char <- gsub("(20[0-1][1-9])", "", char,  perl = TRUE, ignore.case = F)

  char <- fct_relevel(factor(char), "Turnout")
  return(char)
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
rel_mrp <- pivot_longer(cces_cd, -c(q_label, cd, n, p_mrp), names_to = "model", values_to = "p_est") %>% reord_m()
rel_raw <- pivot_longer(cces_cd, -c(q_label, cd, n, p_raw), names_to = "model", values_to = "p_est") %>% reord_m()
rel_all <- pivot_longer(cces_cd, -c(q_label, cd, n), names_to = "model", values_to = "p_est") %>% reord_m()


# summarize
rmse_mrp <- rel_mrp %>%
  group_by(q_label, model) %>%
  rmse(p_mrp, p_est) %>%
  mutate(txt = str_c("RMSD: ", format(.estimate*100, digits = 1, nsmall = 1), "pp"))

bias_mrp <- rel_mrp %>%
  group_by(q_label, model) %>%
  mae(p_mrp, p_est)  %>%
  mutate(txt = str_c("MAD: ", format(.estimate*100, digits = 1, nsmall = 1), "pp"))

mrp_bind <- left_join(rmse_mrp, bias_mrp, by = c("q_label", "model")) %>%
  transmute(q_label, model, txt = str_c(txt.x, "\n", txt.y))

rmse_raw <- rel_raw %>%
  group_by(q_label, model) %>%
  rmse(p_raw, p_est) %>%
  mutate(txt = str_c("RMSD: ", format(.estimate*100, digits = 1, nsmall = 1), "pp"))

bias_raw <- rel_raw %>%
  group_by(q_label, model) %>%
  mae(p_raw, p_est)  %>%
  mutate(txt = str_c("MAD: ", format(.estimate*100, digits = 1, nsmall = 1), "pp"))

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

gg_diff_raw  <- gg_diff_base +
  facet_grid(q_label ~ model, labeller = labeller(model = name_models, q_label = my.parse)) +
  geom_text(data = raw_bind, aes(x = 0.30, y = 0.80, label = txt), size = 3)


gg_diff_mrp <- gg_diff_base %+% rel_mrp +
  aes(x = p_mrp) +
  facet_grid(q_label ~ model, labeller = labeller(model = name_models, q_label = my.parse)) +
  geom_text(data = mrp_bind, aes(x = 0.30, y = 0.80, label = txt), size = 3) +
  labs(x = "MRP Estimate")


ggsave("figures/by-cd_model-comparison-raw.pdf", gg_diff_raw, h = 10, w = 5)
ggsave("figures/by-cd_model-comparison-mrp.pdf", gg_diff_mrp, h = 10, w = 5)


# when sd 2

cces_cd2 <- read_rds("data/output/by-cd-issue_all-estimates_sd-02.Rds") %>%
  ungroup() %>%
  transmute(q_label, cd, p_mrp2 = p_mrp)

cces_comp <- left_join(cces_cd, cces_cd2) %>%
  filter(!is.na(p_mrp2))

cces_comp %>%
  group_by(q_label) %>%
  mae(p_mrp, p_mrp2)


ggplot(cces_comp, aes(p_mrp, p_mrp2)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_grid(~q_label) +
  geom_point(alpha = 0.2) +
  geom_text_repel(data = filter(cces_comp, str_detect(cd, "SC")),
                  aes(label = cd)) +
  theme_bw() +
  coord_equal()
