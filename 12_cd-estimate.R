library(tidyverse)
library(lemon)
library(corrr)
library(scales)
library(patchwork)

cd_phat <- read_rds("data/output/by-cell_sanc_g-a-e_brm-preds.Rds")
resp_18 <- read_rds("data/input/by-question_cces-2018.Rds")

cd_mrp <- cd_phat %>%
  group_by(cd) %>%
  summarize(p_mrp = sum(mean)/sum(count))

cd_raw <- resp_18 %>%
  group_by(q_label, cd) %>%
  summarize(p_raw = mean(response == "Y", na.rm = TRUE),
           p_wgt = weighted.mean(response == "Y", weight, na.rm = TRUE),
           n = n())

cd_sanc <- cd_raw %>%
  filter(q_label == "WitholdSanctuaryFunding")

## compare
gg_df <- cd_mrp %>%
  left_join(cd_sanc)

gg_temp <- gg_df %>%
  ggplot(aes(p_raw, p_wgt)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(limits = c(0, 1), labels = percent) +
  scale_y_continuous(limits = c(0, 1), labels = percent) +
  geom_point(alpha = 0.25) +
  coord_equal() +
  theme_classic()

sc_raw_wgt <- gg_temp + labs(x = "Raw Means", y = "Weighted")
sc_raw_mrp <- gg_temp + aes(p_raw, p_mrp) + labs(x = "Raw Means", y = "MRP")
sc_wgt_mrp <- gg_temp + aes(p_wgt, p_mrp) + labs(x = "Weighted", y = "MRP")

sc_raw_wgt + sc_raw_mrp + sc_wgt_mrp
