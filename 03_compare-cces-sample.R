library(tidyverse)
library(haven)
library(labelled)
library(lemon)
library(ggthemes)
library(patchwork)
library(scales)
library(glue)

# calculate fraction after getting left-joined to full cells
compute_fracs <- function(grptbl) {
  stopifnot(is.grouped_df(grptbl))

  grptbl %>%
    mutate(cces_n_geo = sum(cces_n, na.rm = TRUE),
           cces_wn_geo = sum(cces_wn, na.rm = TRUE),
           cces_sn_geo = sum(cces_sn, na.rm = TRUE)) %>%
    mutate(cces_n = replace_na(cces_n, 0),
           cces_wn = replace_na(cces_wn, 0),
           cces_sn = replace_na(cces_sn, 0),
           cces_ufrac = cces_n / cces_n_geo,
           cces_wfrac = cces_wn / cces_wn_geo,
           cces_sfrac = cces_sn / cces_sn_geo)
}


cc18_raw <- read_rds("data/input/by-person_cces-2018.Rds")
weights  <- read_rds("data/output/weights-state.Rds")

us_educ_frac <- read_rds("data/output/by-us_ACS_gender-age-education.Rds") %>% filter(year == 2017)
cd_educ_frac <- read_rds("data/output/by-cd_ACS_gender-age-education.Rds") %>% filter(year == 2017)
st_educ_frac <- read_rds("data/output/by-st_ACS_gender-age-education.Rds") %>% filter(year == 2017)
us_race_frac <- read_rds("data/output/by-us_ACS_gender-age-race.Rds") %>% filter(year == 2017)
cd_race_frac <- read_rds("data/output/by-cd_ACS_gender-age-race.Rds") %>% filter(year == 2017)
st_race_frac <- read_rds("data/output/by-st_ACS_gender-age-race.Rds") %>% filter(year == 2017)

load("data/output/variable-labels.Rdata")

cc18 <- left_join(cc18_raw, weights, by = c("case_id", "weight"))


# unweighted raw counts
ccu_us_educ  <- count(cc18, gender, age, educ, name = "cces_n")
ccu_st_educ  <- count(cc18, state, gender, age, educ, name = "cces_n")
ccu_cd_educ  <- count(cc18, cd, gender, age, educ, name = "cces_n")
ccu_us_race  <- count(cc18, gender, age, race, name = "cces_n")
ccu_st_race  <- count(cc18, state, gender, age, race, name = "cces_n")
ccu_cd_race  <- count(cc18, cd, gender, age, race, name = "cces_n")

# weighted versions
ccw_us_educ <- count(cc18, gender, age, educ, name = "cces_wn", wt = weight)
ccw_st_educ <- count(cc18, state, gender, age, educ, name = "cces_wn", wt = weight)
ccw_cd_educ <- count(cc18, cd, gender, age, educ, name = "cces_wn", wt = weight)
ccw_us_race <- count(cc18, gender, age, race, name = "cces_wn", wt = weight)
ccw_st_race <- count(cc18, state, gender, age, race, name = "cces_wn", wt = weight)
ccw_cd_race <- count(cc18, cd, gender, age, race, name = "cces_wn", wt = weight)

# state weighted versions
ccs_us_educ <- count(cc18, gender, age, educ, name = "cces_sn", wt = weight_st)
ccs_st_educ <- count(cc18, state, gender, age, educ, name = "cces_sn", wt = weight_st)
ccs_cd_educ <- count(cc18, cd, gender, age, educ, name = "cces_sn", wt = weight_st)
ccs_us_race <- count(cc18, gender, age, race, name = "cces_sn", wt = weight_st)
ccs_st_race <- count(cc18, state, gender, age, race, name = "cces_sn", wt = weight_st)
ccs_cd_race <- count(cc18, cd, gender, age, race, name = "cces_sn", wt = weight_st)

# start aggregating by geo
cc_us_educ  <- left_join(ccu_us_educ, ccw_us_educ, by = c("age", "gender", "educ")) %>%
  left_join(ccs_us_educ)
cc_st_educ  <- left_join(ccu_st_educ, ccw_st_educ, by = c("state", "age", "gender", "educ")) %>%
  left_join(ccs_st_educ)
cc_cd_educ  <- left_join(ccu_cd_educ, ccw_cd_educ, by = c("cd", "age", "gender", "educ")) %>%
  left_join(ccs_cd_educ)
cc_us_race  <- left_join(ccu_us_race, ccw_us_race, by = c("age", "gender", "race")) %>%
  left_join(ccs_us_race)
cc_st_race  <- left_join(ccu_st_race, ccw_st_race, by = c("state", "age", "gender", "race")) %>%
  left_join(ccs_st_race)
cc_cd_race  <- left_join(ccu_cd_race, ccw_cd_race, by = c("cd", "age", "gender", "race")) %>%
  left_join(ccs_cd_race)


# compare  ---------
comp_us_educ <- us_educ_frac %>%
  left_join(cc_us_educ, by = c("gender", "age", "educ")) %>%
  group_by(geo) %>%
  compute_fracs()

comp_st_educ <- st_educ_frac %>%
  left_join(cc_st_educ, by = c("state", "gender", "age", "educ")) %>%
  group_by(state) %>%
  compute_fracs()

comp_cd_educ <- cd_educ_frac %>%
  left_join(cc_cd_educ, by = c("cd", "gender", "age", "educ")) %>%
  group_by(cd) %>%
  compute_fracs()

comp_us_race <- us_race_frac %>%
  left_join(cc_us_race, by = c("gender", "age", "race")) %>%
  group_by(geo) %>%
  compute_fracs()

comp_st_race <- st_race_frac %>%
  left_join(cc_st_race, by = c("state", "gender", "age", "race")) %>%
  group_by(state) %>%
  compute_fracs()

comp_cd_race <- cd_race_frac %>%
  left_join(cc_cd_race, by = c("cd", "gender", "age", "race")) %>%
  group_by(cd) %>%
  compute_fracs()


# stats on RMSE ---
fracs_wide <- bind_rows(zap_labels(comp_us_educ),
                        zap_labels(comp_st_educ),
                        zap_labels(comp_cd_educ))

fracs_long <- fracs_wide %>%
  select(geo, cdid:cd, stid:state, gender, age, educ, matches("frac")) %>%
  pivot_longer(cols = -c(geo:frac_geo),
               values_to = "cces_frac",
               names_to  = "weight_type",
               names_pattern = "cces_(ufrac|wfrac|sfrac)") %>%
  rename(frac_acs = frac_geo) %>%
  ungroup() %>%
  mutate(geo_fct = recode_factor(as.character(geo),
                                 us = "National",
                                 st = "State-by-State",
                                 cd = "CD-by-CD"),  # %>%
         wgt_fct = recode_factor(as.character(weight_type),
                                 `ufrac` = "Unweighted",
                                 `wfrac` = "YouGov weights",
                                 `sfrac` = "State-by-state rim weights"))

pp <- unit_format(accuracy = 0.01, scale = 1e2, unit = "pp")
errs <- fracs_long %>%
  group_by(geo, geo_fct, weight_type, wgt_fct) %>%
  summarize(RMSE = sqrt(mean((frac_acs - cces_frac)^2)),
            bias = mean(abs(frac_acs - cces_frac)),
            n = n()) %>%
  mutate(txt = str_c("RMSE: ", pp(RMSE), "\nBias: ", pp(bias))) %>%
  filter(!is.na(txt)) %>%
  ungroup()
errs


# get a sense of relationship ----
gg_cell_temp <- fracs_long  %>%
  filter(weight_type %in% c("ufrac", "wfrac")) %>%
  ggplot(aes(frac_acs, cces_frac)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
  coord_equal() +
  geom_point(aes(size = geo_fct, alpha = geo_fct)) +
  scale_size_manual(values  = c("National" = 1, "State-by-State" = 0.3, "CD-by-CD" = 0.01)) +
  scale_alpha_manual(values = c("National" = 1, "State-by-State" = 0.5, "CD-by-CD" = 0.2)) +
  scale_x_continuous(limits = c(0, 0.108), breaks = seq(0, 0.1, 0.02), labels = percent_format(accuracy = 2)) +
  scale_y_continuous(limits = c(0, 0.108), breaks = seq(0, 0.1, 0.02), labels = percent_format(accuracy = 2)) +
  labs(x = "Proportion in Geography (Reported by ACS)",
       y = "CCES Estimate") +
  coord_capped_cart(bottom = "both", left = "both") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.title = element_text(size = 10),
        strip.text = element_text(face = "bold", size = 10),
        strip.background = element_rect(color = "transparent", fill = "transparent"),
        legend.position = "bottom") +
  guides(size = FALSE, alpha = FALSE) +
  labs(caption = "Source: CCES 2018, ACS 1yr 2017. All CCES weighting uses YouGov's national weights, even for state/CD subsets in middle/right panels.
  CCES/ACS estimate the proportion of a {gender x age bin x education} cell (60 combinations) per geography (1 nation, 50 states, or 435 CDs)")

gg_cell_educ <- gg_cell_temp +
  facet_rep_grid(wgt_fct ~ geo_fct, repeat.tick.labels = TRUE) +
  geom_text(data = mutate_if(filter(errs, wgt_fct %in% c("Unweighted", "YouGov weights")), is.labelled, as_factor),
            aes(x = 0.08, y = 0.02, label = txt), size = 3)

ggsave("figures/cellfrac-comparisons.pdf", gg_cell_educ, h = 5 + 1.2, w = 5*1.5 + 0.8)
ggsave("figures/cellfrac-comparisons.png", gg_cell_educ, h = 5 + 1.2, w = 5*1.5 + 0.8)

gg_cell_rim <- gg_cell_temp %+% as_factor(filter(fracs_long, wgt_fct == "State-by-state rim weights"))  +
  facet_rep_grid( ~ geo_fct, repeat.tick.labels = TRUE) +
  geom_text(data = as_factor(filter(errs, weight_type %in% "sfrac")),
            aes(x = 0.08, y = 0.02, label = txt), size = 3) +
  labs(caption = "CCES uses custom state-by-state rim weights.") +
  guides(color = FALSE)

ggsave("figures/cellfrac-rim-comparisons.png", h = 3, w = 2.5*3)
ggsave("figures/cellfrac-rim-comparisons.pdf", h = 3, w = 2.5*3)


# education only ----
# paste earlier, but without age and education
# unweighted raw counts

cces_count_ed <-  fracs_wide %>%
  group_by(educ, geo, state, cd) %>%
  summarize(acs_frac = sum(count) / unique(count_geo),
            cces_frac = sum(cces_n) / unique(cces_n_geo),
            cces_wfrac = sum(cces_wn) / unique(cces_wn_geo),
            cces_sfrac = sum(cces_sn) / unique(cces_sn_geo))

ed_long <- cces_count_ed %>%
  pivot_longer(cols = -c(educ, geo:acs_frac),
               values_to = "cces_frac",
               names_to  = "weight_type",
               names_pattern = "cces_(frac|wfrac|sfrac)") %>%
  ungroup() %>%
  mutate(geo_fct = recode_factor(geo,
                                 us = "National",
                                 st = "State-by-State",
                                 cd = "CD-by-CD"),  # %>%
         wgt_fct = to_labelled(recode_factor(as.character(weight_type),
                                 `frac` = "Unweighted",
                                 `wfrac` = "YouGov weights",
                                 `sfrac` = "State-by-state rim weights"))) %>%
  left_join(distinct(transmute(educ_key,
                               educ_fct = as_factor(educ),
                               educ = as.integer(educ))))


pp <- unit_format(accuracy = 0.1, scale = 1e2, unit = "pp")
errs_ed <- ed_long %>%
  group_by(geo_fct, wgt_fct) %>%
  summarize(RMSE = sqrt(mean((acs_frac - cces_frac)^2)),
            bias = mean(abs(acs_frac - cces_frac)),
            n = n()) %>%
  mutate(txt = str_c("RMSE: ", pp(RMSE), "\nBias: ", pp(bias))) %>%
  ungroup()


gg_ed_temp <- ed_long  %>%
  filter(wgt_fct %in% 1:2) %>%
  mutate(wgt_fct = as_factor(wgt_fct)) %>%
  ggplot(aes(acs_frac, cces_frac)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
  coord_equal() +
  geom_point(aes(size = geo_fct, alpha = geo_fct, color = educ_fct)) +
  scale_color_viridis_d(end = 0.8) +
  scale_size_manual(values  = c("National" = 2, "State-by-State" = 0.3, "CD-by-CD" = 0.05)) +
  scale_alpha_manual(values = c("National" = 1, "State-by-State" = 1.0, "CD-by-CD" = 0.5)) +
  scale_x_continuous(limits = c(0, 0.54), breaks = seq(0, 0.5, 0.25), labels = percent_format(accuracy = 1)) +
  scale_y_continuous(limits = c(0, 0.54), breaks = seq(0, 0.5, 0.25), labels = percent_format(accuracy = 1)) +
  labs(x = "Proportion in Geography (Reported by ACS)",
       y = "CCES Estimate") +
  coord_capped_cart(bottom = "both", left = "both") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.title = element_text(size = 10),
        strip.text = element_text(face = "bold", size = 10),
        strip.background = element_rect(color = "transparent", fill = "transparent"),
        legend.position = "bottom") +
  guides(size = FALSE,
         alpha = FALSE,
         color = guide_legend(title = "Education", nrow = 1, override.aes = list(size = 2))) +
  labs(caption = "Source: CCES 2018, ACS 1yr 2017. All CCES weighting uses YouGov's national weights, even for state/CD subsets in middle/right panels.
  CCES/ACS estimate the proportion of an education cell (6 combinations) per geography (1 nation, 50 states, or 435 CDs).")

gg_ed_uw <-  gg_ed_temp +
  facet_rep_grid(wgt_fct ~ geo_fct, repeat.tick.labels = TRUE) +
  geom_text(data = mutate_if(filter(errs_ed, wgt_fct %in% 1:2), is.labelled, as_factor),
            aes(x = 0.1, y = 0.4, label = txt), size = 3)

ggsave("figures/educfrac-comparisons.pdf", gg_ed_uw, h = 5 + 1.2, w = 5*1.5)

gg_ed_rim <- gg_ed_temp %+% as_factor(filter(ed_long, wgt_fct == 3))  +
  facet_rep_grid( ~ geo_fct, repeat.tick.labels = TRUE) +
  geom_text(data = as_factor(filter(errs_ed, wgt_fct %in% 3)),
            aes(x = 0.1, y = 0.4, label = txt), size = 3) +
  labs(caption = "CCES uses custom state-by-state rim weights.") +
  guides(color = FALSE)
gg_ed_rim
ggsave("figures/educfrac-rim-comparisons.pdf", gg_ed_rim, h = 3, w = 2.5*3)
