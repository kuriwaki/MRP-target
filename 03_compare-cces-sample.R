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
           cces_frac = cces_n / cces_n_geo,
           cces_wfrac = cces_wn / cces_wn_geo,
           cces_sfrac = cces_sn / cces_sn_geo)
}


cc18_raw <- read_rds("data/input/by-person_cces-2018.Rds")
weights  <- read_rds("data/output/weights-state.Rds")

us_educ_frac <- read_rds("data/output/by-us_ACS_gender-age-education.Rds")
cd_educ_frac <- read_rds("data/output/by-cd_ACS_gender-age-education.Rds")
st_educ_frac <- read_rds("data/output/by-st_ACS_gender-age-education.Rds")
us_race_frac <- read_rds("data/output/by-us_ACS_gender-age-race.Rds")
cd_race_frac <- read_rds("data/output/by-cd_ACS_gender-age-race.Rds")
st_race_frac <- read_rds("data/output/by-st_ACS_gender-age-race.Rds")

load("data/output/variable-labels.Rdata")

cc18 <- left_join(cc18_raw, weights, by = c("case_id", "weight"))


# unweighted raw counts
ccu_us_educ  <- count(cc18, gender, age, educ, name = "cces_n")
ccu_st_educ  <- count(cc18, state, gender, age, educ, name = "cces_n")
ccu_cd_educ  <- count(cc18, cd, gender, age, educ, name = "cces_n")

ccu_us_race  <- count(cc18, gender, age, race, name = "cces_n")
ccu_st_race  <- count(cc18, state, gender, race, educ, name = "cces_n")
ccu_cd_race  <- count(cc18, cd, gender, age, race, name = "cces_n")

# weighted versions
cc_w_us <- count(cc18, gender, age, educ, name = "cces_wn", wt = weight)
cc_w_st <- count(cc18, state, gender, age, educ, name = "cces_wn", wt = weight)
cc_w_cd <- count(cc18, cd, gender, age, educ, name = "cces_wn", wt = weight)

# state weighted versions
cc_s_us <- count(cc18, gender, age, educ, name = "cces_sn", wt = weight_st)
cc_s_st <- count(cc18, state, gender, age, educ, name = "cces_sn", wt = weight_st)
cc_s_cd <- count(cc18, cd, gender, age, educ, name = "cces_sn", wt = weight_st)


# start aggregating by geo
all_count  <- left_join(cc_u_all, cc_w_all, by = c("age", "gender", "educ")) %>%
  left_join(cc_s_all)
st_count  <-  left_join(cc_u_st, cc_w_st, by = c("state", "age", "gender", "educ")) %>%
  left_join(cc_s_st)
cd_count  <-  left_join(cc_u_cd, cc_w_cd, by = c("cd", "age", "gender", "educ")) %>%
  left_join(cc_s_cd)


# compare with CD state
all_cell_compr <- left_join(filter(all_frac, year == 2017), all_count,
                            by = c("gender", "age", "educ")) %>%
  group_by(geo) %>%
  compute_fracs()

st_cell_compr <- left_join(filter(st_frac, year == 2017), st_count,
                           by = c("state", "gender", "age", "educ")) %>%
  group_by(state) %>%
  compute_fracs()

cd_cell_compr <- left_join(filter(cd_frac, year == 2017), cd_count,
          by = c("cd", "gender", "age", "educ")) %>%
  group_by(cd) %>%
  compute_fracs()


# stats on RMSE ---
fracs_wide <- bind_rows(zap_labels(cd_cell_compr),
                        zap_labels(st_cell_compr),
                        zap_labels(all_cell_compr)) %>%
  select(geo, cdid:cd, stid:state, matches("frac"))

fracs_long <- fracs_wide %>%
  pivot_longer(cols = -c(geo:frac_geo),
               values_to = "cces_frac",
               names_to  = "weight_type",
               names_pattern = "cces_(frac|wfrac|sfrac)") %>%
  rename(frac_acs = frac_geo) %>%
  mutate(geo_fct = recode_factor(geo,
                                 nat = "National",
                                 st = "State-by-State",
                                 cd = "CD-by-CD"),  # %>%
         wgt_fct = to_labelled(
           recode_factor(as.character(weight_type),
                         `frac` = "Unweighted",
                         `wfrac` = "YouGov weights",
                         `sfrac` = "State-by-state rim weights")))

pp <- unit_format(accuracy = 0.01, scale = 1e2, unit = "pp")
errs <- fracs_long %>%
  group_by(geo, w_type) %>%
  summarize(RMSE = sqrt(mean((frac_acs - cces_frac)^2)),
            bias = mean(abs(frac_acs - cces_frac)),
            n = n()) %>%
  mutate(txt = glue("RMSE: {pp(RMSE)}\nBias: {pp(bias)}")) %>%
  ungroup()
errs


# get a sense of relationship ----
gg_u_temp <- cd_cell_compr %>%
  ggplot(aes(frac_geo, cces_frac)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 0.108), breaks = seq(0, 0.1, 0.02), labels = percent_format(accuracy = 2)) +
  scale_y_continuous(limits = c(0, 0.108), breaks = seq(0, 0.1, 0.02), labels = percent_format(accuracy = 2)) +
  theme_classic() +
  labs(x = "Proportion in Geography (Reported by ACS)",
       y = "CCES Estimate") +
  coord_capped_cart(bottom = "both", left = "both") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.title = element_text(size = 8))

x1 <- 0.07
y1 <- 0.09
pt <- 3
tc <- "navy"
gg_u_al <- gg_u_temp %+% all_cell_compr  + labs(title = "Nation, unweighted") + geom_point(size = 1) +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "nat" & errs$w_type == "frac"], size = pt, color = tc) +
  annotate("text", x = 0.025, y = 0.0637, label = "Female, 45-64, HS\nin nation", size = pt - 1) +
  annotate("curve", x = 0.035, y = 0.061, xend = 0.044, yend = 0.062, arrow = arrow(length = unit(0.05, "inches")))
gg_w_al <- gg_u_temp %+% all_cell_compr + aes(y = cces_wfrac)  + labs(title = "Nation, YouGov weights", y = "CCES Estimate") +  geom_point(size = 1) +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "nat" & errs$w_type == "wfrac"], size = pt, color = tc)
gg_u_st <- gg_u_temp %+% st_cell_compr + labs(title = "State-by-State, unweighted") + geom_point(alpha = 0.4, size = 0.3) +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "st" & errs$w_type == "frac"], size = pt, color = tc) +
  annotate("text",  x = 0.020, y = 0.075, label = "Female, 45-64, HS\nin Michigan", size = pt - 1) +
  annotate("curve", x = 0.022, y = 0.066, xend = 0.049, yend = 0.0705, arrow = arrow(length = unit(0.05, "inches")))
gg_w_st <- gg_u_temp %+% st_cell_compr + aes(y = cces_wfrac) + geom_point(alpha = 0.4, size = 0.3) + labs(title = "State-by-State, YouGov weights", y = "CCES Estimate") +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "st" & errs$w_type == "wfrac"], size = pt, color = tc)
gg_u_cd <- gg_u_temp + labs(title = "CD-by-CD, unweighted") + geom_point(alpha = 0.2, size = 0.01) +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "cd" & errs$w_type == "frac"], size = pt, color = tc)
gg_w_cd <- gg_u_temp + aes(y = cces_wfrac) + geom_point(alpha = 0.2, size = 0.01) + labs(title = "CD-by-CD, YouGov weights", y = "CCES Estimate") +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "cd" & errs$w_type == "wfrac"], size = pt, color = tc)

gg_u_al + gg_w_al +   gg_u_st +  gg_w_st  + gg_u_cd + gg_w_cd + plot_layout(ncol = 3, byrow = FALSE) +
  plot_annotation(caption = "Source: CCES 2018, ACS 1yr 2017. All CCES weighting uses YouGov's national weights, even for state/CD subsets in middle/right panels.
  CCES/ACS estimate the proportion of a {gender x age bin x education} cell (60 combinations) per geography (1 nation, 50 states, or 435 CDs).")
ggsave("figures/cellfrac-comparisons.pdf", h = 5 + 1.2, w = 5*1.5 + 0.8)
ggsave("figures/cellfrac-comparisons.png", h = 5 + 1.2, w = 5*1.5 + 0.8)


gg_s_al <- gg_u_temp %+% all_cell_compr + aes(y = cces_sfrac)  + labs(title = "Nation", y = "CCES Estimate") +  geom_point(size = 1) +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "nat" & errs$w_type == "sfrac"], size = pt, color = tc)
gg_s_st <- gg_u_temp %+% st_cell_compr + aes(y = cces_sfrac) + geom_point(alpha = 0.4, size = 0.3) + labs(title = "State-by-State", y = "CCES Estimate") +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "st" & errs$w_type == "sfrac"], size = pt, color = tc)
gg_s_cd <- gg_u_temp + aes(y = cces_sfrac) + geom_point(alpha = 0.2, size = 0.01) + labs(title = "CD-by-CD", y = "CCES Estimate") +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "cd" & errs$w_type == "sfrac"], size = pt, color = tc)

gg_s_al + gg_s_st + gg_s_cd  + plot_layout(ncol = 3, byrow = FALSE) +
  plot_annotation(caption = "CCES uses custom state-by-state rim weights.")
ggsave("figures/cellfrac-rim-comparisons.png", h = 3, w = 2.5*3)
ggsave("figures/cellfrac-rim-comparisons.pdf", h = 3, w = 2.5*3)


# education only ----
# paste earlier, but without age and education
# unweighted raw counts
compr <- bind_rows(zap_labels(all_cell_compr),
                   zap_labels(st_cell_compr),
                   zap_labels(cd_cell_compr))
cces_count_ed <-  compr %>%
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
                                 nat = "National",
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
  mutate(txt = glue("RMSE: {pp(RMSE)}\nBias: {pp(bias)}")) %>%
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
ggsave("figures/educfrac-rim-comparisons.pdf", gg_ed_rim, h = 3, w = 2.5*3)
