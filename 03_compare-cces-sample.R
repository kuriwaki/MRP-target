library(tidyverse)
library(haven)
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
           cces_wn_geo = sum(cces_wn, na.rm = TRUE)) %>%
    mutate(cces_n = replace_na(cces_n, 0),
           cces_wn = replace_na(cces_wn, 0),
           cces_frac = cces_n / cces_n_geo,
           cces_wfrac = cces_wn / cces_wn_geo)
}


resp_18 <- read_rds("data/input/by-question_cces-2018.Rds")
all_frac <- read_rds("data/output/by-national_ACS_gender-age-education.Rds")
cd_frac <- read_rds("data/output/by-CD_ACS_gender-age-education.Rds")
st_frac <- read_rds("data/output/by-ST_ACS_gender-age-education.Rds")

load("data/output/variable-labels.Rdata")

cc18 <- resp_18 %>%
  select(year:marstat) %>%
  distinct()


# unweighted raw counts
cc_u_all <- count(cc18, gender, age, educ, name = "cces_n")
cc_u_st <- count(cc18, state, gender, age, educ, name = "cces_n")
cc_u_cd <- count(cc18, cd, gender, age, educ, name = "cces_n")

# weighted versions
cc_w_all <- count(cc18, gender, age, educ, name = "cces_wn", wt = weight)
cc_w_st <- count(cc18, state, gender, age, educ, name = "cces_wn", wt = weight)
cc_w_cd <- count(cc18, cd, gender, age, educ, name = "cces_wn", wt = weight)

# start aggregating by geo
all_count  <- left_join(cc_u_all, cc_w_all, by = c("age", "gender", "educ"))
st_count  <-  left_join(cc_u_st, cc_w_st, by = c("state", "age", "gender", "educ"))
cd_count  <-  left_join(cc_u_cd, cc_w_cd, by = c("cd", "age", "gender", "educ"))


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
fracs_wide <- bind_rows(cd_cell_compr, st_cell_compr, all_cell_compr) %>%
  select(geo, cdid:cd, stid:state, matches("frac"))

fracs_long <- fracs_wide %>%
  pivot_longer(cols = -c(geo:frac_geo),
               values_to = "cces_frac",
               names_to  = "weighted",
               names_pattern = "cces_(frac|wfrac)") %>%
  rename(frac_acs = frac_geo) %>%
  mutate(weighted = weighted == "wfrac")

pp <- unit_format(accuracy = 0.01, scale = 1e2, unit = "pp")
errs <- fracs_long %>%
  filter(cces_frac > 0) %>%
  group_by(geo, weighted) %>%
  summarize(RMSE = sqrt(mean((frac_acs - cces_frac)^2)),
            bias = mean(abs(frac_acs - cces_frac)),
            n = n()) %>%
  mutate(txt = glue("RMSE: {pp(RMSE)}\nBias: {pp(bias)}")) %>%
  ungroup()


# get a sense of relationship ----
gg_u_temp <- cd_cell_compr %>%
  ggplot(aes(frac_geo, cces_frac)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
  coord_equal() +
  scale_x_continuous(limits = c(0, 0.108), breaks = seq(0, 0.1, 0.02), labels = percent_format(accuracy = 2)) +
  scale_y_continuous(limits = c(0, 0.108), breaks = seq(0, 0.1, 0.02), labels = percent_format(accuracy = 2)) +
  theme_classic() +
  labs(x = "Proportion in Geography (ACS)",
       y = "CCES Estimate") +
  coord_capped_cart(bottom = "both", left = "both") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 10),
        axis.title = element_text(size = 8))

x1 <- 0.07
y1 <- 0.09
pt <- 3
tc <- "navy"
gg_u_al <- gg_u_temp %+% all_cell_compr  + labs(title = "Nation, unweighted") + geom_point(size = 1) +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "nat" & !errs$weighted], size = pt, color = tc) +
  annotate("text", x = 0.025, y = 0.0637, label = "Female, 45-64, HS\nin nation", size = pt - 1) +
  annotate("curve", x = 0.035, y = 0.061, xend = 0.044, yend = 0.062, arrow = arrow(length = unit(0.05, "inches")))
gg_w_al <- gg_u_temp %+% all_cell_compr + aes(y = cces_wfrac)  + labs(title = "Nation, weighted", y = "CCES Estimate") +  geom_point(size = 1) +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "nat" & errs$weighted], size = pt, color = tc)
gg_u_st <- gg_u_temp %+% st_cell_compr + labs(title = "State-by-State, unweighted") + geom_point(alpha = 0.4, size = 0.3) +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "st" & !errs$weighted], size = pt, color = tc) +
  annotate("text",  x = 0.020, y = 0.075, label = "Female, 45-64, HS\nin Michigan", size = pt - 1) +
  annotate("curve", x = 0.022, y = 0.066, xend = 0.049, yend = 0.0705, arrow = arrow(length = unit(0.05, "inches")))
gg_w_st <- gg_u_temp %+% st_cell_compr + aes(y = cces_wfrac) + geom_point(alpha = 0.4, size = 0.3) + labs(title = "State-by-State, weighted", y = "CCES Estimate") +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "st" & errs$weighted], size = pt, color = tc)
gg_u_cd <- gg_u_temp + labs(title = "CD-by-CD, unweighted") + geom_point(alpha = 0.2, size = 0.01) +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "cd" & !errs$weighted], size = pt, color = tc)
gg_w_cd <- gg_u_temp + aes(y = cces_wfrac) + geom_point(alpha = 0.2, size = 0.01) + labs(title = "CD-by-CD, weighted", y = "CCES Estimate") +
  annotate("text", x = x1, y = y1, label = errs$txt[errs$geo == "cd" & errs$weighted], size = pt, color = tc)

gg_u_al + gg_w_al +  gg_u_st +  gg_w_st + gg_u_cd + gg_w_cd  + plot_layout(nrow = 2, byrow = FALSE) +
  plot_annotation(caption = "Source: CCES 2018, ACS 1yr 2017. All CCES weighting uses YouGov's national weights, even for state/CD subsets in middle/right panels.
  CCES/ACS estimate the proportion of a {gender x age bin x education} cell (60 combinations) per geography (1 nation, 50 states, or 435 CDs).")
ggsave("figures/cellfrac-comparisons.pdf", h = 5 + 1.2, w = 5*1.5 + 0.8)


