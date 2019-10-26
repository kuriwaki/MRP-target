library(tidyverse)
library(haven)
library(lemon)
library(ggthemes)
library(patchwork)

resp18 <- read_rds("data/output/by-question_cces-2018.Rds")
all_frac <- read_rds("data/output/by-national_ACS_gender-age-education.Rds")
cd_frac <- read_rds("data/output/by-CD_ACS_gender-age-education.Rds")
st_frac <- read_rds("data/output/by-ST_ACS_gender-age-education.Rds")


cc18_raw <- resp18 %>%
  select(year:marstat) %>%
  distinct()

# for age only, discretize to ACS format
cc18 <- cc18_raw %>%
  mutate(age_bin = case_when(age %in% 18:24 ~ 1L,
                             age %in% 25:34 ~ 2L,
                             age %in% 35:44 ~ 3L,
                             age %in% 45:64 ~ 4L,
                             age >=   65    ~ 5L,
                             TRUE ~ NA_integer_),
         age = labelled(age_bin, age_lbl))


# unweighted raw counts
cc_u_all <- count(cc18, gender, age, educ, name = "cces_n") %>%
  mutate(cces_n_dist = sum(cces_n)) %>%
  mutate(cces_frac = cces_n / cces_n_dist)

cc_u_st <- count(cc18, state, gender, age, educ, name = "cces_n") %>%
  group_by(state) %>%
  mutate(cces_n_dist = sum(cces_n)) %>%
  mutate(cces_frac = cces_n / cces_n_dist)

cc_u_cd <- count(cc18, cd, gender, age, educ, name = "cces_n") %>%
  group_by(cd) %>%
  mutate(cces_n_dist = sum(cces_n)) %>%
  mutate(cces_frac = cces_n / cces_n_dist)

# weighted versions
cc_w_all <- count(cc18, gender, age, educ, name = "cces_wn", wt = weight) %>%
  mutate(cces_wn_dist = sum(cces_wn)) %>%
  mutate(cces_wfrac = cces_wn / cces_wn_dist)

cc_w_st <- count(cc18, state, gender, age, educ, name = "cces_wn", wt = weight) %>%
  group_by(state) %>%
  mutate(cces_wn_dist = sum(cces_wn)) %>%
  mutate(cces_wfrac = cces_wn / cces_wn_dist)

cc_w_cd <- count(cc18, cd, gender, age, educ, name = "cces_wn", wt = weight) %>%
  group_by(cd) %>%
  mutate(cces_wn_dist = sum(cces_wn)) %>%
  mutate(cces_wfrac = cces_wn / cces_wn_dist)


# compare with CD state
all_cell_compr <- left_join(filter(all_frac, year == 2017), cc_u_all,
                           by = c("gender", "age", "educ")) %>%
  left_join(cc_w_all, by = c("gender", "age", "educ")) %>%
  mutate(cces_frac = replace_na(cces_frac, 0)) %>%
  mutate(cces_wfrac = replace_na(cces_wfrac, 0))

st_cell_compr <- left_join(filter(st_frac, year == 2017), cc_u_st,
                           by = c("state", "gender", "age", "educ")) %>%
  left_join(cc_w_st, by = c("state", "gender", "age", "educ")) %>%
  mutate(cces_frac = replace_na(cces_frac, 0)) %>%
  mutate(cces_wfrac = replace_na(cces_wfrac, 0))

cd_cell_compr <- left_join(filter(cd_frac, year == 2017), cc_u_cd,
          by = c("cd", "gender", "age", "educ")) %>%
  left_join(cc_w_cd, by = c("cd", "gender", "age", "educ")) %>%
  mutate(cces_frac = replace_na(cces_frac, 0)) %>%
  mutate(cces_wfrac = replace_na(cces_wfrac, 0))

# stats on RMSE ---
cd_cell_compr %>% mutate()


# get a sense of relationship ----

gg_u_temp <- sample_n(cd_cell_compr, 2e3) %>%
  ggplot(aes(frac_dist, cces_frac)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", alpha = 0.5) +
  coord_equal() +
  geom_point(alpha = 0.4, size = 0.1) +
  scale_x_continuous(limits = c(0, 0.10)) +
  scale_y_continuous(limits = c(0, 0.10)) +
  theme_classic() +
  labs(x = "ACS fraction",
       y = "CCES fraction") +
  coord_capped_cart(bottom = "both", left = "both") +
  theme(plot.title = element_text(hjust = 0.5))

gg_u_al <- gg_u_temp %+% all_cell_compr  + labs(title = "Nation Unweighted") + geom_point(size = 0.5)
gg_w_al <- gg_u_temp %+% all_cell_compr + aes(y = cces_wfrac)  + labs(title = "Nation Weighted", y = "CCES fraction") +  geom_point(size = 0.5)
gg_u_cd <- gg_u_temp + labs(title = "CD Unweighted")
gg_u_st <- gg_u_temp %+% st_cell_compr  + labs(title = "State Unweighted")
gg_w_cd <- gg_u_temp + aes(y = cces_wfrac) + labs(title = "CD Weighted", y = "CCES fraction")
gg_w_st <- gg_u_temp %+% st_cell_compr + aes(y = cces_wfrac)  + labs(title = "State Weighted", y = "CCES fraction")

gg_u_al + gg_w_al + gg_u_st +  gg_w_st + gg_u_cd + gg_w_cd  + plot_layout(nrow = 2, byrow = FALSE) +
  plot_annotation(caption = "Source: CCES 2018, ACS 5yr 2013-2017. CCES weights are YouGov's national weights, applied to subsets of states/CDs in middle/right panels.
  Estimates are proportion of a given Gender x Age Bin x Education cell in a state/CD. To avoid clutter, only up to 2,000 points shown in right panel.")
ggsave("figures/cellfrac-comparisons.pdf", h = 5 + 1, w = 5*1.5 + 0.8)

# State-level demographics
## unweighted

## weighted


# CD-level
## unweighted
## weighted to national