library(tidyverse)
library(lemon)
library(fs)
library(glue)
library(corrr)
library(foreach)
library(scales)

# copy from 10 --
transform_vars <- function(tbl) {
  tbl %>%
    mutate(male = -0.5 + 1 * (as_factor(gender) == "Male"),
           educ = as_factor(educ),
           age = as_factor(age))
}





resp_18 <- read_rds("data/input/by-question_cces-2018.Rds")
new_wts <- read_rds("data/output/weights-state.Rds")
cd_strat_all <- read_rds("data/output/by-cd_ACS_gender-age-education.Rds") %>%
  transform_vars()

stack_turn <- resp_18 %>%
  select(year:vv_turnout_pvm) %>%
  distinct() %>%
  mutate(q_label = "Turnout") %>%
  mutate(response = as.character(recode(vv_turnout_gvm, Voted = "Y", `No Record Of Voting` = "N")))

# For any proportion, s.e. = sqrt(p_w * (1-p_w)/ n_w), where p_w is the weighted
# average from the data, and n_w is the effective sample size, n/(1 + var(w)),
# where var(w) is the sample variances of the weights, nor

svy_obs <- resp_18 %>%
  bind_rows(stack_turn) %>%
  left_join(select(new_wts, case_id, weight_st), by = "case_id") %>%
  group_by(q_label, cd)

p_est <- svy_obs %>%
  summarize(p_raw = mean(response == "Y", na.rm = TRUE),
            p_yg = weighted.mean(response == "Y", weight, na.rm = TRUE),
            p_st = weighted.mean(response == "Y", weight_st, na.rm = TRUE))

n_eff <- svy_obs %>%
  summarize(n_raw = sum(!is.na(response)),
            n_yg  = sum(!is.na(response)) / (1 + var(weight / mean(weight_st))),
            n_st  = sum(!is.na(response)) / (1 + var(weight_st / mean(weight_st))),
  )

se_p <- function(p, n) sqrt(p*(1 - p) / n)
ests_eff <- left_join(p_est, n_eff) %>%
  mutate(se_raw = se_p(p_raw, n_raw),
         se_yg = se_p(p_yg, n_yg),
         se_st = se_p(p_st, n_st))



outcomes <- c("ahca", "budg", "immr", "visa", "tcja", "sanc", "turn")
cces_names <- setNames(c("AHCA", "BudgetBipartisan", "ImmigrationRyan",  "EndVisaLottery",
                         "TaxCutJobsAct", "WitholdSanctuaryFunding", "Turnout"), nm = outcomes)

sd <- c("default", "01", "02", "05")[4]
cells_all <- dir_ls(glue("data/output/cd-estimates/sd-{sd}"), recurse = TRUE)
outcomes_s <- unique(str_extract(cells_all, str_c("(", str_c(outcomes, collapse = "|"), ")")))


# Add mrp
cces_cd <- foreach(y = "turn", .combine = "bind_rows") %do% {

  ests <- dir_ls(glue("data/output/cd-estimates/stan_glmer/sd-{sd}/{y}"))
  cd_phat <- foreach(cd = ests, .combine = "bind_rows") %do% {read_rds(cd)}

  cd_mrp <- cd_phat %>%
    mutate(q_label = cces_names[y]) %>%
    group_by(q_label, cd) %>%
    summarize(p_mrp_est = mean(p_mrp),
              p_mrp_025 = quantile(p_mrp, 0.025),
              p_mrp_050 = quantile(p_mrp, 0.050),
              p_mrp_950 = quantile(p_mrp, 0.950),
              p_mrp_975 = quantile(p_mrp, 0.975))

  inner_join(ests_eff, cd_mrp) %>%
    select(q_label, cd, matches("n_"), matches("p_"), matches("se_"))
}

write_rds(cces_cd, glue("data/output/by-cd_glmer_sd-{sd}.Rds"))
