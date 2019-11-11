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


cd_weighted <- resp_18 %>%
  bind_rows(stack_turn) %>%
  left_join(select(new_wts, case_id, weight_st), by = "case_id") %>%
  group_by(q_label, cd) %>%
  summarize(p_raw = mean(response == "Y", na.rm = TRUE),
            p_yg = weighted.mean(response == "Y", weight, na.rm = TRUE),
            p_st = weighted.mean(response == "Y", weight_st, na.rm = TRUE),
            n = n())


outcomes <- c("ahca", "budg", "immr", "visa", "tcja", "sanc", "turn")
cces_names <- setNames(c("AHCA", "BudgetBipartisan", "ImmigrationRyan",  "EndVisaLottery",
                         "TaxCutJobsAct", "WitholdSanctuaryFunding", "Turnout"), nm = outcomes)

sd <- c("01", "02", "05")[1]
cells_all <- dir_ls(glue("data/output/cells/sd-{sd}"), recurse = TRUE)
outcomes_s <- unique(str_extract(cells_all, str_c("(", str_c(outcomes, collapse = "|"), ")")))

cces_cd <- foreach(y = outcomes_s, .combine = "bind_rows") %do% {

  ests <- dir_ls(glue("data/output/cells/sd-{sd}/{y}"))
  cd_phat <- foreach(cd = ests, .combine = "bind_rows") %do% {read_rds(cd)}
  df_phat <- left_join(cd_strat_all, cd_phat, by = c("cd", "age", "educ", "male"))

  cd_mrp <- df_phat %>%
    mutate(q_label = cces_names[y]) %>%
    filter(!is.na(mean)) %>%
    group_by(q_label, cd) %>%
    summarize(p_mrp = sum(mean)/sum(count))

  inner_join(cd_weighted, cd_mrp) %>%
    select(q_label, cd, n, matches("p_"))
}

write_rds(cces_cd, glue("data/output/by-cd-issue_all-estimates_sd-{sd}.Rds"))
