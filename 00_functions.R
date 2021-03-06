# CODES

std_acs <- function(tbl, var_df = vars) {
  std_df <- tbl %>%
    filter(!str_detect(NAME, "Puerto Rico")) %>%
    rename(count = estimate,
           count_moe = moe)

  left_join(std_df, var_df, by = "variable") %>%
    select(year, everything())
}

#' get CD format
st_df <- tibble(st = state.abb, state = state.name) %>%
  add_row(st = "DC", state = "District of Columbia")

cd_name <- function(vec, st_to_state = st_df) {
  distnum <- vec %>%
    str_extract("([0-9]+|at Large)") %>%
    str_replace("at Large", "1") %>%
    str_pad(width = 2, pad = "0")
  cong <- vec %>% str_extract("1[01][0-9]")
  states <- vec %>% str_extract("(?<=,\\s)[A-z\\s]+")
  st <- map_chr(states, function(x) st_to_state$st[x == st_to_state$state])

  return(as.character(glue("{st}-{distnum}")))
}



transform_vars <- function(tbl) {
  tbl %>%
    mutate(male = -0.5 + 1 * (as_factor(gender) == "Male"),
           educ = as_factor(educ),
           age = as_factor(age))
}

pstrat = function(df, predicted, ...) {
  predicted_quo = rlang::enquo(predicted)
  group_vars = rlang::enquos(...)

  df %>%
    group_by(!!!group_vars) %>%
    summarize(!!predicted_quo := sum(!!predicted_quo * n / sum(n))) %>%
    ungroup()
}


# 03

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

fracs_longer <- function(tbl) {
  tbl %>%
    pivot_longer(cols = -c(geo:acs_frac),
                 values_to = "cces_frac",
                 names_to  = "weight_type",
                 names_pattern = "cces_(ufrac|wfrac|sfrac)") %>%
    ungroup() %>%
    mutate(geo_fct = recode_factor(as.character(geo),
                                   us = "National",
                                   st = "State-by-State",
                                   cd = "CD-by-CD"),  # %>%
           wgt_fct = recode_factor(as.character(weight_type),
                                   `ufrac` = "Unweighted",
                                   `wfrac` = "YouGov weights",
                                   `sfrac` = "State-by-state rim weights"))
}


calc_errors <- function(tbl, accr = 0.1) {
  pp <- unit_format(accuracy = accr, scale = 1e2, unit = "pp")
  tbl %>%
    group_by(geo, geo_fct, weight_type, wgt_fct) %>%
    summarize(RMSE = sqrt(mean((acs_frac - cces_frac)^2)),
              bias = mean(abs(acs_frac - cces_frac)),
              n = n()) %>%
    mutate(txt = str_c("RMSE: ", pp(RMSE), "\nBias: ", pp(bias))) %>%
    filter(!is.na(txt)) %>%
    ungroup()
}