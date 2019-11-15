std_acs <- function(tbl, var_df = vars) {
  std_df <- tbl %>%
    filter(!str_detect(NAME, "Puerto Rico")) %>%
    rename(count = estimate,
           count_moe = moe)

  inner_join(var_df, std_df, by = "variable") %>%
    select(year, everything())
}




st_df <- tibble(st = state.abb, state = state.name) %>%
  add_row(st = "DC", state = "District of Columbia")

cd_name <- function(vec, st_to_state = st_df) {
  distnum <- vec %>% str_extract("([0-9]+|at Large)") %>%
    str_replace("at Large", "1")
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
