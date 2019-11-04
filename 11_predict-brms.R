predicted_d = fitted(fit, newdata = cd_strat, allow_new_levels = TRUE, summary = FALSE)

pstrat = function(df, predicted, ...) {
  predicted_quo = rlang::enquo(predicted)
  group_vars = rlang::enquos(...)

  df %>%
    group_by(!!!group_vars) %>%
    summarize(!!predicted_quo := sum(!!predicted_quo * n / sum(n))) %>%
    ungroup()
}

cd_df_ahca = mclapply(1:nrow(predicted_d), function(i) {
  cd_strat %>%
    mutate(predicted = (predicted_d[i, ] / n)) %>%
    pstrat(predicted, state) %>%
    mutate(rep = i)
}, mc.cores = 4) %>%
  bind_rows()

write_rds(cd_df_ahca, "data/output/mrp/by-cd_ahca-estimates.Rds")