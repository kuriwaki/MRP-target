

if (FALSE) {
  w_df_st <- matrix(NA,
                    nrow = nrow(cc18_uw),
                    ncol = length(u_stids),
                    dimnames = list(NULL, u_stids)) %>%
    as_tibble()

  # loop over states ---
  for (st_i in u_stids) {

    target_i <- targets_st %>%
      filter(stid == st_i) %>%
      mutate(g_a_e = str_c(gender, age, educ))

    uni <- universe(
      data = cc18_uw,
      category(
        name = "g_a_e",
        buckets = target_i$g_a_e,
        targets = target_i$frac_geo
      )
    )

    df_wgt <- iterake(universe = uni,
                      max.iter = 100,
                      max.wgt = 15,
                      wgt.name = "weight_st")

    w_df_st[, st_i] <- df_wgt$weight_st
  }


  # CDs ---------
  # container
  # year
  targets_cd <- cd_frac %>%
    filter(year == 2017)
  u_cds <- unique(targets_cd$cd)
  u_cdids  <- unique(targets_cd$cdid)

  w_df_cd <- matrix(NA,
                    nrow = nrow(cc18),
                    ncol = length(u_cdids),
                    dimnames = list(NULL, u_cdids)) %>%
    as_tibble()


  # loop over CDs ---
  for (dist_i in u_cdids) {

    target_i <- targets_cd %>%
      filter(cdid == dist_i) %>%
      mutate(g_a_e = str_c(gender, age, educ))

    uni <- universe(
      data = cc18,
      category(
        name = "g_a_e",
        buckets = target_i$g_a_e,
        targets = target_i$frac_geo
      )
    )

    df_wgt <- iterake(universe = uni,
                      max.iter = 100,
                      max.wgt = 15,
                      wgt.name = "weight_cd")

    w_df_cd[, dist_i] <- df_wgt$weight_cd
  }

} # end multicolumn weighting




# long, store ------

w_long_st <- w_df_st %>%
  mutate(row_id = 1:n(),
         geo = "st") %>%
  pivot_longer(-c(row_id, geo),
               names_to = "geoid",
               values_to = "weight")

w_long_cd <- w_df_cd %>%
  mutate(row_id = 1:n(),
         geo = "cd") %>%
  pivot_longer(-c(row_id, geo),
               names_to = "geoid",
               values_to = "weight",
               values_drop_na = TRUE)

w_df <- bind_rows(w_long_st, w_long_cd)
write_rds(w_df, "data/output/weights-rim_long.Rds")
