# tables ----
# Table of general results with total scallop counts and overall catch rates,
# with CVs
f_tbl_catch <- function(scal_catch, beds){

  scal_catch %>%
    filter(Size != "clapper") %>%
    group_by(Bed, Size) %>%
    summarize(tows = n(),
              catch = sum(count),
              est = mean(count / area_swept), # Q = 1
              CV  = 100 *
                round(sqrt(var(count / area_swept) / (tows-1)) / est, 2)) %>%
    left_join(beds) %>%
    dplyr::select(Bed,
                  Area = area_nm2,
                  Tows = tows,
                  Size,
                  Catch = catch,
                  CatchRate = est,
                  CV)  %T>%
    write_csv(here::here(paste0("output/", YEAR, "/bed_catch_results_tbl.csv")))

}

f_scallop_est <- function(scal_catch, beds, Q, abundance = TRUE, boot = FALSE){

  if(abundance){
    scal_catch %>%
      filter(Size != "clapper") %>%
      mutate(value = count) %>%
      left_join(beds) %>%
      group_by(Bed) %>%
      mutate(tows = n()) %>%
      ungroup() -> data

    filename = here::here(paste0("output/", YEAR, "/scal_abund_estimates.csv"))

  } else {

    scal_catch %>%
      filter(Size != "clapper") %>%
      mutate(value = wt_lb) %>%
      left_join(beds) %>%
      group_by(Bed) %>%
      mutate(tows = n()) %>%
      ungroup() -> data

    filename = here::here(paste0("output/", YEAR, "/scal_biom_estimates.csv"))
  }

  # calculate base estimate with lognormal CIs

  data %>%
    group_by(Bed, Size) %>%
    mutate(est = mean(value / (0.83 * area_swept)),
           cv = sqrt(var(value / (0.83 * area_swept)) / (tows - 1)) / est) %>%
    mutate(est = est * area_nm2) %>%
    summarise(est = mean(est), cv = mean(cv)) %>%
    mutate(l95 = est * exp(-1.96 * sqrt(log(1 + cv^2))),
           u95 = est * exp(1.96 * sqrt(log(1 + cv^2)))) %>%
    dplyr::select(-cv) %>%
    mutate_if(is.numeric, round, 0) -> est

  # bootstrap ci function

  boot_ci <- function(split){

    rsample::analysis(split) %>%
      mutate(est = mean(value / (0.83 * area_swept)) * area_nm2) %>%
      summarise(est = mean(est))
  }


  if(boot){

    data %>%
      nest(-Bed, -Size) %>%
      mutate(est = map(data, ~rsample::bootstraps(., 1000))) %>%
      unnest(est) %>%
      mutate(models = map(splits, ~est_it(.x))) %>%
      unnest(models) %>%
      group_by(Bed, Size) %>%
      summarise(l95 = quantile(est, 0.25),
                u95 = quantile(est, 0.975)) -> ci

    est %>%
      dplyr::select(-l95, -u95) %>%
      left_join(ci) %>%
      mutate_if(is.numeric, round, 0) -> est

  }

  write_csv(est, filename)
  est
}

# Estimation of bed large-scallop meat-weight with bootstrap CI option
f_meat_wt_est <- function(scal_awl, scal_catch, beds, Q, boot = TRUE){

  scal_catch %>%
    filter(Size == "large") %>%
    add_count(Bed) %>%
    transmute(Bed,
              tow_id,
              count,
              area_swept) -> x

  # Make data frame suitable for bootstrapping
  scal_awl %>%
    filter(size == "large", mwt_lb > 0, mwt_lb < rwt_lb) %>%
    select(tow_id, mwt_lb) %>%
    add_count(tow_id) %>%
    left_join(x, by = "tow_id") %>%
    replace_na(list(mwt_lb = 0, n = 0)) %>%
    group_by(Bed) %>%
    mutate(count = ifelse(n > count, n, count),
           bed_avg_mwt = mean(mwt_lb[mwt_lb > 0]),
           bed_var_mwt = var(mwt_lb[mwt_lb > 0]),
           n_i = n) %>%
    dplyr::select(-n) %>%
    group_by(Bed, tow_id) %>%
    summarize(count = unique(count),
              n_i = unique(n_i),
              sum_mwt = sum(mwt_lb),
              area_swept = mean(area_swept),
              d_hat = ifelse(n_i > 0,
                             count / n_i * sum_mwt / (Q * area_swept),
                             count * mean(bed_avg_mwt) / (Q * area_swept)),
              s2_mwt = ifelse(n_i > 1,
                              var(mwt_lb),
                              mean(bed_var_mwt)),
              s2_x = s2_mwt / (Q * area_swept)^2,
              s2_x_w = ifelse(n_i > 0, count * (count - n_i) / n_i, 0),
              s2_x_w = ifelse(n_i == 0 & count > 0, count^2, s2_x_w)) %>%
    left_join(beds, by = "Bed") %>%
    ungroup() -> data

  # Estimation function, given 'data' (one-time)

  data %>%
    group_by(Bed) %>%
    summarize(tows = unique(tows),
              est = mean(area_nm2) * mean(d_hat),
              se = mean(area_nm2) * sqrt(var(d_hat) / tows +
                                           mean(s2_x_w * s2_x)),
              cv = se / est,
              l95 = est * exp(-1.96 * sqrt(log(1 + cv^2))),
              u95 = est * exp(1.96 * sqrt(log(1 + cv^2)))) %>%
    mutate_at(vars(est, se, l95, u95), round, 0) -> est

  boot_ci <- function(split){

    rsample::analysis(split) %>%
      summarize(tows = unique(tows),
                est = mean(area_nm2) * mean(d_hat))
  }


  # To boot or not to boot

  if(boot){

    data %>%
      nest(-Bed, -tow_id) %>%
      mutate(boot = map(data, ~rsample::bootstraps(., 1000))) %>%
      unnest(boot) %>%
      mutate(models = map(splits, ~boot_ci(.x))) %>%
      unnest(models) %>%
      group_by(Bed) %>%
      summarise(l95 = quantile(est, 0.25),
                u95 = quantile(est, 0.975)) %>%
      mutate_at(vars(l95, u95), round, 0) -> ci


    est %>%
      dplyr::select(-l95, -u95) %>%
      left_join(ci) -> est
    est

  } else {

    est
  }

  write_csv(est, here::here(paste0("output/", YEAR, "/mwt_biom_est.csv")))
  est
}





# percent sex
f_sex_tbl <- function(scal_awl, tows){

  scal_awl %>%
    left_join(tows, by = "tow_id") %>%
    filter(!is.na(sex), size == "large") %>%
    group_by(Bed, sex) %>%
    summarise(n = n()) %>%
    mutate(freq = round(n / sum(n) * 100, digits = 1)) %>%
    group_by(Bed) %>%
    mutate(N = sum(n)) %>%
    spread(., sex, freq) %>%
    dplyr::select(-N, everything(), -n) %>%
    replace(is.na(.), 0) %T>%
    write_csv(here::here(paste0("output/", YEAR, "/sex_tbl.csv")))

}

# clappers
f_clap_tbl <- function(scal_catch, scal_awl){

  scal_awl %>%
    left_join(tows) %>%
    filter(size!="all", size!='small') %>%
    group_by(Bed) %>%
    summarise(N_weak = n(),
              weak = round(sum(weak, na.rm = T) / n() * 100, digits = 1)) -> x


  scal_catch %>%
    group_by(Bed) %>%
    filter(Size!='all') %>%
    summarise(clap = sum(count[Size =="clapper"]),
              N = sum(count),
              prop = round(clap / N * 100, digits = 1)) %>%
    left_join(x) %T>%
    write_csv(here::here(paste0("output/", YEAR, "/clapper_weak_tbl.csv")))
}

# worms
f_worm_tbl <- function(scal_awl, tows){

  tbl_names <- c("Bed", "N", "0%", "1-24%", "25-49%", "50-74%", "75-100%")

  scal_awl %>%
    left_join(tows) %>%
    filter(!is.na(worm)) %>%
    count(Bed, worm) %>%
    group_by(Bed) %>%
    mutate(prop = round(n / sum(n) * 100, 1)) %>%
    spread(worm, prop) %>%
    replace(is.na(.), 0) %>%
    mutate(`4` = 0) %>%
    summarise(N = sum(n),
              zero = round(max(`0`),1),
              one = round(max(`1`),1),
              two = round(max(`2`),1),
              three = round(max(`3`),1),
              four = round(max(`4`),1)) %>%
    rename_at(names(.), function(x) tbl_names) %T>%
    write_csv(here::here(paste0("output/", YEAR, "/worm.csv")))
}


# mud blisters
f_blister_tbl <- function(scal_awl, tows){

  tbl_names <- c("Bed", "N", "0%", "1-24%", "25-49%", "50-74%", "75-100%")

  scal_awl %>%
    left_join(tows) %>%
    filter(!is.na(mud)) %>%
    count(Bed, mud) %>%
    group_by(Bed) %>%
    mutate(prop = round(n / sum(n) * 100, 1)) %>%
    spread(mud, prop) %>%
    replace(is.na(.), 0) %>%
    mutate(`4` = 0) %>%
    summarise(N = sum(n),
              zero = round(max(`0`),1),
              one = round(max(`1`),1),
              two = round(max(`2`),1),
              three = round(max(`3`),1),
              four = round(max(`4`),1)) %>%
    rename_at(names(.), function(x) tbl_names) %T>%
    write_csv(here::here(paste0("output/", YEAR, "/mud.csv")))
}

# gonads


# gonads
f_gonad_tbl <- function(scal_awl, tows){

  tbl_names <- c("Bed","N", "Immature", "Empty", "Init. Recovery", "Filling", "Full", "Unknown")

  scal_awl %>%
    left_join(tows) %>%
    filter(!is.na(gonad)) %>%
    count(Bed, gonad) %>%
    group_by(Bed) %>%
    mutate(prop = round(n / sum(n) * 100, 1)) %>%
    spread(gonad, prop) %>%
    replace(is.na(.), 0) %>%
    summarise(N = sum(n),
              zero = round(max(`0`),1),
              one = round(max(`1`),1),
              two = round(max(`2`),1),
              three = round(max(`3`),1),
              four = round(max(`4`),1),
              five = round(max(`5`),1)) %>%
    rename_at(names(.), function(x) tbl_names) %T>%
    write_csv(here::here(paste0("output/", YEAR, "/gonad.csv")))

}



