# tables ----
# Table of general results with total scallop counts and overall catch rates,
# with CVs
tbl_catch <- function(scal_catch, beds){

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
                  CV) -> x

  write_csv(x, here::here(paste0("output/", YEAR, "/bed_catch_results_tbl.csv")))
  x

}
