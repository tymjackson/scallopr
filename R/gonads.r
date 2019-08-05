# gonads
tbl_gonad <- function(scal_awl, tows){

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
    rename_at(names(.), function(x) tbl_names) -> x

  write_csv(x, here::here(paste0("output/", YEAR, "/gonad.csv")))
  x
}
