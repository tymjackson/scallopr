# worms
tbl_worm <- function(scal_awl, tows){

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
