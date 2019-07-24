# clappers
tbl_clap <- function(scal_catch, scal_awl){

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
