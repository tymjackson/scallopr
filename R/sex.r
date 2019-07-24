# percent sex
tbl_sex <- function(scal_awl, tows){

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
