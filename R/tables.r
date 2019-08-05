tables <- function(scal_awl, tows){
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

  tbl_gonad(scal_awl, tows)
  tbl_worm(scal_awl, tows)
  tbl_sex(scal_awl, tows)

}






