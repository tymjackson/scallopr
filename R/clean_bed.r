# 2. beds (dependent upon the tow function)
clean_bed <- function(area_data, tows){

  area_data %>%
    dplyr::right_join(dplyr::count(tows, Bed)) %>%
    dplyr::rename(tows = n) %>%
    dplyr::mutate(Bed = factor(Bed)) -> x

  write_csv(x, here::here(paste0("output/",YEAR,"/beds.csv")))
  x

}
