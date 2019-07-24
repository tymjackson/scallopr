# 2. beds (dependent upon the tow function)
clean_bed <- function(area_data, tows){

  area_data %>%
    right_join(count(tows, Bed)) %>%
    rename(tows = n) %>%
    mutate(Bed = factor(Bed)) %T>%
    write_csv(here::here(paste0("output/", YEAR, "/beds.csv")))

}
