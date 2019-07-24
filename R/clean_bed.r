# 2. beds (dependent upon the tow function)
clean_bed <- function(tows){

  area %>%
    full_join(count(tows, Bed)) %>%
    drop_na %>%
    rename(tows = n) %>%
    mutate(Bed = factor(Bed)) -> x

  write_csv(x, here::here(paste0("output/",YEAR,"/beds.csv")))
  x

}
