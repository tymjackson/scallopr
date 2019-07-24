# 1. tows correspond to bed definitions
clean_tow <- function(events_data){

  output_dir <- file.path("output", YEAR)

  if (!dir.exists(output_dir)){
    dir.create(output_dir)
  } else {
    print("Good to go!")
  }

  if(YEAR < 2019){
    events_data %>%
      dplyr::filter(# YEAR==YEAR,
        GEAR_PERFORMANCE_CODE_SW == 1, # did the gear work
        STATION_TYPE %in%
          c("Standard", "Repeat", "Standard Non-Selected")) %>%
      # multiple station types
      dplyr::transmute(tow_id = EVENT_ID,
                       Bed = factor(BED_SW),
                       area_swept = TOW_LENGTH_DESIGNATED * 0.00131663,
                       # in nautical miles
                       #area_swept = TOW_LENGTH_FIELD * 0.00131663,
                       # in nautical miles - switched for 2018 to make run before getting complete dataset from Mumm
                       station = STATION_ID) %>%
      dplyr::mutate(bedsta = paste0(tow_id, station)) -> x
  } else {
    events_data %>%
      dplyr::filter(gear_perf == 1,
                    haul_type == 10) %>%
      dplyr::transmute(tow_id = tow,
                       Bed = factor(bed_code),
                       area_swept = distance * 0.00131663, # in nautical miles
                       station = station) %>%
      dplyr::mutate(bedsta = paste0(tow_id, station)) -> x
  }

  # make sure that there are no duplicate data

  if(dim(x)[1] != length(unique(x$bedsta))) {
    stop("Repeated station tows")
  } else {
    select(x, -station, - bedsta) -> x
  }

  # save the output
  write_csv(x, here::here(paste0("output/",YEAR,"/tows.csv")))
  x
}
