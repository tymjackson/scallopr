# functions

# data cleaning ----
# must be done in order

# 1. tows correspond to bed definitions
f_tow_df <- function(events_data){

  # did the gear work
  # multiple station types

  events_data %>%
    filter(GEAR_PERFORMANCE_CODE_SW == 1,
           STATION_TYPE %in% c("Standard", "Repeat", "Standard Non-Selected")) %>%
    transmute(tow_id = EVENT_ID,
              Bed = factor(BED_SW),
              area_swept = TOW_LENGTH_DESIGNATED * 0.00131663, # nautical miles
              station = STATION_ID) %>%
    mutate(bedsta = paste0(tow_id, station)) -> x

  # make sure that there are no duplicate data

  if(dim(x)[1] != length(unique(x$bedsta))) {
    stop("Repeated station tows")
  } else {
    select(x, -station, -bedsta) -> x
  }
  # save the output
  write_csv(x, here::here(paste0("output/", YEAR, "/tows.csv")))
  x
}

# the 2019 survey data is in a different format
f_tow_df2 <- function(events_data){

  # did the gear work
  # multiple station types

  events_data %>%
    filter(gear_perf == 1,
           haul_type == 10) %>%
    transmute(tow_id = tow,
              Bed = factor(bed_code),
              area_swept = distance * 0.00131663, # in nautical miles
              station = station) %>%
    mutate(bedsta = paste0(tow_id, station)) -> x

  # make sure that there are no duplicate data

  if(dim(x) [1] != length(unique(x$bedsta))){
    stop("Repeated station tows")
  } else {
    select(x, -station, -bedsta) -> x
  }
  # save the output
  write_csv(x, here::here(paste0("output/",YEAR,"/tows.csv")))
  x
}

# 2. beds (dependent upon first function)
f_bed_df <- function(area_data, tows){

  area_data %>%
    right_join(count(tows, Bed)) %>%
    rename(tows = n) %>%
    mutate(Bed = factor(Bed)) %T>%
    write_csv(here::here(paste0("output/", YEAR, "/beds.csv")))
}

# 3 scal.catch (need tows first)
f_catch_df <- function(catch_data, tows, YEAR){

  catch_data %>%
    filter(YEAR == YEAR,
           RACE_CODE == 74120,
           CONDITION_CODE_RII %in% c("01", "1", "52"),
           EVENT_ID %in% tows$tow_id) %>%  # filter out uneccessary data
    transmute(tow_id = EVENT_ID,
              size = case_when(SCAL_SIZE_CLASS == 2 ~ "small",
                               SCAL_SIZE_CLASS == 1 ~ "large",
                               SCAL_SIZE_CLASS == 3 ~ "clapper"),
              count = COUNT,
              wt_lb = SAMPLE_WT_KG * 2.20462) %>%
    group_by(tow_id, size) %>%
    summarize(count = sum(count), wt_lb = sum(wt_lb)) %>%
    right_join(expand.grid(size = c("clapper", "small", "large", "all"),
                           tow_id = tows$tow_id)) %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    within(count[size == "all"] <- count[size == "small"] +
             count[size == "large"]) %>% # add "all" to counts
    within(wt_lb[size == "all"] <- wt_lb[size == "small"] +
             wt_lb[size == "large"]) %>% # add "all" to wts
    mutate(Size = factor(size, levels = Size_levels)) %>%
    select(-size) %>%
    left_join(tows, by = "tow_id") %>%
    ungroup() %>%
    write_csv(here::here(paste0("output/", YEAR, "/scal_catch.csv")))
}

f_catch_df2 <- function(catch_data, tows, YEAR){

  catch_data %>%
    filter(rcode == 74120,
           samp_grp %in% c(1, 2),
           tow %in% tows$tow_id) %>%  # filter out uneccessary data
    transmute(tow_id = tow,
              size = case_when(samp_grp == 1 ~ "large",
                               samp_grp == 2 ~ "small"),
              count = samp_cnt,
              wt_lb = samp_wt * 2.20462) %>%
    group_by(tow_id, size) %>%
    summarize(count = sum(count), wt_lb = sum(wt_lb)) %>%
    right_join(expand.grid(size = c("small", "large"),
                           tow_id = tows$tow_id)) %>%
    mutate_if(is.numeric, funs(replace(., is.na(.), 0))) %>%
    # within(count[size == "all"] <- count[size == "small"] + count[size == "large"]) %>% # add "all" to counts
    # within(wt_lb[size == "all"] <- wt_lb[size == "small"] + wt_lb[size == "large"]) %>% # add "all" to wts
    mutate(Size = factor(size, levels = Size_levels)) %>%
    select(-size) %>%
    left_join(tows, by = "tow_id") %>%
    ungroup() %>%
    write_csv(here::here(paste0("output/", YEAR, "/scal_catch.csv")))

}

# 4 scal.awl (need tows first)
f_awl_df <- function(awl_data, tows){

  awl_data %>%
    filter(RACE_CODE == 74120,
           EVENT_ID %in% tows$tow_id,
           SCAL_SIZE_CLASS == 1 | SCAL_SIZE_CLASS == 2) %>% # no clappers
    transmute(tow_id = EVENT_ID,
              size = recode(SCAL_SIZE_CLASS, "2" = "small", "1" = "large", .missing = "clapper"),
              sh = SHELL_HEIGHT_MM,
              rwt_lb = WHOLE_WT_GRAMS * 0.00220462,
              mwt_lb = MEAT_WEIGHT_GRAMS * 0.00220462,
              sex = SEX_SW,
              weak = MEAT_CONDITION_SW,
              gonad = SCAL_GONAD_COND,
              worm = SHELL_WORM_SW,
              mud = MUD_BLISTER_SW)  %T>%
    write_csv(here::here(paste0("output/", YEAR, "/awl_tbl.csv")))

}

f_awl_df2 <- function(mtwt_data, tows){

  mtwt_data %>%
    filter(rcode == 74120,
           tow %in% tows$tow_id,
           samp_grp %in% c(1, 2)) %>%
    transmute(tow_id = tow,
              sh = size,
              size = recode(samp_grp, "2" = "small", "1" = "large"),
              rwt_lb = whole_wt * 0.00220462,
              mwt_lb = meat_weight * 0.00220462,
              sex = sex,
              weak = meat_condition,
              gonad = gonad,
              worm = shell_worm,
              mud = mud_blister)  %T>%
    write_csv(here::here(paste0("output/", YEAR, "/awl_tbl.csv")))

}
