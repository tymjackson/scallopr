#options ----
library(tidyverse)
library(lubridate)
library(scales)
library(magrittr)
library(here)
library(knitr)
library(kableExtra)
library(broom)
library(rsample)
library(FNGr) # devtools::install_github("ben-williams/FNGr")

theme_set(theme_sleek())
options(scipen = 999)
theme_set(theme_sleek())

options(scipen=9999) # remove scientific notation

# globals ----

Bed_levels <- c("KSH1", "KSH2", "KSH3", "KNE2", "KNE3", "KNE6", "KAMN", "KAMS",
                "WK1", "EK1", "YAKB", "YAK1", "YAK2", "YAK3", "YAK4", "YAK5")
District_levels <- c("KSH", "KNE", 'KAM', "EKI", "WKI", "YAK", "D16")
Size_levels <- c("clapper", "small", "large", "all")
Districts <- tibble(Bed = factor(Bed_levels, levels = Bed_levels),
                    District = factor(case_when(substring(Bed, 1, 3) == "KSH" ~ "KSH",
                                                substring(Bed, 1, 3) == "KNE" ~ "KNE",
                                                substring(Bed, 1, 3) == "YAK" ~ "YAK",
                                                Bed == "KAMN" ~ "KAM",
                                                Bed == "KAMS" ~ "KAM",
                                                Bed == "EK1" ~ "EKI",
                                                Bed == "WK1" ~ "WKI"),
                                      levels = District_levels))

area <- data.frame(Bed = c('EK1','KSH1','KSH2','KSH3','WK1','YAKB','YAK1','YAK2',
                           'YAK3','YAK4','YAK5','YAK6','D16','KNE1','KNE2','KNE3',
                           'KNE4','KNE5','KNE6','KAMN','KAMS'),
                   area_nm2= c(89.11,	145.92,	21.15,	8.49,	48.66, 33.36,	52.31,
                               78.58,	167.46,	127.51,	54.86,	70.2,	18.77,
                               35.18,	121.24,	116.73,	16.42,	22.03,	27.48,
                               90.21,	68.03),
                   grids = c(88, 139,	20,	8, 48, 33,	52,	78,	165,	124,	53,
                             67, 18, 33, 112,	107, 15, 20, 25, 88, 66))
