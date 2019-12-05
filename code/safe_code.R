# notes ----
# SAFE report observer data summary
# Scallop catch tables and figures
# author: Tyler Jackson
# contact: tyler.jackson@alaska.gov

# load ----
source("./code/safe_fishery_functions.R")

YEAR <- 2020

# data load ----

## scallop haul data ~1993/94 - 2008/09
## FOR NOW THIS IS JUST A SUMMARY TABLE, BUT SHOULD BE REPLACED TO HAUL DATA
old_catch <- read_csv("./data/old_catch/catch_summary_1992-2008.csv")

## scallop haul data 2009/10 - Present
catch <- do.call(bind_rows,
                 lapply(paste0("data/catch/", list.files("data/catch/")), read_csv))
## shell height data
shell_height <- do.call(bind_rows,
                 lapply(paste0("data/shell_height/", list.files("data/shell_height/")), read_csv))

## GHLs by area, district, season
ghl <- read_csv("./data/metadata/ghl.csv")


mgmt_unit <- read_csv("./data/metadata/management_units.csv")


# data mgmt ----

## rename fields in current data (2009 - present)
### scallop haul data
names(catch) <- c("Fishery", "District", "ADFG", "Trip_ID", "Haul_ID", 
                     "haul", "gear_perf", "haul_sampled", "Set_date", "set_lat",
                     "set_lon", "statarea", "depth", "dredge_count", 
                     "dredge_width", "dredge_down", "dredge_up", "duration", 
                     "dredge_hrs", "haul_speed", "distance_nm", "area_swept", 
                     "rtnd_basket", "round_weight", "meat_weight", "est_yield",
                     "tot_rtnd_basket", "tot_day_meat_weight")
### shell height data
names(shell_height)[c(6, 7)] <- c("shell_height", "shell_num")

## create a vector of districts fished in the most recent season
## change number to current year
catch %>%
  filter(grepl("18", Fishery)) %>%
  pull(District) %>%
  unique() -> fished_districts


# tables and figures ----

## fishery catch statistics
fished_districts %>%
  purrr::map(~f_fishery_stats(catch, old_data_table = T, district = .))

## raw and standardized cpue
### subset fished districts so that GAMs may run
tibble(fished_districts) %>%
  filter(!(fished_districts %in% c("C", "KSE", "WKI", "D"))) %>%
  pull(fished_districts) %>%
  purrr::map(~f_fishery_cpue(catch, district = ., weight_type = "round"))

## shell height composition
fished_districts %>%
  purrr::map(~f_sh_comp(shell_height, district = .))

# clean up: re-do figures that do not suit the generalized function ----


