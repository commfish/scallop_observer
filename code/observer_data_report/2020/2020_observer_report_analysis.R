# notes ----

## 2020 observer data summary analysis
## organized by management area
## Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2020/4/17

# load libraries and set global options ----

## packages
library(tidyverse)
library(scales)
library(magrittr)
library(FNGr)

## sourced scripts
### general observer data functions
source("./code/misc/general_observer_data_functions.R")
### functions for mapping
source("./code/maps/adfg_map_functions.R")

## global options
### custom color/fill pallete
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

### set theme (from FNGr)
theme_set(theme_sleek() + theme(legend.position = "bottom"))
### cutsom axis ticks for yrs (from FNGr)

# data ----

## metadata
### ghl
ghl <- read_csv("./data/metadata/ghl_revised_timeseries_2020.csv")

## observer/logbook data
### scallop haul data 2009/10 - Present
catch <- do.call(bind_rows,
                 lapply(paste0("data/catch/", list.files("data/catch/")), read_csv))
### shell heights 2009/10 - Present
shell_height <- do.call(bind_rows,
                   lapply(paste0("data/shell_height/", 
                                 list.files("data/shell_height/")), read_csv))
### bycatch by day 2009/10 - Present
bycatch <- do.call(bind_rows,
                   lapply(paste0("data/bycatch/", list.files("data/bycatch/")), read_csv))
### crab bycath size data 2009/10 - Present
crab_size <- do.call(bind_rows,
                     lapply(paste0("data/crab_size/", list.files("data/crab_size/")), read_csv))



# data mgmt ----

## remove phantom rows in catch tibble (can't figure out where they come from...)
catch %>%
  drop_na(Haul_ID) -> catch

## rename fields in current data (2009 - present)
f_catch_rename(catch) %>%
## add Season to data
f_add_season() %>%
## classify Karluk bed as KSW district instead of KSH
f_revise_district() %>% 
## coerce date to date class
mutate(Set_date = lubridate::mdy(Set_date)) %>%
## remove tows with zero dredge hours (logbook mistake)
filter(dredge_hrs != 0) -> catch

## rename fields in bycatch data (2009 - present)
f_bycatch_rename(bycatch) %>%
## add Season to data
f_add_season() %>%
## revise district (replace with f_revise district after beds are added to data)
mutate(District = ifelse(District %in% c("D", "YAK", "D16"), "YAK", District)) %>%
## coerce date to date class
mutate(Set_date = lubridate::mdy(Set_date)) -> bycatch

## reason fields in crab size data (2009 - present)
f_crab_size_rename(crab_size) %>%
## add Season to data
f_add_season() %>%
## revise district (replace with f_revise district after beds are added to data)
mutate(District = ifelse(District %in% c("D", "YAK", "D16"), "YAK", District)) %>%
## add Species and Sex
mutate(Species = case_when(RACE_code == 68560 ~ "Tanner crab",
                           RACE_code == 68541 ~ "snow crab"),
       Species = factor(Species, levels = c("Tanner crab", "snow crab")),
       Sex = case_when(sex == 1 ~ "Male",
                       sex == 2 ~ "Female",
                       sex == 3 ~ "Unknown"),
       Sex = factor(Sex, levels = c("Male", "Female", "Unknown"))) -> crab_size
  

## rename fields in shell_height data (2009 - present) 
f_shell_height_rename(shell_height) %>%
## add Season to data
f_add_season() %>%
## revise District as in catch data
mutate(District = catch$District[match(.$Haul_ID, catch$Haul_ID)]) -> shell_height
  
# fishery catch ----

## fishery performance tables by district (f_fish_stats from general functions)
### KNE
f_fish_stats(catch, c("KNE"), add_ghl = T, 
             path = "./output/observer_summary/2020/fish_stats_KNE.csv")
### KSH
f_fish_stats(catch, c("KSH"), add_ghl = T, 
             path = "./output/observer_summary/2020/fish_stats_KSH.csv")
### KSW
f_fish_stats(catch, c("KSW"), add_ghl = T, 
             path = "./output/observer_summary/2020/fish_stats_KSW.csv")
### KSE
f_fish_stats(catch, c("KSE"), add_ghl = T, 
             path = "./output/observer_summary/2020/fish_stats_KSE.csv")
### Area M
f_fish_stats(catch, c("UB", "WC", "C"), add_ghl = T, 
             path = "./output/observer_summary/2020/fish_stats_M.csv")
### Area O
f_fish_stats(catch, c("O"), add_ghl = T, 
             path = "./output/observer_summary/2020/fish_stats_O.csv")
### Area Q
f_fish_stats(catch, c("Q"), add_ghl = T, 
             path = "./output/observer_summary/2020/fish_stats_Q.csv")
### WKI
f_fish_stats(catch, c("WKI"), add_ghl = T, 
             path = "./output/observer_summary/2020/fish_stats_WKI.csv")
### EKI
f_fish_stats(catch, c("EKI"), add_ghl = T, 
             path = "./output/observer_summary/2020/fish_stats_EKI.csv")
### YAK
f_fish_stats(catch, c("YAK"), add_ghl = T, 
             path = "./output/observer_summary/2020/fish_stats_YAK.csv")


## standardized cpue plots (f_standard_cpue from general observer functions)
catch %>%
  # create fields month and vessel
  mutate(Month = lubridate::month(Set_date),
         Month = factor(Month, levels = c(7:12, 1:6)),
         Vessel = factor(ADFG),
         Bed = factor(ifelse(is.na(bed_code), "Unknown", bed_code))) -> tmp

### KNE
#### plot by season
f_standardize_cpue(filter(tmp, District == "KNE", Bed != "Unknown"), 
                   path = "./figures/observer_data_report/2020/std_cpue_effects_KNE.png",
                   by = "Season") %T>%
  write_csv("./output/observer_summary/2020/standardized_cpue_season_KNE.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_boxplot(data = filter(tmp, District == "KNE"), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey80", fill = NA, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")+
  # customize y limits to exclude outliers
  coord_cartesian(ylim = c(0, 2000)) -> x
#### get number of beds in District
n_levels <- length(unique(filter(tmp, District == "KNE")$Bed))
#### plot by bed, season
f_standardize_cpue(filter(tmp, District == "KNE", Bed != "Unknown"), 
                   path = "./figures/observer_data_report/2020/std_cpue_effects_KNE.png",
                   by = "Bed") %T>%
  write_csv("./output/observer_summary/2020/standardized_cpue_bedseason_KNE.csv") %>%
  ggplot(aes(x = Season, y = std_cpue, color = Bed, group = Bed))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = cb_palette[1:n_levels])+
  labs(x = NULL, y = "Standardized \n CPUE (lbs / dredge hr)")+
  theme(legend.position = "bottom") -> y
#### combine plots
cowplot::plot_grid(x, y + theme(legend.position = "none"), cowplot::get_legend(y),
                   ncol = 1, rel_heights = c(1, 1, 0.2)) -> z
ggsave("./figures/observer_data_report/2020/standardized_cpue_KNE.png", 
       plot = z, height = 5, width = 7, units = "in")

### KSH
#### plot by season
f_standardize_cpue(filter(tmp, District == "KSH", dredge_hrs != 0, Bed != "Unknown"), 
                   path = "./figures/observer_data_report/2020/std_cpue_effects_KSH.png",
                   by = "Season") %T>%
  write_csv("./output/observer_summary/2020/standardized_cpue_season_KSH.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_boxplot(data = filter(tmp, District == "KSH"), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey80", fill = NA, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")+
  # customize y limits to exclude outliers
  coord_cartesian(ylim = c(0, 1500)) -> x
#### get number of beds in District
n_levels <- length(unique(filter(tmp, District == "KSH")$Bed))
#### plot by bed, season
f_standardize_cpue(filter(tmp, District == "KSH", dredge_hrs != 0, Bed != "Unknown"), 
                   path = "./figures/observer_data_report/2020/std_cpue_effects_KSH.png",
                   by = "Bed") %T>%
  write_csv("./output/observer_summary/2020/standardized_cpue_bedseason_KSH.csv") %>%
  ggplot(aes(x = Season, y = std_cpue, color = Bed, group = Bed))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = cb_palette[1:n_levels])+
  labs(x = NULL, y = "Standardized \n CPUE (lbs / dredge hr)")+
  theme(legend.position = "bottom") -> y
#### combine plots
cowplot::plot_grid(x, y + theme(legend.position = "none"), cowplot::get_legend(y),
                   ncol = 1, rel_heights = c(1, 1, 0.1)) -> z
ggsave("./figures/observer_data_report/2020/standardized_cpue_KSH.png", 
       plot = z, height = 5, width = 7, units = "in")

### KSW (incuding bed)
tmp %>%
  filter(District == "KSW") %>%
  # move unknown beds to KSW1
  mutate(Bed = factor(ifelse(set_lat > 57.1, "KSH 4-6", "KSW1"))) -> tmp_ksw
#### plot by season
f_standardize_cpue(tmp_ksw, 
                   path = "./figures/observer_data_report/2020/std_cpue_effects_KSW.png",
                   by = "Season") %T>%
  write_csv("./output/observer_summary/2020/standardized_cpue_season_KSW.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_boxplot(data = filter(tmp, District == "KSW"), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey80", fill = NA, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")+
  # customize y limits to exclude outliers
  coord_cartesian(ylim = c(0, 2000)) -> x
#### get number of beds in District
n_levels <- length(unique(filter(tmp, District == "KSW")$Bed))
#### plot by bed, season
f_standardize_cpue(tmp_ksw, 
                   path = "./figures/observer_data_report/2020/std_cpue_effects_KSW.png",
                   by = "Bed") %T>%
  write_csv("./output/observer_summary/2020/standardized_cpue_bedseason_KSW.csv") %>%
  ggplot(aes(x = Season, y = std_cpue, color = Bed, group = Bed))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = cb_palette[1:n_levels])+
  labs(x = NULL, y = "Standardized \n CPUE (lbs / dredge hr)")+
  theme(legend.position = "bottom") -> y
#### combine plots
cowplot::plot_grid(x, y + theme(legend.position = "none"), cowplot::get_legend(y),
                   ncol = 1, rel_heights = c(1, 1, 0.1)) -> z
ggsave("./figures/observer_data_report/2020/standardized_cpue_KSW.png", 
       plot = z, height = 5, width = 7, units = "in")

### M
#### plot by season
f_standardize_cpue2(filter(tmp, District %in% c("WC", "C", "UB")), 
                   path = "./figures/observer_data_report/2020/std_cpue_effects_M.png") %T>%
  write_csv("./output/observer_summary/2020/standardized_cpue_season_M.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_boxplot(data = filter(tmp, District %in% c("WC", "C", "UB")), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey80", fill = NA, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")+
  # customize y limits to exclude outliers
  coord_cartesian(ylim = c(0, 2000)) -> x
ggsave("./figures/observer_data_report/2020/standardized_cpue_M.png", 
       plot = x, height = 3, width = 6, units = "in")

### O
#### plot by season
f_standardize_cpue(filter(tmp, District == "O", Bed != "Unknown"), 
                   path = "./figures/observer_data_report/2020/std_cpue_effects_O.png",
                   by = "Season") %T>%
  write_csv("./output/observer_summary/2020/standardized_cpue_season_O.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_boxplot(data = filter(tmp, District == "O"), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey80", fill = NA, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")+
  # customize y limits to exclude outliers
  coord_cartesian(ylim = c(0, 1500)) -> x
#### get number of beds in District
n_levels <- length(unique(filter(tmp, District == "O")$Bed))
#### plot by bed, season
f_standardize_cpue(filter(tmp, District == "O", Bed != "Unknown"), 
                   path = "./figures/observer_data_report/2020/std_cpue_effects_O.png",
                   by = "Bed") %T>%
  write_csv("./output/observer_summary/2020/standardized_cpue_bedseason_O.csv") %>%
  ggplot(aes(x = Season, y = std_cpue, color = Bed, group = Bed))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = cb_palette[1:n_levels])+
  labs(x = NULL, y = "Standardized \n CPUE (lbs / dredge hr)")+
  theme(legend.position = "bottom") -> y
#### combine plots
cowplot::plot_grid(x, y + theme(legend.position = "none"), cowplot::get_legend(y),
                   ncol = 1, rel_heights = c(1, 1, 0.1)) -> z
ggsave("./figures/observer_data_report/2020/standardized_cpue_O.png", 
       plot = z, height = 5, width = 7, units = "in")

### area Q
#### plot by season
f_standardize_cpue2(filter(tmp, District %in% c("Q")), 
                    path = "./figures/observer_data_report/2020/std_cpue_effects_Q.png") %T>%
  write_csv("./output/observer_summary/2020/standardized_cpue_season_Q.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_boxplot(data = filter(tmp, District %in% c("Q")), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey80", fill = NA, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")+
  # customize y limits to exclude outliers
  coord_cartesian(ylim = c(0, 1300)) -> x
ggsave("./figures/observer_data_report/2020/standardized_cpue_Q.png", 
       plot = x, height = 3, width = 7, units = "in")

### YAK
#### combine beds YAK6Y and YAK6D
tmp %>%
  filter(District == "YAK") %>%
  mutate(Bed = factor(ifelse(Bed %in% c("YAK6Y", "YAK6D"), "YAK6", as.character(Bed))),
         Bed = factor(ifelse(Bed == "EK1", "YAKB", as.character(Bed)))) -> tmp_yak
#### plot by season
f_standardize_cpue(filter(tmp_yak, Bed != "Unknown"), 
                   path = "./figures/observer_data_report/2020/std_cpue_effects_YAK.png",
                   by = "Season") %T>%
  write_csv("./output/observer_summary/2020/standardized_cpue_season_YAK.csv") -> x # x is a temporary object to be overwritten
ggplot()+
  geom_boxplot(data = filter(tmp, District == "YAK"), 
               aes(x = Season, y = round_weight / dredge_hrs), 
               color = "grey80", fill = NA, outlier.shape = NA)+
  geom_point(data = x, aes(x = Season, y = std_cpue))+
  geom_line(data = x, aes(x = Season, y = std_cpue, group = 1))+
  labs(x = NULL, y = "CPUE (lbs / dredge hr)")+
  # customize y limits to exclude outliers
  coord_cartesian(ylim = c(0, 2000)) -> x
#### get number of beds in District
n_levels <- length(unique(filter(tmp, District == "YAK")$Bed))
#### plot by bed, season
f_standardize_cpue(filter(tmp_yak, Bed != "Unknown"), 
                   path = "./figures/observer_data_report/2020/std_cpue_effects_YAK.png",
                   by = "Bed") %T>%
  write_csv("./output/observer_summary/2020/standardized_cpue_bedseason_YAK.csv") %>%
  ggplot(aes(x = Season, y = std_cpue, color = Bed, group = Bed))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = cb_palette[1:n_levels])+
  labs(x = NULL, y = "Standardized \n CPUE (lbs / dredge hr)")+
  theme(legend.position = "bottom") -> y
#### combine plots
cowplot::plot_grid(x, y + theme(legend.position = "none"), cowplot::get_legend(y),
                   ncol = 1, rel_heights = c(1, 1, 0.2)) -> z
ggsave("./figures/observer_data_report/2020/standardized_cpue_YAK.png", 
       plot = z, height = 5, width = 7, units = "in")

ggplot() +
  geom_point(data = tmp %>%
               filter(District == "YAK"), aes(set_lon, set_lat, color = Bed))

# fishery extent ----
## map of dredge locations by district (f_base_map from general functions)
### change NA to unknown
catch %>%
  mutate(bed_code = ifelse(is.na(bed_code), "Unknown", bed_code)) -> tmp
  
### KNE
f_base_map +
  geom_point(data = filter(tmp, District == "KNE"), aes(x = set_lon, y = set_lat, color = bed_code), 
             alpha = 0.5) +
  labs(color = NULL) +
  scale_color_manual(values = c(cb_palette[2:7], "black"))+
  KNE_proj -> x
ggsave("./figures/observer_data_report/2020/all_yr_dredge_map_KNE.png", plot = x,
       height = 4, width = 4, units = "in")
### KSH
f_base_map +
  geom_point(data = filter(tmp, District == "KSH"), aes(x = set_lon, y = set_lat, color = bed_code), 
             alpha = 0.5) +
  labs(color = NULL) +
  scale_color_manual(values = c(cb_palette[2:4], "black"))+
  KSH_proj -> x
ggsave("./figures/observer_data_report/2020/all_yr_dredge_map_KSH.png", plot = x,
       height = 4, width = 4, units = "in")
### KSW
f_base_map +
  geom_point(data = filter(tmp, District == "KSW"), aes(x = set_lon, y = set_lat, color = bed_code), 
             alpha = 0.5) +
  labs(color = NULL) +
  scale_color_manual(values = c(cb_palette[2:6], "black"))+
  geom_hline(yintercept = 57.1, linetype = 2)+
  KSW_proj -> x
ggsave("./figures/observer_data_report/2020/all_yr_dredge_map_KSW.png", plot = x,
       height = 4, width = 4, units = "in")
### area M
f_base_map +
  geom_point(data = filter(tmp, District %in% c("WC", "C", "UB")), 
             aes(x = set_lon, y = set_lat, color = bed_code), alpha = 0.5) +
  labs(color = NULL) +
  scale_color_manual(values = c(cb_palette[2:7], "black"))+
  areaM_proj -> x
ggsave("./figures/observer_data_report/2020/all_yr_dredge_map_M.png", plot = x,
       height = 4, width = 4, units = "in")
### area O
f_base_map +
  geom_point(data = filter(tmp, District == "O"), aes(x = set_lon, y = set_lat, color = bed_code), 
             alpha = 0.5) +
  labs(color = NULL) +
  scale_color_manual(values = c(cb_palette[2:5], "black"))+
  areaO_proj -> x
ggsave("./figures/observer_data_report/2020/all_yr_dredge_map_O.png", plot = x,
       height = 4, width = 4, units = "in")
### area Q
f_base_map +
  geom_point(data = filter(tmp, District == "Q"), aes(x = set_lon, y = set_lat, color = bed_code), 
             alpha = 0.5) +
  labs(color = NULL) +
  scale_color_manual(values = c(cb_palette[2], "black"))+
  areaQ_proj -> x
ggsave("./figures/observer_data_report/2020/all_yr_dredge_map_Q.png", plot = x,
       height = 4, width = 4, units = "in")
### YAK
tmp %>%
  filter(District == "YAK") %>%
  mutate(bed_code = factor(bed_code,
                           levels = c("EK1", "YAKB", "YAK1", "YAK2", "YAK3", "YAK4", "YAK5", 
                                      "YAK6Y", "YAK6D", "Unknown"))) -> tmp_yak
f_base_map +
  geom_point(data = tmp_yak, aes(x = set_lon, y = set_lat, color = bed_code), 
             alpha = 0.5) +
  labs(color = NULL) +
  scale_color_manual(values = c(cb_palette[2:8], cb_palette[2:3], "black"))+
  YAK_proj -> x
ggsave("./figures/observer_data_report/2020/all_yr_dredge_map_YAK.png", plot = x,
       height = 4, width = 5, units = "in")



## raster maps of fishering effort by district, year (f_make_grid from adfg_map_functions.R)
### KNE
# build raster grid
catch %>%
  filter(District == "KNE") %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = KNE_proj$limits$x,
                           lat = KNE_proj$limits$y, by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  dplyr::select(Season, grid) %>%
  unnest(grid) -> tmp
# plot map
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.1, 0.3, 0.5))+
  KNE_proj+
  scale_x_continuous(breaks = seq(-170, -130, 1))+
  scale_y_continuous(breaks = seq(50, 65, 1))+
  facet_wrap(~Season, ncol = 3)+ 
  theme(legend.position = "right") -> x
ggsave("./figures/observer_data_report/2020/effort_map_KNE.png", plot = x, 
       height = 8, width = 7, unit = "in")
### KSH
catch %>%
  filter(District == "KSH") %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = KSH_proj$limits$x,
                           lat = KSH_proj$limits$y, by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  select(Season, grid) %>%
  unnest(grid) -> tmp
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.01, 0.1, 0.2, 0.3))+
  KSH_proj+
  scale_x_continuous(breaks = seq(-170, -130, 1))+
  scale_y_continuous(breaks = seq(50, 65, 1))+
  facet_wrap(~Season, ncol = 3)+ 
  theme(legend.position = "right") -> x
ggsave("./figures/observer_data_report/2020/effort_map_KSH.png", plot = x, 
       height = 8, width = 7, unit = "in")
### KSW
catch %>%
  filter(District == "KSW") %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = KSW_proj$limits$x,
                           lat = KSW_proj$limits$y, by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  select(Season, grid) %>%
  unnest(grid) -> tmp
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.01, 0.25, 0.5))+
  KSW_proj+
  scale_x_continuous(breaks = seq(-170, -130, 1))+
  scale_y_continuous(breaks = seq(50, 65, 1))+
  facet_wrap(~Season, ncol = 4, drop = F)+ 
  theme(legend.position = "right") -> x
ggsave("./figures/observer_data_report/2020/effort_map_KSW.png", plot = x, 
       height = 8, width = 7, unit = "in")
### area M
catch %>%
  filter(District %in% c("WC", "C", "UB")) %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = areaM_proj$limits$x,
                           lat = areaM_proj$limits$y, by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  select(Season, grid) %>%
  unnest(grid) -> tmp
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.01, 0.25, 0.5, 0.75))+
  areaM_proj+
  facet_wrap(~Season, nrow = 3, drop = T)+ 
  theme(legend.position = "right") -> x
ggsave("./figures/observer_data_report/2020/effort_map_M.png", plot = x, 
       height = 6, width = 7, unit = "in")
### area O
catch %>%
  filter(District %in% c("O")) %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = areaO_proj$limits$x,
                           lat = areaO_proj$limits$y, by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  select(Season, grid) %>%
  unnest(grid) -> tmp
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.01, 0.25, 0.5, 0.75))+
  areaO_proj+
  scale_x_continuous(breaks = seq(-180, 0 , 2))+
  facet_wrap(~Season, nrow = 4, drop = T)+ 
  theme(legend.position = "right") -> x
ggsave("./figures/observer_data_report/2020/effort_map_O.png", plot = x, 
       height = 8, width = 7, unit = "in")
### area Q
catch %>%
  filter(District %in% c("Q")) %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = c(-166, -164),
                           lat = c(55, 56), by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  select(Season, grid) %>%
  unnest(grid) -> tmp
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.01, 0.10, 0.20, 0.30))+
  coord_quickmap(xlim = c(-167, -163), ylim = c(54.5, 56.5))+
  scale_x_continuous(breaks = seq(-180, 0 , 2))+
  facet_wrap(~Season, nrow = 4, drop = T)+ 
  theme(legend.position = "right") -> x
ggsave("./figures/observer_data_report/2020/effort_map_Q.png", plot = x, 
       height = 8, width = 7, unit = "in")
### YAK
catch %>%
  filter(District %in% c("YAK")) %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = YAK_proj$limits$x,
                           lat = YAK_proj$limits$y, by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  select(Season, grid) %>%
  unnest(grid) -> tmp
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.01, 0.05, 0.10, 0.15))+
  YAK_proj+
  scale_x_continuous(breaks = seq(-180, 0 , 2))+
  facet_wrap(~Season, nrow = 4, drop = T)+ 
  theme(legend.position = "right") -> x
ggsave("./figures/observer_data_report/2020/effort_map_YAK.png", plot = x, 
       height = 8, width = 7, unit = "in")



## extent of round weight catch (f_extent_catch from general_observer_data_functions.R)
### KNE
catch %>%
  # remove tows with missing lat/lon
  filter(District == "KNE",
         !is.na(set_lon), !is.na(set_lat)) %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/cpue_extent_KNE.png", plot = x, 
       height = 3, width = 7, unit = "in")  
### KSH
catch %>%
  filter(District == "KSH") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/cpue_extent_KSH.png", plot = x, 
       height = 3, width = 7, unit = "in")
### KSW
catch %>%
  filter(District == "KSW") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/cpue_extent_KSW.png", plot = x, 
       height = 3, width = 7, unit = "in")
### areaM
#### UB
catch %>%
  filter(District == "UB") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/cpue_extent_UB.png", plot = x, 
       height = 3, width = 7, unit = "in")
#### O
catch %>%
  filter(District == "O") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/cpue_extent_O.png", plot = x, 
       height = 3, width = 7, unit = "in")

#### Q
catch %>%
  filter(District == "Q") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/cpue_extent_Q.png", plot = x, 
       height = 3, width = 7, unit = "in")

#### YAK
catch %>%
  filter(District == "YAK") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight, na.rm = T) / sum(dredge_hrs, na.rm = T),
            Extent = mean(extent, na.rm = T) * 300) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./300, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/cpue_extent_YAK.png", plot = x, 
       height = 3, width = 7, unit = "in")

# bycatch ----

## total crab bycatch and bycatch:meatweight ratio by Season, District
### compute stats in large tibble to subset by district
bycatch %>%
  group_by(Season, District) %>%
  summarise(total_effort = sum(dredge_hrs),
            total_sample = sum(sample_hrs),
            tanner_rate = sum(bairdi_count) / total_sample,
            total_tanner = tanner_rate * total_effort,
            tanner_ratio = total_tanner / sum(mt_wt),
            snow_rate = sum(opilio_count) / total_sample,
            total_snow = snow_rate * total_effort,
            snow_ratio = total_snow / sum(mt_wt),
            dungeness_rate = sum(dungeness_count) / total_sample,
            total_dungeness = dungeness_rate * total_effort,
            dungeness_ratio = total_dungeness / sum(mt_wt),
            halibut_rate = sum(halibut_count) / total_sample,
            total_halibut = halibut_rate * total_effort,
            halibut_ratio = total_halibut  / sum(mt_wt),
            total_king = sum(king_count),
            king_ratio = total_king / sum(mt_wt)) %>%
  left_join(ghl %>%
              dplyr::select(Season, District, ghl, tanner_cbl, snow_cbl, king_cbl)) -> tmp
### extract KNE bycatch summary table
tmp %>%
  filter(District == "KNE") %>%
  select(Season, ghl, tanner_cbl, total_tanner, tanner_ratio, total_king, king_ratio,  total_halibut, 
         halibut_ratio) %T>%
  write_csv("./output/observer_summary/2020/bycatch_summary_KNE.csv") %>%
  # plot by annual totals in KNE by species
  pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
  mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                             species == "total_dungeness" ~ "Dungeness crab",
                             species == "total_tanner" ~ "Tanner crab",
                             species == "total_king" ~ "Red king crab"),
         Species = factor(species, c("Tanner crab", "Red king crab", "Dungeness crab",
                                     "Pacific halibut"))) %>%
  ggplot(aes(x = Season, y = total, color = Species, group = Species))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Total catch (count)", color = NULL)+
  scale_color_manual(values = cb_palette[c(1, 2, 4)])+
  facet_wrap(~Species, scales = "free_y", ncol = 1)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/bycatch_totals_KNE.png", plot = x,
       height = 6, width = 7, units = "in")   

### KSH bycatch summary table
tmp %>%
  filter(District == "KSH") %>%
  select(Season, ghl, tanner_cbl, total_tanner, tanner_ratio, total_king, king_ratio,  total_dungeness,
         dungeness_ratio, total_halibut, halibut_ratio) %T>%
  write_csv("./output/observer_summary/2020/bycatch_summary_KSH.csv") %>%
  # plot by annual totals in KSH by species
  pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
  mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                             species == "total_dungeness" ~ "Dungeness crab",
                             species == "total_tanner" ~ "Tanner crab",
                             species == "total_king" ~ "Red king crab"),
         Species = factor(species, c("Tanner crab", "Red king crab", "Dungeness crab",
                                     "Pacific halibut"))) %>%
  ggplot(aes(x = Season, y = total, color = Species, group = Species))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Total catch (count)", color = NULL)+
  scale_color_manual(values = cb_palette[1:4])+
  facet_wrap(~Species, scales = "free_y", ncol = 1)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/bycatch_totals_KSH.png", plot = x,
       height = 8, width = 7, units = "in") 

### KSW bycatch summary table
tmp %>%
  filter(District == "KSW") %>%
  select(Season, ghl, tanner_cbl, total_tanner, tanner_ratio, total_king, king_ratio,  total_dungeness,
         dungeness_ratio, total_halibut, halibut_ratio) %T>%
  write_csv("./output/observer_summary/2020/bycatch_summary_KSW.csv") %>%
  # plot by annual totals in KSW by species
  pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
  mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                             species == "total_dungeness" ~ "Dungeness crab",
                             species == "total_tanner" ~ "Tanner crab",
                             species == "total_king" ~ "Red king crab"),
         Species = factor(species, c("Tanner crab", "Red king crab", "Dungeness crab",
                                     "Pacific halibut"))) %>%
  ggplot(aes(x = Season, y = total, color = Species, group = Species))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Total catch (count)", color = NULL)+
  scale_color_manual(values = cb_palette[1:4])+
  facet_wrap(~Species, scales = "free_y", ncol = 1)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/bycatch_totals_KSW.png", plot = x,
       height = 8, width = 7, units = "in") 

### area M
### UB bycatch summary table
tmp %>%
  filter(District == "UB") %>%
  select(Season, ghl, tanner_cbl, snow_cbl, total_tanner, tanner_ratio, total_snow, snow_ratio,
         total_king, king_ratio,  total_dungeness, dungeness_ratio, total_halibut, halibut_ratio) %T>%
  write_csv("./output/observer_summary/2020/bycatch_summary_UB.csv") %>%
  # remove dungeness crab
  dplyr::select(-total_dungeness, -dungeness_ratio, -total_king, -king_ratio) %>%
  # plot by annual totals in Q by species
  pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
  mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                             species == "total_tanner" ~ "Tanner crab",
                             species == "total_snow" ~ "Snow crab"),
         Species = factor(species, c("Tanner crab", "Snow crab", "Pacific halibut"))) %>%
  ggplot(aes(x = Season, y = total, color = Species, group = Species))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Total catch (count)", color = NULL)+
  scale_color_manual(values = cb_palette[1:5])+
  facet_wrap(~Species, scales = "free_y", ncol = 1)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/bycatch_totals_UB.png", plot = x,
       height = 8, width = 7, units = "in")
### O bycatch summary table
tmp %>%
  filter(District == "O") %>%
  select(Season, ghl, tanner_cbl, total_tanner, tanner_ratio, total_king, king_ratio,  total_dungeness,
         dungeness_ratio, total_halibut, halibut_ratio) %T>%
  write_csv("./output/observer_summary/2020/bycatch_summary_O.csv") %>%
  # plot by annual totals in O by species
  pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
  mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                             species == "total_dungeness" ~ "Dungeness crab",
                             species == "total_tanner" ~ "Tanner crab",
                             species == "total_king" ~ "Red king crab"),
         Species = factor(species, c("Tanner crab", "Red king crab", "Dungeness crab",
                                     "Pacific halibut"))) %>%
  ggplot(aes(x = Season, y = total, color = Species, group = Species))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Total catch (count)", color = NULL)+
  scale_color_manual(values = cb_palette[1:4])+
  facet_wrap(~Species, scales = "free_y", ncol = 1)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/bycatch_totals_O.png", plot = x,
       height = 8, width = 7, units = "in")

### Q bycatch summary table
tmp %>%
  filter(District == "Q") %>%
  select(Season, ghl, tanner_cbl, snow_cbl, total_tanner, tanner_ratio, total_snow, snow_ratio,
         total_king, king_ratio,  total_dungeness, dungeness_ratio, total_halibut, halibut_ratio) %T>%
  write_csv("./output/observer_summary/2020/bycatch_summary_Q.csv") %>%
  # remove dungeness crab
  dplyr::select(-total_dungeness, -dungeness_ratio) %>%
  # plot by annual totals in Q by species
  pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
  mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                             species == "total_tanner" ~ "Tanner crab",
                             species == "total_snow" ~ "Snow crab",
                             species == "total_king" ~ "Red king crab"),
         Species = factor(species, c("Tanner crab", "Snow crab", "Red king crab", "Pacific halibut"))) %>%
  ggplot(aes(x = Season, y = total, color = Species, group = Species))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Total catch (count)", color = NULL)+
  scale_color_manual(values = cb_palette[1:5])+
  facet_wrap(~Species, scales = "free_y", ncol = 1)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/bycatch_totals_Q.png", plot = x,
       height = 8, width = 7, units = "in")

### YAK bycatch summary table
tmp %>%
  filter(District == "YAK") %>%
  select(Season, ghl, tanner_cbl, total_tanner, tanner_ratio, total_king, king_ratio,  total_dungeness,
         dungeness_ratio, total_halibut, halibut_ratio) %T>%
  write_csv("./output/observer_summary/2020/bycatch_summary_YAK.csv") %>%
  # plot by annual totals in YAK by species
  pivot_longer(c(grep("total", names(.))), names_to = "species", values_to = "total") %>%
  mutate(species = case_when(species == "total_halibut" ~ "Pacific halibut",
                             species == "total_dungeness" ~ "Dungeness crab",
                             species == "total_tanner" ~ "Tanner crab",
                             species == "total_king" ~ "Red king crab"),
         Species = factor(species, c("Tanner crab", "Red king crab", "Dungeness crab",
                                     "Pacific halibut"))) %>%
  ggplot(aes(x = Season, y = total, color = Species, group = Species))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Total catch (count)", color = NULL)+
  scale_color_manual(values = cb_palette[1:4])+
  facet_wrap(~Species, scales = "free_y", ncol = 1)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/bycatch_totals_YAK.png", plot = x,
       height = 8, width = 7, units = "in") 




## meat weight:tanner crab bycatch ratio
### area K
bycatch %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs) %>%
  summarise(tanner_catch = sum(tanner_catch),
            mt_wt = sum(mt_wt)) %>%
  filter(District %in% c("KNE", "KSH", "KSW", "KSE")) %>%
  ggplot(aes(x = Season, y = tanner_catch / mt_wt, 
             group = District, color = District))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Bycatch Ratio \n (Tanner crab : lbs scallop meat)", color = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/tanner_bycatch_ratio_area_K.png", plot = x,
       height = 3, width = 7, units = "in")          
### area M
bycatch %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs) %>%
  summarise(tanner_catch = sum(tanner_catch),
            mt_wt = sum(mt_wt)) %>%
  filter(District %in% c("WC", "C", "UB")) %>%
  ggplot(aes(x = Season, y = tanner_catch / mt_wt, 
             group = District, color = District))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Bycatch Ratio \n (Tanner crab : lbs scallop meat)", color = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/tanner_bycatch_ratio_area_M.png", plot = x,
       height = 3, width = 7, units = "in") 
### area O
bycatch %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs) %>%
  summarise(tanner_catch = sum(tanner_catch),
            mt_wt = sum(mt_wt)) %>%
  filter(District %in% c("O")) %>%
  ggplot(aes(x = Season, y = tanner_catch / mt_wt, 
             group = District))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  labs(x = NULL, y = "Bycatch Ratio \n (Tanner crab : lbs scallop meat)", color = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/tanner_bycatch_ratio_area_O.png", plot = x,
       height = 3, width = 7, units = "in") 
### area Q
bycatch %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count),
            opilio_count = sum(opilio_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs,
         snow_rate = ifelse(sample_hrs == 0,
                            sum(opilio_count) / sum(sample_hrs),
                            opilio_count / sample_hrs),
         snow_catch = snow_rate * dredge_hrs,) %>%
  summarise(`Tanner crab` = sum(tanner_catch),
            `Snow crab` = sum(snow_catch),
            mt_wt = sum(mt_wt)) %>%
  filter(District %in% c("Q")) %>%
  pivot_longer(c(3, 4), names_to = "species", values_to = "total_catch") %>%
  ggplot(aes(x = Season, y = total_catch / mt_wt, 
             group = species, color = species))+
  geom_point()+
  geom_line()+
  scale_color_manual(values = cb_palette[1:2])+
  labs(x = NULL, y = "Bycatch Ratio \n (crab : lbs scallop meat)", color = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/tanner_bycatch_ratio_area_Q.png", plot = x,
       height = 3, width = 7, units = "in") 
### YAK
bycatch %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs) %>%
  summarise(tanner_catch = sum(tanner_catch),
            mt_wt = sum(mt_wt)) %>%
  filter(District %in% c("YAK")) %>%
  ggplot(aes(x = Season, y = tanner_catch / mt_wt, 
             group = District))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  labs(x = NULL, y = "Bycatch Ratio \n (Tanner crab : lbs scallop meat)", color = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/tanner_bycatch_ratio_area_YAK.png", plot = x,
       height = 3, width = 7, units = "in") 


## daily bycatch of Tanner crab
### summarise crab and scallop daily catch 
bycatch %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs) %>%
  dplyr::select(Season, District, Set_date, tanner_catch, mt_wt) %>%
  # join to GHL and CBL
  left_join(ghl, by = c("District", "Season")) %>%
  # add cumulative proportion of GHL or CBL
  arrange(Set_date) %>%
  mutate(ghl_remain = (ghl - cumsum(mt_wt)) / ghl,
         cbl_remain = (tanner_cbl - cumsum(tanner_catch)) / tanner_cbl) %>%
  # pivot longer for plotting
  pivot_longer(c(ghl_remain, cbl_remain), 
               names_to = "bench", values_to = "remain") %>%
  # only retain the last 6 yrs
  filter(Set_date > lubridate::ymd("2014-07-01")) -> ghl_cbl_remain
### plot KNE
ghl_cbl_remain %>%
  filter(District == "KNE") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.05))+
  facet_wrap(~Season, scales = "free", ncol = 2)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/daily_tanner_cbl_KNE.png", plot = x,
       height = 6, width = 6, units = "in")
### plot KSH
ghl_cbl_remain %>%
  filter(District == "KSH") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.05))+
  facet_wrap(~Season, scales = "free", ncol = 2)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/daily_tanner_cbl_KSH.png", plot = x,
       height = 6, width = 6, units = "in")
### plot KSW
ghl_cbl_remain %>%
  filter(District == "KSW") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.05))+
  facet_wrap(~Season, scales = "free", ncol = 2)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/daily_tanner_cbl_KSW.png", plot = x,
       height = 6, width = 6, units = "in")
### plot UB
ghl_cbl_remain %>%
  filter(District == "UB") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  scale_y_continuous(breaks = seq(-2, 1, 0.5), limits = c(-2, 1.05))+
  facet_wrap(~Season, scales = "free", ncol = 2)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/daily_tanner_cbl_UB.png", plot = x,
       height = 6, width = 6, units = "in")
### plot O
ghl_cbl_remain %>%
  filter(District == "O") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.05))+
  facet_wrap(~Season, scales = "free", ncol = 2)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/daily_tanner_cbl_O.png", plot = x,
       height = 6, width = 6, units = "in")
### plot YAK
ghl_cbl_remain %>%
  filter(District == "YAK") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.05))+
  facet_wrap(~Season, scales = "free", ncol = 2)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/daily_tanner_cbl_YAK.png", plot = x,
       height = 6, width = 6, units = "in")
### plot Q
### recompute summary including snow crab
### summarise crab and scallop daily catch 
bycatch %>%
  filter(District == "Q") %>%
  # summarise variables by day
  group_by(Season, District, Set_date) %>%
  summarise(dredge_hrs = sum(dredge_hrs), 
            sample_hrs = sum(sample_hrs),
            mt_wt = sum(mt_wt),
            bairdi_count = sum(bairdi_count),
            opilio_count = sum(opilio_count)) %>%
  # compute tanner catch rate (unsampled days get yearly total)
  group_by(Season, District) %>%
  mutate(tanner_rate = ifelse(sample_hrs == 0,
                              sum(bairdi_count) / sum(sample_hrs),
                              bairdi_count / sample_hrs),
         tanner_catch = tanner_rate * dredge_hrs,
         snow_rate = ifelse(sample_hrs == 0,
                            sum(opilio_count) / sum(sample_hrs),
                            opilio_count / sample_hrs),
         snow_catch = snow_rate * dredge_hrs) %>%
  dplyr::select(Season, District, Set_date, tanner_catch, snow_catch, mt_wt) %>%
  # join to GHL and CBL
  left_join(ghl, by = c("District", "Season")) %>%
  # add cumulative proportion of GHL or CBL
  arrange(Set_date) %>%
  mutate(ghl_remain = (ghl - cumsum(mt_wt)) / ghl,
         tan_cbl_remain = (tanner_cbl - cumsum(tanner_catch)) / tanner_cbl,
         snow_cbl_remain = (snow_cbl - cumsum(snow_catch)) / snow_cbl) %>%
  # pivot longer for plotting
  pivot_longer(c(ghl_remain, tan_cbl_remain, snow_cbl_remain), 
               names_to = "bench", values_to = "remain") %>%
  mutate(bench = factor(bench, levels = c("tan_cbl_remain", "snow_cbl_remain", "ghl_remain"))) %>%
  # only retain the last 6 yrs
  filter(Set_date > lubridate::ymd("2014-07-01")) %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[c(1, 3, 2)], labels = c("Tanner CBL", "Snow CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(-0.05, 1.05))+
  facet_wrap(~Season, scales = "free", ncol = 2) -> x
ggsave("./figures/observer_data_report/2020/daily_tanner_cbl_Q.png", plot = x,
       height = 6, width = 6, units = "in")

## Tanner crab bycatch size composition by district
crab_size %>%
  mutate(cw_bin = cut(cw, breaks = seq(0, 250, 5), labels = F),
         cw_bin = seq(5, 250, 5)[cw_bin]) %>%
  group_by(Season, District,  Species, Sex, cw_bin) %>%
  summarise(num_crab = round(sum(samp_frac))) %>%
  mutate(year = as.numeric(substring(Season, 1, 4))) %>%
  filter(year >= 2010) -> tmp
### KNE district 
filter(tmp, District == "KNE") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T) -> x
ggsave("./figures/observer_data_report/2020/tanner_size_KNE.png", plot = x,
      height = 8, width = 6, units = "in")
### KSH district 
filter(tmp, District == "KSH") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T) -> x
ggsave("./figures/observer_data_report/2020/tanner_size_KSH.png", plot = x,
       height = 8, width = 6, units = "in")
### KSW district 
filter(tmp, District == "KSW") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T) -> x
ggsave("./figures/observer_data_report/2020/tanner_size_KSW.png", plot = x,
       height = 8, width = 6, units = "in")
### UB district 
filter(tmp, District == "UB") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T) -> x
ggsave("./figures/observer_data_report/2020/tanner_size_UB.png", plot = x,
       height = 8, width = 6, units = "in")
### O district 
filter(tmp, District == "O") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T) -> x
ggsave("./figures/observer_data_report/2020/tanner_size_O.png", plot = x,
       height = 8, width = 6, units = "in")
### YAK district 
filter(tmp, District == "YAK") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T) -> x
ggsave("./figures/observer_data_report/2020/tanner_size_YAK.png", plot = x,
       height = 8, width = 6, units = "in")
### Q district 
#### tanner crab
filter(tmp, District == "Q", Species == "Tanner crab") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T) -> x
ggsave("./figures/observer_data_report/2020/tanner_size_Q.png", plot = x,
       height = 8, width = 6, units = "in")
#### snow crab
filter(tmp, District == "Q", Species == "snow crab") %>%
  ggplot()+
  geom_bar(aes(x = cw_bin, y = num_crab, fill = Sex), stat = "identity", color = "black")+
  labs(x = "Carapace width (mm)", y = "Number of crab", fill = NULL) +
  scale_fill_manual(values = cb_palette[4:6])+
  scale_x_continuous(breaks = seq(0, 250, 30))+
  facet_wrap(~Season, ncol = 2, scales = "free_y", drop = T) -> x
ggsave("./figures/observer_data_report/2020/snow_size_Q.png", plot = x,
       height = 8, width = 6, units = "in")

# discards ----
## discard in lbs and number of animals
### compute discard lbs and number
#### area K
bycatch %>%
  filter(District %in% c("KNE", "KSH", "KSW", "KSE")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
            discard_lb = discard_rate_lb * effort,
            disc_per_lb = sum(disc_count) / sum(disc_wt, broken_wt),
            discard_rate_num = (sum(disc_count) + disc_per_lb * sum(rem_disc_wt)) / sum(sample_hrs),
            discard_num = discard_rate_num * effort) -> tmp
### save plot of lbs
tmp %>%
  ggplot(aes(x = Season, y = discard_lb, color = District, group = District))+
  geom_point()+
  geom_line()+
  scale_y_continuous(labels = comma)+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Scallop Discards (lbs)")+
  theme(legend.position = "none") -> x
### save plot of number
tmp %>%
  ggplot(aes(x = Season, y = discard_num, color = District, group = District))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Scallop Discards (count)", color = NULL)+
  theme(legend.position = "bottom") -> y
## combine plots
cowplot::plot_grid(x, y + theme(legend.position = "none"), cowplot::get_legend(y),
                   ncol = 1, rel_heights = c(1, 1, 0.1)) -> z
ggsave("./figures/observer_data_report/2020/scallop_discards_area_K.png", 
       plot = z, height = 6, width = 7, units = "in")
#### area M
bycatch %>%
  filter(District %in% c("WC", "C", "UB")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
            discard_lb = discard_rate_lb * effort,
            disc_per_lb = sum(disc_count) / sum(disc_wt, broken_wt),
            discard_rate_num = (sum(disc_count) + disc_per_lb * sum(rem_disc_wt)) / sum(sample_hrs),
            discard_num = discard_rate_num * effort) -> tmp
### save plot of lbs
tmp %>%
  ggplot(aes(x = Season, y = discard_lb, color = District, group = District))+
  geom_point()+
  geom_line()+
  scale_y_continuous(labels = comma)+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Scallop Discards (lbs)")+
  theme(legend.position = "none") -> x
### save plot of number
tmp %>%
  ggplot(aes(x = Season, y = discard_num, color = District, group = District))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Scallop Discards (count)", color = NULL)+
  theme(legend.position = "bottom") -> y
## combine plots
cowplot::plot_grid(x, y + theme(legend.position = "none"), cowplot::get_legend(y),
                   ncol = 1, rel_heights = c(1, 1, 0.1)) -> z
ggsave("./figures/observer_data_report/2020/scallop_discards_area_M.png", 
       plot = z, height = 6, width = 7, units = "in")
#### area O
bycatch %>%
  filter(District %in% c("O")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
            discard_lb = discard_rate_lb * effort,
            disc_per_lb = sum(disc_count) / sum(disc_wt, broken_wt),
            discard_rate_num = (sum(disc_count) + disc_per_lb * sum(rem_disc_wt)) / sum(sample_hrs),
            discard_num = discard_rate_num * effort) -> tmp
### save plot of lbs
tmp %>%
  ggplot(aes(x = Season, y = discard_lb, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Scallop Discards (lbs)")+
  theme(legend.position = "none") -> x
### save plot of number
tmp %>%
  ggplot(aes(x = Season, y = discard_num, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  labs(x = NULL, y = "Scallop Discards (count)", color = NULL)+
  theme(legend.position = "bottom") -> y
## combine plots
cowplot::plot_grid(x, y, ncol = 1) -> z
ggsave("./figures/observer_data_report/2020/scallop_discards_area_O.png", 
       plot = z, height = 6, width = 7, units = "in")
#### area Q
bycatch %>%
  filter(District %in% c("Q")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
            discard_lb = discard_rate_lb * effort,
            disc_per_lb = sum(disc_count) / sum(disc_wt, broken_wt),
            discard_rate_num = (sum(disc_count) + disc_per_lb * sum(rem_disc_wt)) / sum(sample_hrs),
            discard_num = discard_rate_num * effort) -> tmp
### save plot of lbs
tmp %>%
  ggplot(aes(x = Season, y = discard_lb, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Scallop Discards (lbs)")+
  theme(legend.position = "none") -> x
### save plot of number
tmp %>%
  ggplot(aes(x = Season, y = discard_num, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  labs(x = NULL, y = "Scallop Discards (count)", color = NULL)+
  theme(legend.position = "bottom") -> y
## combine plots
cowplot::plot_grid(x, y, ncol = 1) -> z
ggsave("./figures/observer_data_report/2020/scallop_discards_area_Q.png", 
       plot = z, height = 6, width = 7, units = "in")
#### area YAK
bycatch %>%
  filter(District %in% c("YAK")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            discard_rate_lb = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
            discard_lb = discard_rate_lb * effort,
            disc_per_lb = sum(disc_count) / sum(disc_wt, broken_wt),
            discard_rate_num = (sum(disc_count) + disc_per_lb * sum(rem_disc_wt)) / sum(sample_hrs),
            discard_num = discard_rate_num * effort) -> tmp
### save plot of lbs
tmp %>%
  ggplot(aes(x = Season, y = discard_lb, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  scale_y_continuous(labels = comma)+
  labs(x = NULL, y = "Scallop Discards (lbs)")+
  theme(legend.position = "none") -> x
### save plot of number
tmp %>%
  ggplot(aes(x = Season, y = discard_num, group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  labs(x = NULL, y = "Scallop Discards (count)", color = NULL)+
  theme(legend.position = "bottom") -> y
## combine plots
cowplot::plot_grid(x, y, ncol = 1) -> z
ggsave("./figures/observer_data_report/2020/scallop_discards_area_YAK.png", 
       plot = z, height = 6, width = 7, units = "in")


### export tables by district
#### summarise round weight by season and district
catch %>%
  group_by(Season, District) %>%
  summarise(round_weight = sum(round_weight, na.rm = T)) %>%
  # join with 'tmp' (discard estimates)
  right_join(tmp, by = c("District", "Season")) %>%
  # compute discard lbs ratio and overwrite object 'tmp'
  mutate(discard_ratio = discard_lb / round_weight) -> tmp
#### KNE
tmp %>%
  filter(District == "KNE") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
         discard_rate_lb, discard_rate_num) %>%
  write_csv("./output/observer_summary/2020/discard_summary_KNE.csv")
#### KSH
tmp %>%
  filter(District == "KSH") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
         discard_rate_lb, discard_rate_num) %>%
  write_csv("./output/observer_summary/2020/discard_summary_KSH.csv")
#### KSW
tmp %>%
  filter(District == "KSW") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
         discard_rate_lb, discard_rate_num) %>%
  write_csv("./output/observer_summary/2020/discard_summary_KSW.csv")
#### KSE
tmp %>%
  filter(District == "KSE") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
         discard_rate_lb, discard_rate_num) %>%
  write_csv("./output/observer_summary/2020/discard_summary_KSE.csv")
#### UB
tmp %>%
  filter(District == "UB") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
         discard_rate_lb, discard_rate_num) %>%
  write_csv("./output/observer_summary/2020/discard_summary_UB.csv")
#### C
tmp %>%
  filter(District == "C") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
         discard_rate_lb, discard_rate_num) %>%
  write_csv("./output/observer_summary/2020/discard_summary_C.csv")
#### O
tmp %>%
  filter(District == "O") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
                discard_rate_lb, discard_rate_num) %>%
  write_csv("./output/observer_summary/2020/discard_summary_O.csv")
#### Q
tmp %>%
  filter(District == "Q") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
                discard_rate_lb, discard_rate_num) %>%
  write_csv("./output/observer_summary/2020/discard_summary_Q.csv")
#### YAK
tmp %>%
  filter(District == "YAK") %>%
  dplyr::select(Season, round_weight, discard_lb, discard_num, discard_ratio, 
                discard_rate_lb, discard_rate_num) %>%
  write_csv("./output/observer_summary/2020/discard_summary_YAK.csv")

## discard ratio, round weight and num animals
### area K
bycatch %>%
  filter(sample_hrs != 0,
         District %in% c("KNE", "KSH", "KSW", "KSE")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            sample_hrs = sum(sample_hrs),
            round_weight = sum(est_rnd_wt),
            disc_lbs = sum(disc_wt, broken_wt, rem_disc_wt),
            tot_disc = disc_lbs / sample_hrs * effort,
            ratio = tot_disc / round_weight) %>%
  ggplot(aes(x = Season, y = ratio, 
             group = District, color = District))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Discard Ratio \n (Round lbs discarded : retained)")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/scallop_discard_ratio_area_K.png", plot = x,
       height = 3, width = 7, units = "in") 
### area M
bycatch %>%
  filter(sample_hrs != 0,
         District %in% c("WC", "C", "UB")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            sample_hrs = sum(sample_hrs),
            round_weight = sum(est_rnd_wt),
            disc_lbs = sum(disc_wt, broken_wt, rem_disc_wt),
            tot_disc = disc_lbs / sample_hrs * effort,
            ratio = tot_disc / round_weight) %>%
  ggplot(aes(x = Season, y = ratio, 
             group = District, color = District))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Discard Ratio \n (Round lbs discarded : retained)")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/scallop_discard_ratio_area_M.png", plot = x,
       height = 3, width = 7, units = "in") 
### area O
bycatch %>%
  filter(sample_hrs != 0,
         District %in% c("O")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            sample_hrs = sum(sample_hrs),
            round_weight = sum(est_rnd_wt),
            disc_lbs = sum(disc_wt, broken_wt, rem_disc_wt),
            tot_disc = disc_lbs / sample_hrs * effort,
            ratio = tot_disc / round_weight) %>%
  ggplot(aes(x = Season, y = ratio, 
             group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Discard Ratio \n (Round lbs discarded : retained)")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/scallop_discard_ratio_area_O.png", plot = x,
       height = 3, width = 7, units = "in") 
### area Q
bycatch %>%
  filter(sample_hrs != 0,
         District %in% c("Q")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            sample_hrs = sum(sample_hrs),
            round_weight = sum(est_rnd_wt),
            disc_lbs = sum(disc_wt, broken_wt, rem_disc_wt),
            tot_disc = disc_lbs / sample_hrs * effort,
            ratio = tot_disc / round_weight) %>%
  ggplot(aes(x = Season, y = ratio, 
             group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Discard Ratio \n (Round lbs discarded : retained)")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/scallop_discard_ratio_area_Q.png", plot = x,
       height = 3, width = 7, units = "in")
### area YAK
bycatch %>%
  filter(sample_hrs != 0,
         District %in% c("YAK")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            sample_hrs = sum(sample_hrs),
            round_weight = sum(est_rnd_wt),
            disc_lbs = sum(disc_wt, broken_wt, rem_disc_wt),
            tot_disc = disc_lbs / sample_hrs * effort,
            ratio = tot_disc / round_weight) %>%
  ggplot(aes(x = Season, y = ratio, 
             group = 1))+
  geom_point(color = cb_palette[1])+
  geom_line(color = cb_palette[1])+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Discard Ratio \n (Round lbs discarded : retained)")+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/scallop_discard_ratio_area_YAK.png", plot = x,
       height = 3, width = 7, units = "in")



# shell height composition ----
## create dataset with SH and appropriate weights
bycatch %>%
  # compute a daily discard rate (lbs/dregde hr)
  group_by(Season, District, Set_date) %>%
  summarise(disc_wt = sum(disc_wt, broken_wt, rem_disc_wt),
            sample = sum(sample_hrs)) %>%
  group_by(Season, District) %>%
  mutate(disc_rate = ifelse(sample != 0, 
                            disc_wt / sample, 
                            sum(disc_wt) / sum(sample))) %>%
  # join to catch data by haul
  dplyr::select(Season, District, Set_date, disc_rate) %>%
  right_join(catch, by = c("Season", "District", "Set_date")) %>%
  # estimate discards by haul
  mutate(disc_est_lbs = dredge_hrs * disc_rate) %>%
  # estimate weights for shell height histogram (prop of annual catch)
  dplyr::select(Season, District, Haul_ID, round_weight, disc_est_lbs) %>%
  pivot_longer(c(round_weight, disc_est_lbs), 
               names_to = "Rtnd_disc", values_to = "wt_lbs") %>%
  mutate(Rtnd_disc = ifelse(Rtnd_disc == "round_weight", "R", "D"),
         w = wt_lbs / sum(wt_lbs, na.rm = T)) %>%
  ungroup() %>%
  dplyr::select(Haul_ID, Rtnd_disc, w) %>%
right_join(shell_height, by = c("Haul_ID", "Rtnd_disc")) -> tmp
# KNE
tmp %>%
  filter(District == "KNE") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3) -> x
ggsave("./figures/observer_data_report/2020/sh_comp_KNE.png", plot = x,
       height = 6, width = 7, units = "in")
# KSH
tmp %>%
  filter(District == "KSH") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3) -> x
ggsave("./figures/observer_data_report/2020/sh_comp_KSH.png", plot = x,
       height = 6, width = 7, units = "in")
# KSW
tmp %>%
  filter(District == "KSW") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3) -> x
ggsave("./figures/observer_data_report/2020/sh_comp_KSW.png", plot = x,
       height = 6, width = 7, units = "in")
# KSE
tmp %>%
  filter(District == "KSE") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3) -> x
ggsave("./figures/observer_data_report/2020/sh_comp_KSE.png", plot = x,
       height = 6, width = 7, units = "in")
# UB
tmp %>%
  filter(District == "UB") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3) -> x
ggsave("./figures/observer_data_report/2020/sh_comp_UB.png", plot = x,
       height = 6, width = 7, units = "in")
# C
tmp %>%
  filter(District == "C") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3) -> x
ggsave("./figures/observer_data_report/2020/sh_comp_C.png", plot = x,
       height = 6, width = 7, units = "in")
# area M
tmp %>%
  filter(District %in% c("WC", "C", "UB")) %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3) -> x
ggsave("./figures/observer_data_report/2020/sh_comp_area_M.png", plot = x,
       height = 6, width = 7, units = "in")
# area O
tmp %>%
  filter(District == "O") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3) -> x
ggsave("./figures/observer_data_report/2020/sh_comp_O.png", plot = x,
       height = 6, width = 7, units = "in")
# area Q
tmp %>%
  filter(District == "Q") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3) -> x
ggsave("./figures/observer_data_report/2020/sh_comp_Q.png", plot = x,
       height = 6, width = 7, units = "in")
# area YAK
tmp %>%
  filter(District == "YAK") %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_wrap(~Season, ncol = 3) -> x
ggsave("./figures/observer_data_report/2020/sh_comp_YAK.png", plot = x,
       height = 6, width = 7, units = "in")






# area K ----

## plot meat weight ~ 10% round weight
catch %>%
  filter(District %in% c("KNE", "KSH", "KSW", "KSE")) %>%
  group_by(Season, District) %>%
  summarise(meat_weight = sum(meat_weight),
            round_weight = sum(round_weight * 0.10)) %>%
  pivot_longer(c(3, 4), names_to = "type", values_to = "weight") %>%
  ggplot(aes(x = Season, y = weight, group = type, linetype = type, shape = type))+
  scale_linetype_manual(values = c(1, 2), 
                        labels = c("Meat Weight", "10% Round Weight"))+
  scale_shape_manual(values = c(16, 1), 
                     labels = c("Meat Weight", "10% Round Weight"))+
  geom_point()+
  geom_line()+
  labs(x = NULL, y = "Annual Catch (lbs)", linetype = NULL, shape = NULL)+
  facet_wrap(~District, scales = "free_y")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90, vjust = 0.5)) -> x
ggsave("./figures/observer_data_report/2020/annual_catch_area_K.png", 
       plot = x, height = 5, width = 6, units = "in")

  





## interannual trend in nominal meat weight cpue, all districts
catch %>%
  filter(District %in% c("KNE", "KSH", "KSW", "KSE")) %>%
  group_by(Season, District) %>%
  summarise(mw_cpue = sum(meat_weight, na.rm = T) / sum(dredge_hrs, na.rm = T)) %>%
  ggplot()+
  geom_point(aes(x = Season, y = mw_cpue, colour = District))+
  geom_line(aes(x = Season, y = mw_cpue, group = District, colour = District))+
  labs(y = "Nominal CPUE (meat lbs / dredge hr)", x = NULL)+
  scale_color_manual(values = cb_palette)+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))




