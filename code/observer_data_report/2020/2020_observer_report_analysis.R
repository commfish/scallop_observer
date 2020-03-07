# notes ----

## 2020 observer data summary analysis
## organized by management area
## Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2020/2/27

# load libraries and set global options ----

## packages
library(tidyverse)
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
theme_set(theme_sleek())
### cutsom axis ticks for yrs (from FNGr)

# data ----


## metadata
### ghl
ghl <- read_csv("./data/metadata/ghl.csv")


## observer/logbook data
### scallop haul data 2009/10 - Present
catch <- do.call(bind_rows,
                 lapply(paste0("data/catch/", list.files("data/catch/")), read_csv))




# data mgmt ----

## remove phantom rows in catch tibble (can't figure out where they come from...)
catch %>%
  drop_na(names(.)) -> catch

## rename fields in current data (2009 - present)
### scallop haul data
catch <- f_catch_rename(catch)


## add Season to data
catch <- f_add_season(catch)


# area K ----

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

## map of dredge locations by district (f_base_map from general functions)
### KNE
f_base_map +
  geom_point(data = filter(catch, District == "KNE"), aes(x = set_lon, y = set_lat), alpha = 0.5) +
  KNE_proj+
  facet_wrap(~Season, nrow = 2)
### KSH
f_base_map +
  geom_point(data = filter(catch, District == "KSH"), aes(x = set_lon, y = set_lat)) +
  KSH_proj
### KSW
f_base_map +
  geom_point(data = filter(catch, District == "KSW"), aes(x = set_lon, y = set_lat)) +
  KSW_proj
### KSE
f_base_map +
  geom_point(data = filter(catch, District == "KSE"), aes(x = set_lon, y = set_lat)) +
  KSE_proj


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
  select(Season, grid) %>%
  unnest(grid) -> tmp
# plot map
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.1, 0.3, 0.5))+
  KNE_proj+
  facet_wrap(~Season, nrow = 2) -> x
ggsave("./figures/observer_data_report/effort_map_KNE.png", plot = x, 
       height = 8, width = 10, unit = "in")
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
  facet_wrap(~Season, nrow = 2) -> x
ggsave("./figures/observer_data_report/effort_map_KSH.png", plot = x, 
       height = 8, width = 10, unit = "in")
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
  scale_x_continuous(breaks = seq(-156.5, -154.5, 0.6))+
  facet_wrap(~Season, nrow = 2, drop = F) -> x
ggsave("./figures/observer_data_report/effort_map_KSW.png", plot = x, 
       height = 8, width = 10, unit = "in")
### KSE
catch %>%
  filter(District == "KSE") %>%
  rename(long = set_lon, lat = set_lat) %>%
  group_by(Season) %>%
  mutate(prop_effort = dredge_hrs/sum(dredge_hrs)) %>%
  nest(data = -Season) %>%
  mutate(grid = purrr::map(data, f_make_grid, long = KSE_proj$limits$x,
                           lat = KSE_proj$limits$y, by = c(0.1, 0.05), 
                           values = "prop_effort")) %>%
  select(Season, grid) %>%
  unnest(grid) -> tmp
f_base_map+
  geom_polygon(data = drop_na(tmp), aes(x = long, y = lat, group = group, fill = prop_effort))+
  labs(fill = "Proportion \n Effort")+
  scale_fill_gradientn(colors = topo.colors(5), values = c(0, 0.1, 0.2, 1),
                       breaks = c(0.01, 0.05, 0.1, 0.15))+
  KSE_proj+
  facet_wrap(~Season, nrow = 2, drop = T) -> x
ggsave("./figures/observer_data_report/effort_map_KSE.png", plot = x, 
       height = 6, width = 4, unit = "in")


## extent of round weight catch (f_extent_catch from general_observer_data_functions.R)
### KNE
catch %>%
  filter(District == "KNE") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight) / sum(dredge_hrs),
            Extent = mean(extent) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/cpue_extent_KNE.png", plot = x, 
       height = 3, width = 7, unit = "in")  
### KSH
catch %>%
  filter(District == "KSH") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight) / sum(dredge_hrs),
            Extent = mean(extent) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/cpue_extent_KSH.png", plot = x, 
       height = 3, width = 7, unit = "in")
### KSW
catch %>%
  filter(District == "KSW") %>%
  nest(data = -Season) %>%
  mutate(extent = purrr::map_dbl(data, f_extent_catch)) %>%
  unnest(data) %>%
  group_by(Season) %>%
  summarise(CPUE = sum(round_weight) / sum(dredge_hrs),
            Extent = mean(extent) * 1000) %>%
  pivot_longer(c("CPUE", "Extent"), names_to = "metric", values_to = "value") %>%
  ggplot(aes(x = Season, y = value, linetype = metric, group = metric))+
  geom_point()+
  geom_line()+
  scale_y_continuous(sec.axis = sec_axis(~./1000, name = "Extent"))+
  labs(x = NULL, y = "CPUE (round lbs / dredge hr)", linetype = NULL)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/cpue_extent_KSW.png", plot = x, 
       height = 3, width = 7, unit = "in")



# area D ----

## fishery performance table
fish_stats(catch, c("D", "D16", "YAK"), add_ghl = T, 
           path = "./output/observer_summary/2020/fish_stats_D.csv")


