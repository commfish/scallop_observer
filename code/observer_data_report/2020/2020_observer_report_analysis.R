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
### bycatch by day 2009/10 - Present
bycatch <- do.call(bind_rows,
                 lapply(paste0("data/bycatch/", list.files("data/bycatch/")), read_csv))
### shell heights 2009/10 - Present
shell_height <- do.call(bind_rows,
                   lapply(paste0("data/shell_height/", 
                                 list.files("data/shell_height/")), read_csv))


# data mgmt ----

## remove phantom rows in catch tibble (can't figure out where they come from...)
catch %>%
  drop_na(names(.)) -> catch

## rename fields in current data (2009 - present)
f_catch_rename(catch) %>%
## add Season to data
f_add_season() %>%
## classify Karluk bed as KSW district instead of KSH
f_revise_district() %>% 
## coerce date to date class
mutate(Set_date = lubridate::mdy(Set_date))-> catch

## rename fields in bycatch data (2009 - present)
f_bycatch_rename(bycatch)%>%
## add Season to data
f_add_season() %>%
## coerce date to date class
mutate(Set_date = lubridate::mdy(Set_date)) -> bycatch

## rename fields in shell_height data (2009 - present) 
f_shell_height_rename(shell_height) %>%
## add Season to data
f_add_season() %>%
## revise District as in catch data
mutate(District = catch$District[match(.$Haul_ID, catch$Haul_ID)]) -> shell_height
  
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

## discard in lbs and number of animals
### discard lbs
bycatch %>%
  filter(District %in% c("KNE", "KSH", "KSW", "KSE")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            discard_rate = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
            discard_wt = discard_rate * effort) %>%
  ggplot(aes(x = Season, y = discard_wt, color = District, group = District))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Scallop Discards (lbs)")+
  theme(legend.position = c(0.92, 0.75)) -> x
ggsave("./figures/observer_data_report/2020/scallop_discards_lbs_area_K.png", plot = x,
       height = 3, width = 7, units = "in")  
### discard in lbs (zoomed in)
bycatch %>%
  filter(District %in% c("KNE", "KSH", "KSW", "KSE")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            discard_rate = sum(disc_wt, broken_wt, rem_disc_wt) / sum(sample_hrs),
            discard_wt = discard_rate * effort) %>%
  ggplot(aes(x = Season, y = discard_wt, color = District, group = District))+
  geom_point()+
  geom_line()+
  coord_cartesian(ylim=c(0, 150000))+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Scallop Discards (lbs)")+
  theme(legend.position = c(0.08, 0.35)) -> x
ggsave("./figures/observer_data_report/2020/scallop_discards_lbs_zoomed_area_K.png", plot = x,
       height = 3, width = 7, units = "in")
### discard number of animals
bycatch %>%
  filter(District %in% c("KNE", "KSH", "KSW", "KSE")) %>%
  group_by(Season, District) %>%
  summarise(effort = sum(dredge_hrs),
            disc_per_lb = sum(disc_count) / sum(disc_wt, broken_wt),
            disc_rate_num = (sum(disc_count) + disc_per_lb * sum(rem_disc_wt)) / sum(sample_hrs),
            total_disc_num = disc_rate_num * effort) %>%
  ggplot(aes(x = Season, y = total_disc_num, color = District, group = District))+
  geom_point()+
  geom_line()+
  scale_colour_manual(values = cb_palette[1:4])+
  labs(x = NULL, y = "Scallop Discards (animals)")+
  theme(legend.position = c(0.08, 0.55)) -> x
ggsave("./figures/observer_data_report/2020/scallop_discards_num_animals_area_K.png", 
       plot = x, height = 3, width = 7, units = "in")

## discard ratio, round weight and num animals
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
  theme(legend.position = c(0.08, 0.75)) -> x
ggsave("./figures/observer_data_report/2020/scallop_discard_ratio_area_K.png", plot = x,
       height = 3, width = 7, units = "in") 

## total crab bycatch by Season, District
bycatch %>%
  group_by(Season, District) %>%
  summarise(total_effort = sum(dredge_hrs),
            total_sample = sum(sample_hrs),
            tanner_rate = sum(bairdi_count) / total_sample,
            total_tanner = tanner_rate * total_effort,
            dungeness_rate = sum(dungeness_count) / total_sample,
            total_dungeness = dungeness_rate * total_effort,
            halibut_rate = sum(halibut_count) / total_sample,
            total_halibut = halibut_rate * total_effort,
            king_tot = sum(king_count)) %>%
  write_csv("./output/observer_summary/2020/bycatch_totals_by_district_area_K.csv")

## daily bycatch of Tanner crab
### summarise crab and scallop daily catch 
bycatch %>%
  # summarise variable by day
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
         cbl_remain = (cbl - cumsum(tanner_catch)) / cbl) %>%
  # pivot longer for plotting
  pivot_longer(c(ghl_remain, cbl_remain), 
               names_to = "bench", values_to = "remain") -> ghl_cbl_remain
### plot KNE
ghl_cbl_remain %>%
  filter(District == "KNE") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = remain, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y =  "Percent Remaining", color = NULL)+
  facet_wrap(~Season, scales = "free", ncol = 2)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/daily_tanner_cbl_KNE.png", plot = x,
       height = 7, width = 5, units = "in")
### plot KSH
ghl_cbl_prop %>%
  filter(District == "KSH") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = prop, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y = "Proportion of managment quantity", color = NULL)+
  facet_wrap(~Season, scales = "free_x", ncol = 2)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/daily_tanner_cbl_KSH.png", plot = x,
       height = 7, width = 5, units = "in")
### plot KSW
ghl_cbl_prop %>%
  filter(District == "KSW") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = prop, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y = "Proportion of managment quantity", color = NULL)+
  facet_wrap(~Season, scales = "free_x", ncol = 2)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/daily_tanner_cbl_KSW.png", plot = x,
       height = 7, width = 5, units = "in")
### plot KSE
ghl_cbl_prop %>%
  filter(District == "KSE") %>%
  ggplot()+
  geom_line(aes(x = Set_date, y = prop, group = bench, col = bench))+
  scale_color_manual(values = cb_palette[1:2], labels = c("Tanner CBL", "GHL"))+
  labs(x = NULL, y = "Proportion of managment quantity", color = NULL)+
  facet_wrap(~Season, scales = "free_x", ncol = 2)+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/daily_tanner_cbl_KSE.png", plot = x,
       height = 7, width = 5, units = "in")

## shell height composition (all districts)
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
         w = wt_lbs / sum(wt_lbs)) %>%
  # join to shell height
  ungroup() %>%
  dplyr::select(Haul_ID, Rtnd_disc, w) %>%
  right_join(shell_height, by = c("Haul_ID", "Rtnd_disc")) %>%
  # plot
  filter(District %in% c("KNE", "KSH", "KSW", "KSE"),
         Season %in% c("2015/16", "2016/17", "2017/18", "2018/19", "2019/20")) %>%
  ggplot()+
  geom_histogram(aes(x = sh, y = ..density.., weight = w, fill = Rtnd_disc), 
                 binwidth = 5, color = "black")+
  scale_fill_manual(values = cb_palette[2:3], labels = c("Discarded", "Retained"))+
  labs(x = "Shell height (mm)", y = "Weighted Density", fill = NULL)+
  facet_grid(cols = vars(District), rows = vars(Season))+
  theme(legend.position = "bottom") -> x
ggsave("./figures/observer_data_report/2020/sh_comp_area_K.png", plot = x,
       height = 6, width = 8, units = "in")
















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
ggsave("./figures/observer_data_report/2020/effort_map_KNE.png", plot = x, 
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
ggsave("./figures/observer_data_report/2020/effort_map_KSH.png", plot = x, 
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
ggsave("./figures/observer_data_report/2020/effort_map_KSW.png", plot = x, 
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
ggsave("./figures/observer_data_report/2020/effort_map_KSE.png", plot = x, 
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
ggsave("./figures/observer_data_report/2020/cpue_extent_KNE.png", plot = x, 
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
ggsave("./figures/observer_data_report/2020/cpue_extent_KSH.png", plot = x, 
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
ggsave("./figures/observer_data_report/2020/cpue_extent_KSW.png", plot = x, 
       height = 3, width = 7, unit = "in")



# area D ----

## fishery performance table
fish_stats(catch, c("D", "D16", "YAK"), add_ghl = T, 
           path = "./output/observer_summary/2020/fish_stats_D.csv")


