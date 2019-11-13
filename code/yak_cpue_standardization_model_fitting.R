# notes ----
# CPUE standardization in YAK district
# author: Tyler Jackson
# contact: tyler.jackson@alaska.gov
# date: 2019-11-13

# load ----
library(tidyverse)
library(GGally)
library(lubridate)
library(mgcv)
library(FNGr); theme_set(theme_sleek())

source("./code/gam_functions.R")

### define function for fitting smoother computing derivative (tidal index)
f_smooth_deriv <- function(x){
  spl <- smooth.spline(x$tide_level ~ x$t)
  x$tidal_index <- predict(spl, deriv = 1)$y
  x
}

# data ----
# Observer catch data 2009 - 2018
## Catch by Haul from the Wiki
scallops <- do.call(bind_rows,
                    lapply(paste0("data/catch/", list.files("data/catch/")), read_csv))
# Yakutat Bay hourly tide level (annomally in ft from mean water level)
## NOAA Tides and Currents
tide <- read.table("./data/environmental/yakutat bay tide 2009 - 2019/yakutat_tide_predictions_jan2009_dec2019.txt",
                   header = T, skip = 13)

# Cap Suckling Data Buoy
## National Data Buoy Center
buoy <- do.call(bind_rows,
                  lapply(paste0("data/environmental/cape suckling buoy/", 
                                list.files("data/environmental/cape suckling buoy")), read_csv))


## rename scallop data fields for consistency
names(scallops) <- c("Fishery", "District", "ADFG", "Trip_ID", "Haul_ID", 
                     "haul", "gear_perf", "haul_sampled", "Set_date", "set_lat",
                     "set_lon", "statarea", "depth", "dredge_count", 
                     "dredge_width", "dredge_down", "dredge_up", "duration", 
                     "dredge_hrs", "haul_speed", "distance_nm", "area_swept", 
                     "rtnd_basket", "round_weight", "meat_weight", "est_yield",
                     "tot_rtnd_basket", 'tot_day_meat_weight')

## Scallop data manipulation
## adjust dates and set variables
## remove data where dredge hours & area swept are zero
## calculate round weight cpue by time
## calculate meat weight cpue by time
## only include Yakutat Area districts (combined ~2018)
## add bed 
## retain only complete data
scallops %>%
  mutate(set_date = mdy(Set_date),
         year = year(set_date),
         date = yday(set_date),
         set_hr = hour(dredge_down),
         year = ifelse(date < 100, year - 1, year), # fishing year
         Year = factor(year),
         Vessel = factor(ADFG),
         FY = paste0(year, "/", sprintf('%02d', (year + 1) %% 100)),
         dum = 1) %>%
  filter(area_swept>0) %>%
  mutate(rw_cpue = round_weight / dredge_hrs,
         mw_cpue = meat_weight / dredge_hrs,
         bed = case_when(set_lon <= -138.3  & set_lon > -138.79 ~ 5,
                         set_lon <= -138.79 & set_lon > -140.1  ~ 4,
                         set_lon <= -140.1  & set_lon > -142.15 ~ 3,
                         set_lon <= -142.15 & set_lon > -142.7  ~ 2,
                         set_lon <= -142.7  & set_lon > -143.5  ~ 1,
                         set_lon <= -143.5 ~ 0,
                         TRUE ~ 6),
         Bed = factor(bed)) %>%
  filter(District %in% c("YAK", "D16", "D")) %>%
  .[complete.cases(.),] -> scallops

## Tide data manipulation
## change filed names
## separate date into year, month, day, hr
## nest with day (to improve smoother resolution)
## fit a smoother using a cubic spline and retrun the derivative (proxy for index tidal current intensity)
## join to scallop data
tide %>%
  as_tibble %>%
  mutate(set_date = ymd(Date), 
         year = year(set_date),
         month = month(set_date),
         day = day(set_date),
         set_hr = as.numeric(substring(Time, 1, 2))) %>%
  arrange(set_date) %>%
  mutate(t = 1:nrow(.)) %>%
  rename(tide_level = Pred) %>%
  select(5:10, 4) %>%
  nest(-set_date) %>%
  mutate(data = map(data, f_smooth_deriv)) %>%
  unnest(data) %>%
  mutate(tidal_index = abs(tidal_index),
         set_hr = set_hr) -> tide 

## Buoy data manipulation
## filter out superfluous rows
## create set_date
## rename fields
## change missing data to NA
## covert wave height to ft
## join with scallop data
buoy %>%
  filter(complete.cases(.)) %>%
  mutate(set_date = paste0(`#YY`, "-",MM, "-", DD),
         set_date = ymd(set_date),
         set_hr = hh) %>%
  select(set_date, set_hr, WDI, `R WSP`, WVHT) %>%
  rename(wind_dir = WDI, 
         wind_sp = `R WSP`,
         wave_ht = WVHT) %>%
  mutate(wind_dir = ifelse(wind_dir == 999, NA, wind_dir),
         wind_sp = ifelse(wind_sp == 99, NA, wind_sp),
         wave_ht = ifelse(wave_ht == 99, NA, wave_ht),
         wave_ht = wave_ht * 3.28084) -> buoy

## join scallop, tide, and buoy data
buoy %>%
  right_join(scallops, by = c("set_date", "set_hr")) %>%
  left_join(tide, by = c("set_date", "set_hr")) %>%
  select(-year.y, -month, -day, -t) -> scallops_comb

## check for missing buoy data
scallops_comb %>%
  filter(is.na(wave_ht)) %>%
  count(Year, month(set_date))

## compute the month average weather conditions
buoy %>%
  mutate(month = month(set_date)) %>%
  group_by(month) %>%
  summarize(wind_dir_avg = mean(wind_dir, na.rm = T),
            wind_sp_avg = mean(wind_sp, na.rm=T),
            wave_ht_avg = mean(wave_ht, na.rm = T)) -> mon.avg

## replace missing weather data with the long term monthly average
scallops_comb %>%
  mutate(month = month(set_date)) %>%
  left_join(mon.avg, by = "month") %>%
  mutate(wind_dir = ifelse(is.na(wind_dir), wind_dir_avg, wind_dir),
         wind_sp = ifelse(is.na(wind_sp), wind_sp_avg, wind_sp),
         wave_ht = ifelse(is.na(wave_ht), wave_ht_avg, wave_ht)) %>%
  select(-month, -wind_dir_avg, -wind_sp_avg, -wave_ht_avg) -> scallops_comb


## dump dredges that were not satisfactory
scallops_comb %>%
  filter(gear_perf == 1) -> scallops_comb

# eda ----

## correlation of all relevant continuous variables
scallops_comb %>%
  select(depth, wind_dir, wind_sp, wave_ht, tidal_index, tide_level, haul_speed, area_swept, rw_cpue) %>%
  GGally::ggpairs()

## view interannual trend in mean rw_cpue by bed
scallops_comb %>%
  group_by(Bed, Year) %>%
  summarize(rw_cpue = mean(rw_cpue, na.rm=T)) %>%
  ggplot()+
  geom_point(aes(x = Year, y = rw_cpue, col = Bed))+
  geom_line(aes(x = Year, y = rw_cpue, group = Bed, col = Bed))

## view differences in rw_cpue by vessel and bed within year
scallops_comb %>%
  ggplot()+
  geom_boxplot(aes(x = Vessel, y = rw_cpue, col = Bed))+
  facet_wrap(~Year)

# model fitting ----

## define cpue adjustment as 10% of the all time average (CPUE must be positive for gamma distribution)
adj <- mean(scallops_comb$rw_cpue, na.rm = T) * 0.10

## Model with just Year
### fit model
mod_0 <- bam(rw_cpue + adj ~ Year, data = scallops_comb, gamma = 1.4, family = Gamma(link = log))
### examine diagnostics
plot(mod_0, all.terms = T, shade = T, residuals = T, cex = 1, pch = 1, pages = 1)
dev.off()
gam.check(mod_0)
f_resid_plots(mod_0)

## Model with linear Year and Bed
### fit model
mod_1 <- update(mod_0, ~. + Bed)
### examine diagnostics
plot(mod_1, all.terms = T, shade = T, residuals = T, cex = 1, pch = 1, pages = 1)
dev.off()
gam.check(mod_1)
f_resid_plots(mod_1)

## fit model with interaction between bed and year
mod_1.1 <- bam(rw_cpue + adj ~ Year * Bed, data = scallops_comb, gamma = 1.4, family = Gamma(link = log))
### examine diagnostics
summary(mod_1.1)
gam.check(mod_1.1)
f_resid_plots(mod_1.1)

## Model with linear Year, Bed, and Vessel
### fit model
mod_2 <- update(mod_1.1, ~. + Vessel)
### examine diagnostics
summary(mod_2)
plot(mod_2, all.terms = T, shade = T, residuals = T, cex = 1, pch = 1, pages = 1)
dev.off()
gam.check(mod_2)
f_resid_plots(mod_2)

## Model with smoothed depth, linear Year, Bed, and Vessel
### fit model
mod_3 <- update(mod_2, ~. + s(depth))
### examine diagnostics
summary(mod_3)
plot(mod_3, all.terms = T, shade = T, residuals = T, cex = 0.3, pch = 1, pages = 1)
dev.off()
gam.check(mod_3)
f_resid_plots(mod_3)

### fit model with interaction and fewer degrees of freedom on depth
mod_3.1 <- update(mod_2, ~. + s(depth, by = Bed, k = 4))
### examine diagnostics
summary(mod_3.1)
plot(mod_3.1, all.terms = T, shade = T, residuals = T, pages = 1)
dev.off()
gam.check(mod_3.1)
f_resid_plots(mod_3.1)
ggplot(scallops_comb)+
  geom_point(aes(x = depth, y = resid(mod_3.1)))+
  geom_hline(yintercept = 0, linetype = 2, col = "red")+
  facet_wrap(~Bed)

## Model with smoothed depth and wave ht, linear Year, Bed, and Vessel
### fit model with interaction
mod_4 <- update(mod_3.1, ~. + s(wave_ht))
### examine diagnostics
summary(mod_4) # edf for wave ht seems larger than needed given data
plot(mod_4, all.terms = T, shade = T, residuals = T, pages = 1) 
dev.off()
gam.check(mod_4)
f_resid_plots(mod_4)
### refit model restricting degrees of freedom on wave height 
mod_4.1 <- update(mod_3.1, ~. + s(wave_ht, k = 3))
summary(mod_4.1)
gam.check(mod_4.1)
lmtest::lrtest(mod_4.1, mod_4) # adding extra df (larger k) does not signifcantly increase model fit, go with k = 3

## Model with smoothed depth, wave ht, and wind sp, and linear Year, Bed, and Vessel
mod_5 <- update(mod_4.1, ~. + s(wind_sp))
### examine diagnostics
summary(mod_5)# edf for wave ht seems larger than needed given data
plot(mod_5, select = 9, shade = T, residuals = T, cex = 0.3, pch = 1) 
f_resid_plots(mod_5)
### refit model restricting degrees of freedom on wind speed 
mod_5.1 <- update(mod_4.1, ~. + s(wind_sp, k = 3))
summary(mod_5.1)
gam.check(mod_5.1)

## Model with smoothed depth, wave ht, wind sp, and tidal_index, and linear Year, Bed, and Vessel
mod_6 <- update(mod_5.1, ~. + s(tidal_index))
summary(mod_6)
plot(mod_6, select = 10, shade = T, residuals = T, cex = 0.3, pch = 1) 

## Model with smoothed depth, wave ht, wind sp, and haul speed, and linear Year, Bed, and Vessel
mod_7 <- update(mod_5.1, ~. + s(haul_speed, k = 3))
summary(mod_7)
plot(mod_7, select = 10, shade = T, residuals = T, cex = 0.5, pch = 1)
### refit model without slow haul speed outliers
mod_7.1 <- update(mod_7, subset = haul_speed >= 4)
summary(mod_7.1)
plot(mod_7.1, select = 10, shade = T, residuals = T, cex = 0.5, pch = 1)
gam.check(mod_7.1)

## Model with smoothed depth, wave ht, wind sp, haul speed, and tensor product of lat and lon,
## and linear Year, Bed, and Vessel
mod_8 <- update(mod_7.1, ~. + te(set_lon, set_lat, by = Bed)) 
summary(mod_8)
plot(mod_8, select = 17, shade = T, residuals = T, cex = 0.5, pch = 1)
f_resid_plots(mod_8)

## The existing model in use
mod_9 <- bam(rw_cpue + adj ~ s(depth, k=4, by = Bed) + te(set_lon, set_lat, by = Bed) + Year + 
              Bed + Vessel, data = scallops_comb, gamma=1.4, family=Gamma(link=log))
summary(mod_9)

## Fit reduced model (Best inferred by diagnostic plots)
mod_10 <- bam(rw_cpue + adj ~ s(depth, k=4, by = Bed) + te(set_lon, set_lat, by = Bed) + s(wave_ht, k = 3) + Year * 
               Bed + Vessel, data = scallops_comb, gamma=1.4, family=Gamma(link=log))
summary(mod_10)
f_resid_plots(mod_10)
gam.check(mod_10)

## Add tidal_index to reduced model and see if it improves it
mod_11 <- update(mod_10, ~. + s(tidal_index))
summary(mod_11) # s(tidal_index) is insignificant
## ANOVA on the addition of tidal index
anova(mod_10, mod_11, test = "Chisq") # suggests significant reduction in deviance
gam.check(mod_11)

# model comparison ----

## compile models into a list
gam <- mget(ls(pattern = "mod_"))
tibble(gam) %>%
  mutate(name = names(gam)) -> model_comp

## select only models we wish to compare
## add estimaed df, GCV score, AIC, and delta AIC
model_comp %>%
  slice(1, 3, 6, 8, 10, 12, 13, 15, 16, 17, 4, 5) %>% 
  mutate(edf = map_dbl(gam, f_edf),
         gcv = map_dbl(gam, f_gcv),
         AIC = map_dbl(gam, AIC),
         delta_AIC = AIC - min(AIC)) 

## The reduced model without tidal_index (mod_10) seems to be the most appropriate, continue with more model checking






