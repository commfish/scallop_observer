# notes ----
# SAFE report 
# Fishery Data Functions
# author: Tyler Jackson (some functions by Ben Williams)
# contact: tyler.jackson@alaska.gov

# load ----
library(tidyverse)
library(latticeExtra)
library(lubridate)
library(mgcv)
library(tibbletime)
library(cowplot)
library(FNGr); theme_set(theme_sleek())

# functions ----

## catch statistics
f_fishery_stats <- function(catch, district, old_data_table = F){
  
  # manipulate data
  catch %>%
    filter(District == district) %>%
    mutate(set_date = mdy(Set_date),
           month = month(set_date),
           year = year(set_date),
           Season = ifelse(month >= 7, 
                           paste0(year,"/", substring(year + 1, 3, 4)), 
                           paste0(year - 1,"/", substring(year, 3, 4)))) %>%
    group_by(Season, Fishery) %>%
    summarise(n_vessels = length(unique(ADFG)),
              mt_wt = sum(meat_weight, na.rm = T),
              rnd_wt = sum(round_weight, na.rm = T),
              dredge_hrs = sum(dredge_hrs, na.rm = T),
              mt_cpue = mt_wt / dredge_hrs,
              rnd_cpue = rnd_wt / dredge_hrs) %>%
    ungroup()-> dat
  
  # combine with old data table (if necessary)
  if(old_data_table == T){
    old_catch %>%
      filter(District == district) %>%
      select(-District) %>%
      bind_rows(dat) -> dat
  }
  
  # add empty rows for years with no fishing
  tibble(yrs = min(as.numeric(substring(dat$Season,1, 4))):max(as.numeric(substring(dat$Season,1, 4)))) %>%
    mutate(yrs_plus1 = as.character(yrs + 1),
           Seasons = paste0(yrs, "/", substring(yrs_plus1, 3, 4))) %>%
    filter(!(Seasons %in% dat$Season)) %>%
    pull(Seasons) -> missing_seasons

  if(length(missing_seasons) > 0){
    dat %>%
      add_row(Season = missing_seasons) %>%
      mutate(year = as.numeric(substring(Season, 1, 4))) %>%
      arrange(year) -> dat
  }
  
  # generate plot using latticeExtra package
  bar <- barchart(mt_wt~Season, dat, col = "grey", scales = list(x = list(rot=90)), 
                  ylab = "Harvest (lb scallop meat) \n")
  pts <- xyplot(mt_cpue~c(1:nrow(dat)), dat, col = "black", type = "o", pch = 8, 
                ylab = "CPUE (lb scallop meat / dredge hr)", ylim = c(0, 1.1*max(dat$mt_cpue, na.rm = T)),
                scales = list(y = list(at = seq(0, 1000, 15))))
  plot <- doubleYScale(bar, pts, add.ylab2 = TRUE, use.style = F)
  
  # save plot  
  png(paste0("./figures/safe/", YEAR, "/fishery_statistic_", district, ".png"), 
      width = 5, height = 4, units = "in", res = 300)
  print(plot)
  dev.off()
  
  
  #save table
  write.csv(dat, paste0('./output/safe/', YEAR, '/fishery_statistics_', district, '.csv'), row.names = F)
}

## cpue standarization meat weight
f_gam_mw <- function(data){
  
  adj <- mean(data$mw_cpue, na.rm = T) * 0.10
  
  if('Bed' %in% colnames(data)){
    
    fit <- bam(mw_cpue + adj ~ s(depth, k=4, by = Bed) + 
                 te(set_lon, set_lat, by = Bed) + Year +
                 Bed + s(Vessel, bs='re', by=dum), 
               data=data, gamma=1.4, family=Gamma(link=log))
  } else {
    
    fit <- bam(mw_cpue + adj ~ s(depth, k=4) + 
                 te(set_lon, set_lat) + Year + 
                 s(Vessel, bs='re', by=dum), 
               data=data, gamma=1.4, family=Gamma(link=log))
  }
  return(fit)
}

## cpue standarization round weight
f_gam_rw <- function(data){
  
  adj <- mean(data$rw_cpue, na.rm = T) * 0.10
  
  if('Bed' %in% colnames(data)){
    fit <- bam(rw_cpue + adj ~ s(depth, k=4, by = Bed) + 
                 te(set_lon, set_lat, by = Bed) + Year + 
                 Bed + Vessel, data=data, gamma=1.4, family=Gamma(link=log))
  } else {
    fit <- bam(rw_cpue + adj ~ s(depth, k=4) + 
                 te(set_lon, set_lat) + Year + 
                 Vessel, data=data, gamma=1.4, family=Gamma(link=log))
  }
  return(fit)
}

## raw and standarized cpue
f_fishery_cpue <- function(x, district, weight_type){
  
  # manipulate data to get Season
  x %>%
    filter(District == district) %>%
    mutate(set_date = mdy(Set_date),
           month = month(set_date),
           year = year(set_date),
           Season = ifelse(month >= 7, 
                           paste0(year,"/", substring(year + 1, 3, 4)), 
                           paste0(year - 1,"/", substring(year, 3, 4)))) -> x
  
  # obtain summary stats on raw and standarized cpue
  if(weight_type %in% c("meat_weight", "meat")){
    ## raw cpue stats
    x %>%
      group_by(Season) %>%
      summarise(raw_cpue_median = round(median(meat_weight / dredge_hrs, na.rm = T), 1),
                raw_cpue_mean = round(mean(meat_weight / dredge_hrs, na.rm = T), 1),
                raw_cpue_sd = round(sd(meat_weight / dredge_hrs, na.rm = T), 1)) -> x_raw
    ## standardized cpue
    x %>%
      mutate(Year = factor(substring(Season, 1, 4)),
             mw_cpue = meat_weight / dredge_hrs, 
             Vessel = factor(ADFG),
             dum = 1) %>%
      filter(as.numeric(as.character(Year)) >= 2009) %>% # convert Year to numeric to preserve season (avoid misclassifying season on date)
      select(names(.)[names(.) %in% c("Season","Year", "Bed", "Vessel", 
                                         "set_lat", "set_lon", "depth", "mw_cpue", "dum")]) %>% 
      .[complete.cases(.),] -> x_std
    
    fit <- f_gam_mw(x_std)
    
    x_std %>%
      mutate(fitted = fit$fitted.values) %>%
      group_by(Season) %>%
      mutate(adj = mean(mw_cpue, na.rm = T) * 0.1) %>%
      mutate(std_cpue = fitted - adj) %>%
      summarise(std_cpue = mean(std_cpue, na.rm = T)) %>%
      right_join(x_raw, by = "Season") %>%
      select(1, 3:5, 2) -> x_cpue
  }
  
  if(weight_type %in% c("round_weight", "round")){
    ## raw cpue stats
    x %>%
      group_by(Season) %>%
      summarise(raw_cpue_median = round(median(round_weight / dredge_hrs, na.rm = T), 1),
                raw_cpue_mean = round(mean(round_weight / dredge_hrs, na.rm = T), 1),
                raw_cpue_sd = round(sd(round_weight / dredge_hrs, na.rm = T), 1)) -> x_raw
    ## standardized cpue
    x %>%
      mutate(Year = factor(substring(Season, 1, 4)),
             rw_cpue = round_weight / dredge_hrs, 
             Vessel = factor(ADFG),
             dum = 1) %>%
      filter(as.numeric(as.character(Year)) >= 2009) %>% # convert Year to numeric to preserve season (avoid misclassifying season on date)
      select(names(.)[names(.) %in% c("Season","Year", "Bed", "Vessel", 
                                      "set_lat", "set_lon", "depth", "rw_cpue", "dum")]) %>% 
      .[complete.cases(.),] -> x_std
    
    fit <- f_gam_rw(x_std)
    
    x_std %>%
      mutate(fitted = fit$fitted.values) %>%
      group_by(Season) %>%
      mutate(adj = mean(rw_cpue, na.rm = T) * 0.1) %>%
      mutate(std_cpue = fitted - adj) %>%
      summarise(std_cpue = mean(std_cpue, na.rm = T))%>%
      right_join(x_raw, by = "Season") %>%
      select(1, 3:5, 2) -> x_cpue
    }
  
  # save cpue table
  write.csv(x_cpue, paste0("./output/safe/", YEAR, "/fishery_cpue_", weight_type, "_", district, ".csv"), 
            row.names = F)
  
  
  # create plots
  
  ## rollify the function mean()
  roll_mean <- rollify(mean, window = 5)
  
  ## raw plot
  x_cpue %>%
    mutate(raw_grand_mean = mean(raw_cpue_mean, na.rm = T),
           raw_roll_mean = roll_mean(raw_cpue_mean)) %>%
    select(Season, raw_cpue_mean, raw_grand_mean, raw_roll_mean) %>%
    pivot_longer(c(raw_cpue_mean, raw_roll_mean, raw_grand_mean), names_to = "Name") %>%
    mutate(Name = case_when(Name == "raw_cpue_mean" ~ "Raw CPUE",
                            Name == "raw_roll_mean" ~ "5-yr Rolling Mean", 
                            Name == "raw_grand_mean" ~ "Mean"),
           Name = factor(Name, levels = c("Raw CPUE", "Mean", "5-yr Rolling Mean"))) -> pdat 
    ggplot(pdat)+
    geom_line(aes(x = Season, y = value, group = Name, linetype = Name, color = Name))+
    geom_point(data = pdat[pdat$Name != "Mean",], aes(x = Season, y = value, color = Name), 
               show.legend = F)+
    scale_color_manual(values = c("forestgreen", "black", "black"))+
    labs(x = "Season", y = "Raw CPUE (lbs / dredge hr)", color = NULL, linetype = NULL)+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) -> p_raw
    
  ## standardized plot
  x_cpue %>%
    mutate(std_grand_mean = mean(std_cpue, na.rm = T),
           std_roll_mean = roll_mean(std_cpue)) %>%
    select(Season, std_cpue, std_grand_mean, std_roll_mean) %>%
    pivot_longer(c(std_cpue, std_grand_mean, std_roll_mean), names_to = "Name") %>%
    mutate(Name = case_when(Name == "std_cpue" ~ "Std. CPUE",
                            Name == "std_roll_mean" ~ "5-yr Rolling Mean", 
                            Name == "std_grand_mean" ~ "Mean"),
           Name = factor(Name, levels = c("Std. CPUE", "Mean", "5-yr Rolling Mean"))) -> pdat 
    ggplot(pdat)+
    geom_line(aes(x = Season, y = value, group = Name, linetype = Name, color = Name))+
    geom_point(data = pdat[pdat$Name != "Mean",], aes(x = Season, y = value, color = Name), 
               show.legend = F)+
    scale_color_manual(values = c("forestgreen", "black", "black"))+
    labs(x = "Season", y = "Standardized CPUE (lbs / dredge hr)", color = NULL, linetype = NULL)+
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) -> p_std
  
  ## combine and save plots
  png(paste0("./figures/safe/", YEAR, "/fishery_cpue_", weight_type, "_", district, ".png"), 
      width = 7, height = 8, units = "in", res = 300)
  print(plot_grid(p_raw, p_std, ncol = 1))
  dev.off()
}

## shell height composition
f_sh_comp <- function(sh, district){
  # manipulate data 
  sh %>%
    filter(District == district) %>%
    mutate(Season = as.numeric(substring(Fishery, 3, 4)),
           Season = ifelse(Season < 70, Season + 2000, Season + 1900),
           Season = factor(paste0(Season, "/", substring(Season + 1, 3, 4))),
           Rtnd_Disc = case_when(Rtnd_Disc == "D" ~ "Discard",
                                 Rtnd_Disc == "R" ~ "Retained")) -> sh
  
  # create a base plot
  ggplot(sh, aes(x = shell_height, fill = Rtnd_Disc, color = Rtnd_Disc)) -> plot
    
  # create a histogram
  plot+
    geom_histogram(binwidth = 3, alpha = 0.2)+
    scale_color_manual(values = c("#e41a1c", "#377eb8"))+
    labs(x = "Shell height (mm)", y = "Count")+
    facet_grid(rows = vars(Season))+
    theme(legend.position = "none") -> sh_hist
  
  # create a density plot
  plot+
    geom_density(alpha = 0.2)+
    scale_color_manual(values = c("#e41a1c", "#377eb8"))+
    labs(x = "Shell height (mm)", y = "Density", fill = NULL, color = NULL)+
    facet_grid(rows = vars(Season)) -> sh_dens
  
  # save the legend from the plot
  legend <- get_legend(sh_dens + theme(legend.position = "bottom"))
  
  # combine plots
  ggdraw()+
    draw_plot(sh_dens + theme(legend.position = "none"), x = 0, y = 0.075, width = 0.5, height = 0.9)+
    draw_plot(sh_hist, x = 0.5, y = 0.075, width = 0.5, height = 0.9)+
    draw_plot(legend, x = 0, y = 0, width = 1, height = 0.1) -> sh_plot
  
  #print and save
  png(paste0("./figures/safe/", YEAR, "/shell_height_", district, ".png"), 
      width = 6, height = 8, units = "in", res = 300)
  print(sh_plot)
  dev.off()
}