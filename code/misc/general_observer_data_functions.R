# notes ----
## general functions applicable to all observer data analysis
## Tyler Jackson
## tyler.jackson@alaska.gov
## last updated: 2020/2/27

# load libraries ----
library(tidyverse)

# functions ----

# rename catch data to appropriate style
# args:
## x - catch data (as downloaded directly from wiki)
f_catch_rename <- function(x){
  names(x) <- c("Fishery", "District", "ADFG", "Trip_ID", "Haul_ID", 
                "haul", "gear_perf", "haul_sampled", "Set_date", "set_lat",
                "set_lon", "statarea", "depth", "dredge_count", 
                "dredge_width", "dredge_down", "dredge_up", "duration", 
                "dredge_hrs", "haul_speed", "distance_nm", "area_swept", 
                "rtnd_basket", "round_weight", "meat_weight", "est_yield",
                "tot_rtnd_basket", "tot_day_meat_weight")
  x
}

# add season to data (based on Fishery field)
# args:
## x - tibble of observer or logbook data
## fishery_col - name of column that denotes fishery. Default = "Fishery"
f_add_season <- function(x, fishery_col = "Fishery"){
  x %>%
    pull(grep(fishery_col, names(.))) %>%
    str_sub(., 3, 4) %>%
    as.numeric() %>%
    tibble(Season = .) %>%
    mutate(Season = ifelse(Season < 90, Season + 2000, Season + 1900),
           Season = factor(paste0(Season, "/", substring(Season + 1, 3, 4)))) %>%
    bind_cols(x)
}


# quick summary of fishery statistics
# args:
## x - catch data (as downloaded directly from wiki)
## district - abbrev. for any districts to summarise over
## ghl - logical include cmobine ghl. Default = F
## path - optional. Writes .csv file to path provided
f_fish_stats <- function(x, district, add_ghl = F, path){
  # add Season if it is not found
  if(!("Season" %in% names(x))) {x <- add_season(x)}
  # summarize catch data
  x %>%
    # filter to area D
    filter(District %in% district) %>%
    # summarise data 
    group_by(Season) %>%
    summarise(mt_wt = sum(meat_weight, na.rm = T),
              rnd_wt = sum(round_weight, na.rm = T),
              dredge_hrs = sum(dredge_hrs, na.rm = T),
              number_hauls = n(),
              mw_cpue = mt_wt / dredge_hrs,
              rw_cpue = rnd_wt / dredge_hrs) -> tmp
  # add ghl if necessary
  if(add_ghl == T){
    if(!exists("ghl")){stop("tibble named 'ghl' not found")}
    else{
    ghl %>%
      filter(District %in% district) %>%
      group_by(Season) %>%
      summarise(ghl = sum(ghl, na.rm = T)) %>%
      right_join(tmp, by = "Season") -> tmp
    }
  }
  # write to a csv if necessary
  if(missing(path)) {tmp}
  else{
    write_csv(tmp, path)
    tmp
  }
}

# 2D density polygon dataframe for ggplot2 map, no weighting
## args:
## data: tibble contain plotting data
## x: name of column denoting longitude
## y: name of column denoting latitude
## h: 2D bandwidth. Default = c(0.1, 0.1).
## facet: Optional. Will facet wrap by variable provided.
f_density_2D <- function(data, x, y, h = c(0.1, 0.1), facet){
  if(!missing(facet)){
  ### create facet labels for ggplot_build workaround of 2D density plot issue
  tibble(var = unique(pull(data, facet)),
         PANEL = factor(1:length(unique(pull(data, facet))))) -> pan_join
  
  data %>%
    rename(long = x,
           lat = y, 
           wrap = facet) %>%
  # create map layers to pull data from
    ggplot()+
    stat_density_2d(aes(x = long, y = lat, fill = stat(level)), geom = "polygon", 
                    h = h)+
    scale_fill_gradientn(colors = topo.colors(10))+
    facet_wrap(~wrap) -> density
  # pull data and join facet labels
  ggplot_build(density)$data[[1]] %>%
    left_join(pan_join, by = "PANEL") -> x
  names(x)[ncol(x)] <- facet
  as_tibble(x)
  }
  else{
    data %>%
      rename(long = x,
             lat = y) %>%
      # create map layers to pull data from
      ggplot()+
      stat_density_2d(aes(x = long, y = lat, fill = stat(level)), geom = "polygon", 
                      h = h)+
      scale_fill_gradientn(colors = topo.colors(10)) -> density
    # pull data and join facet labels
    ggplot_build(density)$data[[1]] 
  }
}

## graphical extent of roundweight catch (mean distance between dredges in graphical units)
### args:
### x - logbook catch data
### quant - cut off quantile for contribution to catch. Default = 0.95.
f_extent_catch <- function(x, quant = 0.9){
  x %>%
    arrange(-round_weight) %>%
    mutate(cum_prop = cumsum(round_weight) / sum(round_weight)) %>%
    filter(cum_prop <= quant) %>%
    select(set_lon, set_lat) %>%
    dist() %>%
    mean()
}


# objects ----

## base map
## high resolution map of alaska, canada
usa <- raster::getData("GADM", country = c("USA"), level = 1, path = "./data/maps")
can <- raster::getData("GADM", country = c("CAN"), level = 1, path = "./data/maps")
bind_rows(fortify(usa), fortify(can)) %>%
  filter(long > -168, long < -129, lat > 53, lat < 62) -> canam
ggplot()+
  geom_polygon(data = canam, aes(x = long, y = lat, group = group), 
               color = NA, fill = "grey90")+
  labs(x = expression(paste(Longitude^o,~'W')), 
       y = expression(paste(Latitude^o,~'N')))+
  theme(panel.background = element_rect(fill = "grey70")) -> f_base_map

## ditrict specific coordinate projections
KNE_proj <- coord_quickmap(xlim = c(-153.1, -150), ylim = c(56.5, 58.7))
KSH_proj <- coord_quickmap(xlim = c(-155, -152.4), ylim = c(57, 59))
KSW_proj <- coord_quickmap(xlim = c(-156.4, -154.3), ylim = c(56, 58))
KSE_proj <- coord_quickmap(xlim = c(-155.6, -152.3), ylim = c(55.5, 57.5))



        