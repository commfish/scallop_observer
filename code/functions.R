# mapping ----

mode <- function(x) {
  uniqv <- unique(x)
  as.numeric(uniqv[which.max(tabulate(match(x, uniqv)))])
}

f_xmap <- function(x){
  # function to create a base background map - called and named xmap in other functions
  ggplot()+
    geom_polygon(data=ak, aes(long, lat, group=group), fill=8, color='black') +
    xlab(expression(paste(Longitude^o,~'W'))) +
    ylab(expression(paste(Latitude^o,~'W'))) + 
    coord_map(xlim=c(min(x$set_lon,na.rm=T) - 0.2, max(x$set_lon,na.rm=T) + 0.2),
              ylim=c(min(x$set_lat,na.rm=T) - 0.2, max(x$set_lat,na.rm=T) + 0.2)) +
    theme_sleek()
}

f_global_map <- function(x){
  x %>% 
    mutate(District = factor(District, levels = c('D16', 'YAK', 'EKI', 'WKI', 'KSH', 'KNE', 'KSW', 'C', 'UB', 'O', 'Q'))) -> x
  f_xmap(x) + geom_point(data=x, aes(set_lon, set_lat, color=District), alpha=.2) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) -> x
  x
  # ggsave(paste0('figs/', YEAR, '/global_map.png'), plot = x, height = 5, width=6.5, units='in')
}

f_global_map_year <- function(x){
  
  x %>% 
    mutate(District = factor(District, levels = c('D16', 'YAK', 'EKI', 'WKI', 'KSH', 'KNE', 'KSW', 'C', 'UB', 'O', 'Q'))) %>% 
    filter(year %in% c(max(year), max(year) - 1, max(year) - 2)) -> x
  f_xmap(x) + geom_point(data=x, aes(set_lon, set_lat, color=District), alpha=.2) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    facet_wrap(~FY, dir = 'v') + 
    theme_sleek() + theme(legend.position = 'top') -> x
  x
  # ggsave(paste0('figs/', YEAR, '/global_year_map.png'), plot = x, height = 8.5, width=6.5, units='in')
}

f_district_map <- function(data, breaks = 2){
  # map of all fishing areas from 2009 - current assessment
  # may need to remove some years?
  y = deparse(substitute(data))
  xmap <- f_xmap(data)
  
  if("bed" %in% colnames(data)){
    xmap + geom_point(data=data, aes(set_lon, set_lat, color=Bed), alpha=.2) +
      guides(colour = guide_legend(override.aes = list(alpha = 1))) +
      facet_wrap(~FY, dir = 'v') + 
      scale_x_continuous(breaks = seq(-170,-130, breaks)) +
      theme_sleek() + 
      theme(legend.position = 'top') -> x
  } else{
    xmap + geom_point(data=data, aes(set_lon, set_lat), alpha=.2) +
      guides(colour = guide_legend(override.aes = list(alpha = 1))) +
      facet_wrap(~FY, dir = 'v') + scale_x_continuous(breaks = seq(-170,-130, breaks)) -> x
  }
  x
  #ggsave(paste0('figs/', YEAR, '/district_', y, '_map.png'), plot = x, height = 8.5, width=6.5, units='in')
}