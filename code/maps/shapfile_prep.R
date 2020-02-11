# notes ----
## prepare a shapefile for mapping in ggplot2
## Tyler Jackson
## tyler.jackson@alaska.gov
## source: https://github.com/tidyverse/ggplot2/wiki/plotting-polygon-shapefiles
## last updated 2020/2/7

# load ----
library(rgdal) # requires sp, will use proj.4 if installed
library(maptools)
library(tidyverse)


# functions ----

path <- "./data/maps/statewide_scallop_survey_grid"
layer <- "scalGrid2019_all_albers"

f_shp_prep <- function(path, layer){

shp <- readOGR(dsn = path, layer = layer)
shp@data$id <- rownames(shp@data)
shp.points <- fortify(shp, region="id")
join(shp.points, shp@data, by="id")

}

f_shp_prep(path, layer)

