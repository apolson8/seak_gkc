
# notes ----
## prepare a shapefile for mapping in ggplot2
## Tyler Jackson
## tyler.jackson@alaska.gov
## sources: 
### https://github.com/tidyverse/ggplot2/wiki/plotting-polygon-shapefiles
### https://mgimond.github.io/Spatial/coordinate-systems-in-r.html
## last updated 2020/2/19

## source functions within a separate script
## see annotation below for arguments

# load ----
library(sp)
library(rgdal) # requires sp, will use proj.4 if installed
library(maptools)
library(tidyverse)
library(gpclib); gpclibPermit()

# functions ----

#path <- "./data/maps/statewide_scallop_survey_grid_2019"
#layer <- "scalGrid2019_all_albers"

## read shapefile into R to draw a polygon in ggplot2
### arugment path is the file path to the directory housing the shapefiles
### arugment layer is the name of shapfiles, minus the extension
f_shp_prep <- function(path, layer){
  
  shp <- readOGR(dsn = path, layer = layer)
  shp@data$id <- rownames(shp@data)
  shp.points <- fortify(shp, region = "id")
  plyr::join(shp.points, shp@data, by = "id")
  
}

## transfom AK Albers to NAD83 projection
### arugment x is output of f_shp_prep
### arguments lat and long are names of lat and long fields
f_albers_to_nad83 <- function(x, longitude = "long", latitude = "lat"){
  # isolate lat and lon
  x %>%
    dplyr::select(longitude, latitude) -> xy
  # set up CRS string for transformation
  crs <- CRS("+proj=aea +lat_1=55.0 +lat_2=65.0 +lat_0=50.0 +lon_0=-154.0 +datum=NAD83")
  # complete the transformation
  g <- spTransform(SpatialPoints(xy, proj4string=crs), 
                   CRS("+proj=longlat +datum=NAD83"))
  # replace coords
  x[, c(longitude, latitude)] <- coordinates(g)
  # print!
  as_tibble(x)
}