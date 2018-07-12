#' -----------------------------------------------------------------------------
#' Project: American Lung Association HIA
#' Date created: January 23, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' 
#' This project estimates the health impacts attributable to two coal-fired 
#' power plants in the front range region of CO: Comanche (in Pueblo, CO) and 
#' Martin Drake (in Colorado Springs, CO). The facilities are slated to be 
#' decommissioned by 2025.
#' 
#' This script makes a map of the study area
#' -----------------------------------------------------------------------------


library(ggplot2)
library(ggmap)
library(tmap)
library(rgeos)
library(ggthemes)
library(tidyverse)
library(sf)
library(sp)
library(rgeos)
library(viridis)


#' For ggplots
simple_theme <- theme(
    text  = element_text(family="Calibri",size = 15, color = 'black'),
    panel.spacing.y = unit(0,"cm"),
    panel.spacing.x = unit(0.25, "lines"),
    panel.grid.minor = element_line(color="transparent"),
    panel.grid.major = element_line(colour="transparent"),
    panel.border=element_rect(fill = NA),
    panel.background=element_blank(),
    axis.ticks = element_line(colour = "black"),
    axis.text = element_text(color = "black", size=10),
    #legend.position = c(0.1,0.1),
    plot.margin=grid::unit(c(0,0,0,0), "mm"),
    legend.key = element_blank()
    )
 
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

geo_data <- "T:/Rsch-MRS/ECHO/SEM Large Data/Spatial Data"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' -----------------------------------------------------------------------------
#' Create shapefiles for the area map in Arcmap
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/power_plants.RData")
pp_sf <- st_transform(pp, ll_wgs84)
head(pp_sf)

st_write(pp_sf, "./Data/Spatial Data/power_plants.shp", delete_dsn = T)

#' Get list of ZCTAs from the Winter Baseline case
load("./HIA Inputs/HIA_Winter_zcta.RData")
zcta_sf <- st_as_sf(zcta)
head(zcta_sf)

st_write(zcta_sf, "./Data/Spatial Data/zctas_for_hia.shp", delete_dsn = T)

zcta_union <- st_union(zcta_sf)
head(zcta_union)
plot(st_geometry(zcta_union))

st_write(zcta_union, "./Data/Spatial Data/zcta_bound_for_hia.shp", delete_dsn = T)

#' Get receptor points from the Winter Baseline case
load("./HIA Inputs/HIA_Winter_cmaq_spatial.RData")
cmaq_sf <- st_as_sf(cmaq_p) %>% 
  st_transform(crs = ll_wgs84)

st_write(cmaq_sf, "./Data/Spatial Data/cmaq_pts_for_hia.shp", delete_dsn = T)
