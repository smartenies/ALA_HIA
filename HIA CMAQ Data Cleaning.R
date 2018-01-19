#' =============================================================================
#' Project: American Lung Association HIA
#' Date created: January 19, 2018
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
#' This script summarized air pollution exposures at the ZCTA level
#' =============================================================================

library(foreign)
library(sp)
library(Hmisc)
library(gstat)
library(rgdal)
library(ggplot2)
library(ggmap)
library(scales)
library(ggsn)
library(raster)
library(rgeos)
library(maptools)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(gridExtra)
library(plyr)
library(stringr)
library(readxl)

#' =============================================================================
#' Load population and concentration grids
#' =============================================================================

load("./Data/Spatial Data/ZCTA grid.RData")

#' For now, using dummy CMAQ data-- will need to reconcile this code later
#' Going to use the population grid-- ultimately will want to take the CMAQ
#' data and make a similar grid object
#' WILL NEED TO MAKE SURE HTE CMAQ OUTPUT AND POPULATION DENSITY DATA MATCH
#' IF NOT-- WILL NEED TO ADJUST POP DENSITY GRID AND THEN REDO THE ZCTA GRID

cmaq <- as(zcta_grid, "SpatialPixelsDataFrame")
cmaq$rec_id <- seq(1:nrow(cmaq))

n <- nrow(cmaq) 
reps <- 30 #how many days do I want to simulate
m <- matrix(sample(5:15, n*reps, replace=TRUE), ncol=30)

cmaq@data <- cbind(cmaq@data, m)
cmaq$GEOID10 <- NULL
cmaq$pop_den <- NULL
rm(m, n, reps)

#' =============================================================================
#' Join cmaq and population density data
#' =============================================================================










