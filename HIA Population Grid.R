#' =============================================================================
#' Project: American Lung Association HIA
#' Date created: September 26, 2017
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
#' This script generates the population density grid for each ZCTA in the area
#'     1) The ZCTAs are rasterized (resolution = 0.008 deg)
#'     2) Population density is assigned to ZCTA cells
#'     3) A spatial grid object with ZCTA ID and population denisty is saved
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

geo_data <- "C:/Users/semarten/Documents/Geodatabases"

#' -----------------------------------------------------------------------------
#' 1) Rasterize ZCTAs in Colorado
#' Assigns the ZCTA identifier to the population density grid
#' Used to generate population-weighted exposure metrics for each ZCTA and day 
#' -----------------------------------------------------------------------------

#' Read in the population density geotiff
pop_den <- raster(paste(geo_data, "/2015_CO_PopDensity.tif", sep=""))
summary(pop_den)
res(pop_den)

load("./Data/Spatial Data/co_zcta_latlong.RData")
zcta_p <- spTransform(co_zcta, CRS=proj4string(pop_den)) #' match CRS
zcta_p@data$ALAND10 <- NULL 

plot(pop_den)
plot(co_zcta, add=T)

#' add a numeric identifier
zcta_p$GEOID_NUM <- as.numeric(zcta_p$GEOID10)
link <- zcta_p@data[,c("GEOID10", "GEOID_NUM")]

#' rasterize ZCTA
ext <- extent(pop_den)
zcta_r <- raster(ext, res=res(pop_den))
crs(zcta_r) <- proj4string(pop_den)
zcta_r <- rasterize(zcta_p, zcta_r, field='GEOID_NUM')
summary(zcta_r)

plot(zcta_r,colNA="grey50")
plot(zcta_p, col=NA, border="blue", add=T)

#' Check out how well the rasterization worked in GIS
writeOGR(zcta_p, dsn = geo_data, layer = "ZCTA_poly",
         driver="ESRI Shapefile", overwrite_layer = T)
writeRaster(zcta_r, filename=paste(geo_data,"/ZCTA_ras.asc",sep=""),
            format="ascii", overwrite=TRUE)

#' Create a spatial grid data frame with both ZCTA ID and population denisty
den_grid <- as(pop_den, 'SpatialGridDataFrame')

zcta_grid <- as(zcta_r, 'SpatialGridDataFrame')
zcta_grid <- cbind(zcta_grid, den_grid)
names(zcta_grid@data) <- c("GEOID10", "pop_den")
summary(zcta_grid)

plot(zcta_grid)
plot(zcta_p, col=NA, border="blue", add=T)

save(zcta_grid, file="./Data/Spatial Data/ZCTA grid.RData")

