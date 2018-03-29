#' -----------------------------------------------------------------------------
#' Project: American Lung Association HIA
#' Date created: March 5, 2018
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
#' This script generates a weighting matix for the HIA.
#' Much of this code is based on prior work by Ryan Gan 
#'     
#'     1) The SEDAC population density data (~1 km grid) are regridded to 
#'     match the CMAQ outputs (which are in WGS84 and are not on a regular grid)
#'     2) Population density is extracted at each CAMQ receptor
#'     3) The object is saved to be used in the exposure assessment code
#' -----------------------------------------------------------------------------

library(foreign)
library(sp)
library(spatialEco)
library(gstat)
library(rgdal)
library(raster)
library(ggplot2)
library(ggmap)
library(rgeos)
library(maptools)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(gridExtra)
library(plyr)
library(stringr)
library(ncdf4)

#' For ggplots
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=10),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

geo_data <- "T:/Rsch-MRS/ECHO/SEM Large Data/Spatial Data"
utm_13 <- "+init=epsg:26913"
mercator <- "+proj=merc +a=6370000.0 +b=6370000.0 +lat_ts=33 +lon_0=0"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Set up the analysis
#' Will be done in the "master" script later

cmaq_out <- "southern_colorado.nc"
pop_den_tif <- "2010-COloradoPopDensity.tif"
#' -----------------------------------------------------------------------------

#' -----------------------------------------------------------------------------
#' Read in the CMAQ data and get the coordinates
#' Ali has generated a netcdf file for one month--- will use to get the coords
#' -----------------------------------------------------------------------------

#' Make grid for CMAQ receptors
library(ncdf4)

#' Open the netcdf file
cmaq <- nc_open(paste("./Data/CMAQ Data/", cmaq_out, sep=""))
summary(cmaq)

#' Extract coordinates for the CMAQ receptors
#' Matrix dimensions are 19 x 30
cmaq_lon <- ncvar_get(cmaq, var="lon")
cmaq_lat <- ncvar_get(cmaq, var="lat")
dim(cmaq_lat)

nc_close(cmaq)

#' Create data frame with ID 
cmaq_df <- data.frame(lon = as.vector(cmaq_lon),
                      lat = as.vector(cmaq_lat))
cmaq_df$id <- seq(1:nrow(cmaq_df))

#' plot grid cell centroids 
#' The grid of points is irregular due to the geographic coordinate system used
ggplot(cmaq_df) +
  geom_point(aes(x=lon, y=lat)) +
  simple_theme
head(cmaq_df)

#' Create a spatial object from the CMAQ coordinates
cmaq_p <- cmaq_df
coordinates(cmaq_p) <- c("lon", "lat")
proj4string(cmaq_p) <- CRS(ll_wgs84)
summary(cmaq_p)
cmaq_p@data <- cmaq_df

cmaq_e <- extent(cmaq_p)
cmaq_e

#' -----------------------------------------------------------------------------
#' Read in the SEDAC population density grid
#' Going to use 2010 since we'll be modeling 2011 as the baseline year
#' 2015 might be too far out for comparability
#' Cropped the GEOTiff in Arcmap prior to reading into R
#' -----------------------------------------------------------------------------

#' Read in the population density geotiff
#' Resolution is 0.00833 deg (approximately 1 km)
#' Coordinates are in WGS84 (same as CMAQ)
pop_den_t <- raster(paste("./Data/SEDAC Data/", pop_den_tif, sep=""))
pop_den_t

plot(pop_den_t)

#' Trim the density raster using the CMAQ extent
pop_den_r_1k <- crop(pop_den_t, cmaq_e)
pop_den_r_1k

plot(pop_den_r_1k)
points(cmaq_p, pch=20, cex=0.5)

#' Convert the raster to points
pop_den_df <- as.data.frame(rasterToPoints(pop_den_r_1k))
names(pop_den_df) <- c("lon", "lat", "pop_denisty")
pop_den_p <- pop_den_df
coordinates(pop_den_p) <- c("lon", "lat")
proj4string(pop_den_p) <- proj4string(pop_den_r_1k)

plot(pop_den_p, pch=20, cex=0.5)

#' -----------------------------------------------------------------------------
#' Regrid the populaton density points to the WRF (CMAQ) grid
#' 1) create and empty raster
#'     -should have nrow and ncol to match the CMAQ grid points
#'     -This will have to be specified manually, since the CMAQ grid is 
#'     irregular and can't be rasterized in R
#' 2) Rasterize the SECAD points to this new empty grid
#' 3) When rasterizing, use the mean function
#'     - SEDAC resolution is smaller than CMAQ
#'     _ Averaging density should be OK since it's in persons per sq km
#' -----------------------------------------------------------------------------

#' Create an empty raster with the same dimensions and CRS as the CMAQ points
cmaq_r <- raster(cmaq_e, nrow=nrow(cmaq_lat), ncol=ncol(cmaq_lat))
crs(cmaq_r) <- crs(cmaq_p)
cmaq_r

#' Rasterize the SEDAC data to this empty grid using average of points in cell
#' Range for population density is much smaller when averaging to the ~4 km grid
pop_den_r_4k <- rasterize(pop_den_df[,1:2], cmaq_r, pop_den_df[,3], fun=mean)

pop_den_r_1k
pop_den_r_4k

plot(pop_den_r_4k)
points(cmaq_p, pch=20, cex=0.5)

#' Extract population density at each CMAQ receptor
cmaq_p <- extract(pop_den_r_4k, cmaq_p, sp=T)
names(cmaq_p)[ncol(cmaq_p)] <- "pop_density"

#' plot CMAQ population density
ggplot(as.data.frame(cmaq_p), aes(x=lon, y=lat, col = pop_density)) +
  geom_point() +
  simple_theme

#' -----------------------------------------------------------------------------
#' Save the CMAQ-population density weights
#' -----------------------------------------------------------------------------

cmaq_p_df <- as.data.frame(cmaq_p)
save(cmaq_p, cmaq_p_df, pop_den_r_4k, pop_den_r_1k,
     file="./Data/CMAQ Data/cmaq_pop_density.RData")

#' -----------------------------------------------------------------------------
#' Get ZCTAs that are covered by the CAMQ grid
#' -----------------------------------------------------------------------------

#' Read in the ZCTA shapefile
co_zcta <- readOGR(dsn = geo_data, layer = "CO_ZCTA_2014")
co_zcta <- spTransform(co_zcta, CRS=proj4string(cmaq_p)) #' match CRS

plot(co_zcta)
points(cmaq_p, col="red", pch=20, cex=0.5)

#' Subset ZCTA that are completely within the CMAQ domain
cmaq_bound <- gConvexHull(cmaq_p)
co_zcta$contains <- gContains(cmaq_bound, co_zcta, byid = T)

plot(co_zcta)
plot(cmaq_bound, add=T)

#' All ZCTAs with at least one point
zcta <- co_zcta[cmaq_p,]

plot(zcta)
points(cmaq_p, col="red", pch=20, cex=0.5)

#' ZCTAs that are completely within the CMAQ domain
zcta_within <- co_zcta[which(co_zcta$contains == T),]

plot(zcta_within)
points(cmaq_p, col="red", pch=20, cex=0.5)

#' Save the ZCTA objects
save(co_zcta, zcta, zcta_within,
     file="./Data/Spatial Data/zip codes.RData")

#' Does every ZCTA contain a CMAQ receptor?
rec_count <- colSums(gContains(zcta, cmaq_p, byid=T))
rec_count <- setNames(rec_count, zcta@data$GEOID10)
rec_count

#' How many ZCTAs are there?
nrow(zcta)

#' How many with at least one receptor?
length(rec_count[rec_count > 0])

#' How many with just one?
length(rec_count[rec_count == 1])




