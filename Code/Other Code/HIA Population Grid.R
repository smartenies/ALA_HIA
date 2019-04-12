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
#'     1) The ZCTAs are rasterized (resolution = 0.00833 deg)
#'     2) Population density is assigned to ZCTA cells
#'     3) A spatial grid object with ZCTA ID and population denisty is saved
#'     4) The CMAQ grid cell centroids are extracted from the CMAQ netcdf 
#'     
#' =============================================================================

library(foreign)
library(sp)
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
#' 1) Rasterize ZCTAs in Colorado
#' Assigns the ZCTA identifier to the population density grid
#' Used to generate population-weighted exposure metrics for each ZCTA and day 
#' -----------------------------------------------------------------------------

#' Read in the population density geotiff
pop_den <- raster("./Data/SEDAC Data/2010-ColoradoPopDensity.tif")
summary(pop_den)
res(pop_den)

crs(pop_den)

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

plot(zcta_r,colNA="black")
plot(zcta_p, col=NA, border="blue", add=T)

#' Check out how well the rasterization worked in GIS
sedac_data <- "./Data/SEDAC Data"
writeOGR(zcta_p, dsn = sedac_data, layer = "ZCTA_poly",
         driver="ESRI Shapefile", overwrite_layer = T)
writeRaster(zcta_r, filename=paste(sedac_data,"/ZCTA_ras.tif",sep=""),
            format="GTiff", overwrite=TRUE)

#' Create a spatial grid data frame with both ZCTA ID and population denisty
den_grid <- as(pop_den, 'SpatialGridDataFrame')

zcta_grid <- as(zcta_r, 'SpatialGridDataFrame')
zcta_grid <- cbind(zcta_grid, den_grid)
names(zcta_grid@data) <- c("GEOID10", "pop_den")
summary(zcta_grid)

zcta_pts <- as(zcta_grid, "SpatialPointsDataFrame")
summary(zcta_pts)

plot(zcta_grid)
plot(zcta_p, col=NA, border="blue", add=T)

save(zcta_grid, zcta_pts, file="./Data/SEDAC Data/ZCTA grid.RData")


#' Make grid for CMAQ receptors
library(ncdf4)

#' Open the netcdf file
cmaq <- nc_open("./Data/CMAQ Data/southern_colorado.nc")
summary(cmaq)

#' Extract coordinates
cmaq_lon <- ncvar_get(cmaq, var="lon")
cmaq_lat <- ncvar_get(cmaq, var="lat")

cmaq_coords <- data.frame(lon = as.vector(cmaq_lon),
                          lat = as.vector(cmaq_lat))

#' plot grid cell centroids
ggplot(cmaq_coordinates, aes(x=lon, y=lat)) +
  simple_theme



cmaq <- raster("./Data/CMAQ Data/southern_colorado.nc")
cmaq
extent(cmaq)







cmaq_o <- ncvar_get(cmaq, varid="ozone")
cmaq_p <- ncvar_get(cmaq, varid="pm")



cmaq <- raster("./Data/CMAQ Data/southern_colorado.nc")
cmaq
summary(cmaq)
res(cmaq)

plot(cmaq)

