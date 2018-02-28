#' =============================================================================
#' Project: ECHO Aim 1 
#' Date created: September 11, 2017
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' 
#' This project examines the relationships between spatially-distributed
#' economic, environmental, and social variables and health outcomes meausred
#' in the Healthy Start cohort (UC Denver)
#' 
#' This script generates plots of daily and annual exposure metrics based on
#' CMAQ modeling
#' 
#' NOTE: don't forget the ./ before the directory when reading in files!
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

hia_plots <- "T:/Rsch-Magzamen/ALA_HIA/Plots/Daily Exposure Metrics/"
utm_13 <- "+init=epsg:26913"
lat_long <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' =============================================================================


pollutants <- c("PM2.5", "O3")
load("./HIA Inputs/Cities.RData")

for (city in 1:length(cities)) {
  city_name <- cities[[city]]
  load(paste("./HIA Inputs/", city_name, "_receptors.RData", sep=""))
  
  #' Set up the receptors
  recs_sp <- recs
  coordinates(recs_sp) <- c("longitude", "latitude")
  proj4string(recs_sp) <- CRS(lat_long)
  
  recs_r <- rasterize()

  for (pols in 1:length(pollutants)) {
    pol <- pollutants[pols]
    load(paste("./HIA Inputs/", city_name, "_", pol, "_metrics.RData", sep=""))
   
     
  }
}

 















