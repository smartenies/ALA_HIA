#' =============================================================================
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
#' This script sets up the HIA
#' 
#' Before running this script:
#'     1) Run the "HIA CR Pooling" script
#'     2) Run the "HIA Databases" script
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

#' =============================================================================
#' Designate the file and path names for this run 
#' =============================================================================

#' Population weighting  and exposure assessment setup
#' cmaq_out <- "southern_colorado.nc"
#' pop_den_tif <- "2010-COloradoPopDensity.tif"



#' -----------------------------------------------------------------------------
#' Health and Population Data:
inp_path <- "./HIA Inputs/"
cr_file <- "CR.txt"
pop_file <- "populations.txt"
rate_rile <- "rates.txt"
#' -----------------------------------------------------------------------------


#' -----------------------------------------------------------------------------
#' Outputs:
out_path <- "./HIA Outputs/"

#' -----------------------------------------------------------------------------


