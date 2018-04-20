#' Header script that needs to run between each script in the HIA
#' Loads all the packages and the inputs needed for each method

library(sp)
library(gstat)
library(rgdal)
library(raster)
library(spatialEco)
library(ggplot2)
library(ggmap)
library(rgeos)
library(maptools)
library(ggthemes)
library(ncdf4)
library(tidyverse)
library(Hmisc)
library(writexl)

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
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Designate the path, input, and output names for this run 
#' -----------------------------------------------------------------------------

#' Population weighting  and exposure assessment setup:
cmaq_scenario <- "southern_colorado.nc"
cmaq_background <- NA
pop_den_tif <- "2010-COloradoPopDensity.tif"

#' CMAQ start date:
start_date <- as.Date("01-01-2011", format="%m-%d-%Y")

#' Unique prefix for all the output files in this test
pre <- "jan_2011_test_"

#' How many days should be used to scale the modeled estimates?
d_per_y <- 365

#' how many iterations should be used in the MC?
mc_n <- 1000

#" Set seed 
sim_seed <- 1234

#' HIA inputs:
cr_file <- "./HIA Inputs/CR.txt"
pop_file <- "./HIA Inputs/population.txt"
rate_file <- "./HIA Inputs/rates.txt"

#' Outputs:
out_path <- "./HIA Outputs/"