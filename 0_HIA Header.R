#' Header script that needs to run between each script in the HIA
#' Loads all the packages and the inputs needed for each method

library(sp)
library(gstat)
library(rgdal)
library(raster)
library(ggplot2)
library(rgeos)
library(maptools)
library(ggthemes)
library(ncdf4)
library(Hmisc)
library(tidyverse)
library(readxl)
library(writexl)
library(readxl)
library(sf)
library(viridis)
library(DescTools)
library(IC2)
library(metafor)

#detach("package:plyr", unload=TRUE)

#' For ggplots
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color="transparent"),
  panel.grid.major = element_line(colour="transparent"),
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
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' -----------------------------------------------------------------------------
#' Designate the path, input, and output names for this run 
#' -----------------------------------------------------------------------------

#' Unique prefixes for each run
pre <- c("Summer_Full_Shutdown_")

#' Exposure assessment setup:
#' Order should match list of prefixes above
#'     "Baseline" is the baseline case, or starting point
#'     "Scenario" gets substracted from the baseline case
#'     If just doing total burden, set cmaq_scenario to NA
cmaq_baseline <- c("southern_colorado_2011196_2011230.nc")
cmaq_scenario <- c("southern_colorado_2035196_2035230.nc")

#' CMAQ start dates
#' Order should match list of prefixes above
start_dates <- c(as.Date("07-15-2011", format="%m-%d-%Y"))

#' CMAQ end dates
#' Order should match list of prefixes above
end_dates <- c(as.Date("08-18-2011", format="%m-%d-%Y"))

#' Which year should we use for the population density? 2010 or 2015
pop_den_tif <- "2010-COloradoPopDensity.tif"
  
#' exposure scale factor to avoid errors in kriging
#' The variogram fitting step doesn't do well with small numbers
#' due to truncation issues
exp_scale <- 10**6

#' Population for mapping rates
rate_pop <- 10000

#' How many days should be used to scale the modeled estimates?
#' For seasonal estimate, use 365/2
d_per_y <- 365/2

#' how many iterations should be used in the MC?
mc_n <- 1000

#" Set seed 
sim_seed <- 1234

#' HIA inputs:
cr_file <- "./HIA Inputs/CR.txt"
pop_file <- "./HIA Inputs/population.txt"
rate_file <- "./HIA Inputs/rates.txt"
ses_file <- "./HIA Inputs/ses indicators.txt"

#' Outputs:
out_path <- "./HIA Outputs/"