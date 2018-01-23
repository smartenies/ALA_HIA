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
#' Designate the file and path names for this run 
#' =============================================================================

#' -----------------------------------------------------------------------------
#' Exposures:
exp_path <- "./HIA Inputs/Exposures/"  #Don't change this
exp_file <- "Test CMAQ Data.RData"
#' -----------------------------------------------------------------------------

#' -----------------------------------------------------------------------------
#' Health and Population Data:
inp_path <- "./HIA Inputs/"
cr_file <- "CR.txt"
#' -----------------------------------------------------------------------------

#' -----------------------------------------------------------------------------
#' Outputs:

#' -----------------------------------------------------------------------------


