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
#' -----------------------------------------------------------------------------

#' -----------------------------------------------------------------------------
#' Designate the path, input, and output names for this run 
#' -----------------------------------------------------------------------------

#' Population weighting  and exposure assessment setup:
cmaq_scenario <- "southern_colorado.nc"
cmaq_baseline <- NA
pop_den_tif <- "2010-COloradoPopDensity.tif"

#' CMAQ start date:
start_date <- as.Date("01-01-2011", format="%m-%d-%Y")

#' Unique prefix for all the output files in this test
pre <- "jan_2011_test_"

#' How many days should be used to scale the modeled estimates?
d_per_y <- 365

#' HIA inputs:
cr_file <- "./HIA Inputs/CR.txt"
pop_file <- "./HIA Inputs/population.txt"
rate_file <- "./HIA Inputs/rates.txt"

#' Outputs:
out_path <- "./HIA Outputs/"

#' -----------------------------------------------------------------------------
#' Run Analysis
#' -----------------------------------------------------------------------------

#' Pool the CRs
source("0_HIA CR Pooling.R")
rm(list=ls())

#' Compile the HIA databases
source("0_Population Estimates.R")
rm(list=ls())

source("0_HIA Databases.R")
rm(list=ls())

#' Assess exposures at the ZCTA level
source("2_HIA Header.R", local=T)
source("3_HIA Exp Assessment.R", local=T)
rm(list=ls())

source("2_HIA Header.R", local=T)
source("4_HIA Monte Carlo.R", local=T)
rm(list=ls())