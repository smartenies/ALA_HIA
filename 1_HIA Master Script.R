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
#' 
#' Before running this script, make sure you set up the analysis in the "Header"
#' script
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
source("0_HIA Header.R", local=T)
source("2_HIA Exp Assessment.R", local=T)
rm(list=ls())

source("0_HIA Header.R", local=T)
source("3_HIA Monte Carlo.R", local=T)
rm(list=ls())