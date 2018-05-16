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
#' BEFORE RUNNING THIS CODE, SET THE ANALYSIS UP IN THE HEADER
#' -----------------------------------------------------------------------------

source("1_HIA Header.R", local=T)

#' Pool the CRs
source("2_HIA CR Pooling.R")

#' summarize population estimates
source("2_Population Estimates.R")

#' Compile the HIA databases
source("2_HIA Databases.R")

#' Assess exposures at the ZCTA level
source("3_HIA Exp Assessment.R", local=T)

#' Run the MC analysis
source("4_HIA Monte Carlo.R", local=T)

#' Monetization and Inequality
source("5_HIA Inequality.R", local=T)