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

source("./Code/1a_HIA Header.R")

#' #' Get the ZCTAs in order
#' source("./Code/2_HIA Spatial Databases.R")
#' 
#' #' Pool the CRs
#' #' Only need to run once, but it doesn't hurt to keep it
#' source("./Code/2_HIA CR Pooling.R")
#' 
#' #' summarize population estimates
#' #' Again, only need to run once, but it doesn't change anything to run again
#' source("./Code/2_Population Estimates.R")
#' 
#' #' Compile the HIA databases
#' #' Same as above
#' source("./Code/2_HIA Databases.R")

#' Loop through each step in the HIA
start <- Sys.time()

#' for (s in 1:length(pre)) {
#'   #' Assess exposures at the ZCTA level
#'   print(s)
#'   source("./Code/3_HIA Exp Assessment.R")
#' }
#' 
#' gc()
#' 
#' for (s in 1:length(pre)) {
#'   #' Run the MC analysis
#'   print(s)
#'   source("./Code/4_HIA Monte Carlo.R")
#' }
#' 
#' gc()

for (s in 1:length(pre)) {
  #' Summary of Exposures, Health Benefits, and Inequality Analysis
  if(s %% 2 == 0) next
  source("./Code/5_HIA Summary.R")
}

runtime <- Sys.time() - start

