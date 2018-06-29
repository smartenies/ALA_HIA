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

source("0_HIA Header.R")

for (s in 1:length(pre)) {
  start <- Sys.time()
  
  #' Pool the CRs
  #' Only need to run once, but it doesn't hurt to keep it 
  source("2_HIA CR Pooling.R")
  
  #' summarize population estimates
  #' Again, only need to run once, but it doesn't change anything to run again
  source("2_Population Estimates.R")
  
  #' Compile the HIA databases
  #' Same as above
  source("2_HIA Databases.R")
  
  #' Assess exposures at the ZCTA level
  source("3_HIA Exp Assessment.R")
  
  #' Run the MC analysis
  source("4_HIA Monte Carlo.R")
  
  #' Monetization and Inequality
  source("5_HIA Inequality.R")
  
  runtime <- Sys.time() - start
  save(runtime, file=paste("./HIA Outputs/", pre[s], "_runtime.RData"))
}


