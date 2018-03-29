#' =============================================================================
#' Project: American Lung Association HIA
#' Date created: March 29, 2018
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
#' This script calculates the attributable health impacts for each pollutant,
#' exposure metric, outcome, and ZIP code using a Monte Carlo analysis (based
#' on Ryan Gan's ozone HIA)
#' =============================================================================

#' load the HIA databases
cr <- read.table(cr_file, header=T)
pop <- read.table(pop_file, header = T) 
rate <- read.table(rate_file, header=T)

#' Loop through pollutants, metrics, and days to estimate attibutable health
#' impacts, scaled to a full year
pol_names <- c("pm", "o3")

for (i in 1:length(pol_names)) {
  
  #' load ZCTA exposures
  load(paste("./HIA Inputs/", pre, pol_names[i], "_zcta_metrics.RData",sep=""))
  
  metrics <- names(zcta_list)
  
  
}
