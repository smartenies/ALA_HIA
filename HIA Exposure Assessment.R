#' =============================================================================
#' Project: American Lung Association HIA
#' Date created: January 19, 2018
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
#' This script summarizes air pollution exposures at the ZCTA level
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
#' Cleaning CMAQ data and summarizing to the metrics we want for the HIA:
#'     Annual average
#'     Daily average
#'     Daily 1 hour max
#'     Daily 8 hour max
#' =============================================================================

#' -----------------------------------------------------------------------------
#' Designate the file name for this run (TO BE MOVED TO THE MASTER SCRIPT)
exp_path <- "./HIA Inputs/Exposures/"
exp_file <- "Test CMAQ Data.RData"
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/ZCTA grid.RData")

#' For now, using dummy CMAQ data-- will need to reconcile this code later
#' Making a raster with the same spatial resolution as the population density
#' grid for now
#' WILL NEED TO MAKE SURE HTE CMAQ OUTPUT AND POPULATION DENSITY DATA MATCH
#' IF NOT-- WILL NEED TO ADJUST POP DENSITY GRID AND THEN REDO THE ZCTA GRID

n <- nrow(zcta_grid) 
reps <- 24*5 #how many hours do I want to simulate
cmaq_pm2.5 <- matrix(sample(5:15, n*reps, replace=TRUE), ncol=reps)
cmaq_o3 <- matrix(sample(50:80, n*reps, replace=TRUE), ncol=reps)

rm(n, reps, zcta_grid, zcta_pts)

#' Check dimentions of the CMAQ matrix
dim(cmaq_pm2.5)
dim(cmaq_o3)
#' 405002 receptors and 48 hours

#' all of the cmaq outputs we want to calculate metrics for need to be in
#' a list
cmaq_list <- list(cmaq_pm2.5, cmaq_o3)
names(cmaq_list) <- c("PM2.5", "O3")

#rm(cmaq_pm2.5, cmaq_o3)

#' =============================================================================
#' Functions for summarizing the receptor concentrations
#' =============================================================================

#' function for daily metrics
daily_exp <- function(mat, hrs = 24, func) {
   try(if(ncol(mat)%%hrs != 0) stop("Error: number of columns in matrix not divisible by hrs"))
   days <- ncol(mat) / hrs
  
   f_mat <- matrix(nrow=(nrow(mat)))
   
   for (day in 1:days) {
     d_mat <- mat[,((day*hrs)-(hrs-1)):((day*hrs)-(hrs-1)+(hrs)-1)]
     temp <- as.matrix(apply(d_mat, 1, func))
     f_mat <- cbind(f_mat, temp) 
   }
   return(f_mat[,-1])
}

#' function for 8 h daily max (Based on the ozone NAAQS)
#' d8h_max is highest 8 h mean in the 24 h period)
d8h_exp <- function(mat, hrs = 24) {
  try(if(ncol(mat)%%hrs != 0) stop("Error: number of columns in matrix not divisible by hrs"))
  days <- ncol(mat) / hrs
  
  f_mat <- matrix(nrow=(nrow(mat)), ncol=days)
  
  for (day in 1:(days-1)) {
    d_mat <- mat[,((day*hrs)-(hrs-1)):((day*hrs)-(hrs-1)+(hrs*2)-1)]
    
    for (rec in 1:nrow(d_mat)) {
      o_mat <- matrix(d_mat[rec,], nrow=1)
      
      avg_list <- list()
      a <- 16 #should have 17 moving averages starting at 7:00am and ending at 11:00pm
      
      for (k in 7:(7+a)) {
        hours <- seq(from=k, to=k+7)
        conc<- o_mat[,hours]
        #' need at least 6 hourly concentrations to calculate mean
        avg_list[k-6] <- ifelse(sum(!is.na(conc)) >= 6, mean(conc, na.rm=T), NA)
      }
      
      b <- unlist(avg_list)
      
      #' only valid if there are >= 13 8-hr means
      max <- ifelse(sum(!is.na(b)) >= 13, max(b, na.rm=T), NA)
      
      f_mat[rec, day] <- max
    }
  }
  return(f_mat)
}

#' Testing the functions
# test_m <- matrix(c(rep(1, times=48), rep(2, times=48)), ncol=24*4)
# test_m <- rbind(test_m, test_m, test_m)
# daily_mean <- daily_exp(mat=test_m, hrs=24, fun = mean)
# daily_max <- daily_exp(mat=test_m, hrs=24, fun = mean)
# daily_8h <- d8h_exp(mat=test_m, hrs=24)

#' =============================================================================
#' Summarize the hourly data into daily and annual metrics
#' =============================================================================

#' Calculate annual and daily metrics for each receptor
for (i in 1:length(cmaq_list)) {
  # ID the first matrix
  cmaq <- cmaq_list[[i]]
  name <- names(cmaq_list)[i]
  
  #' annual mean
  print(paste(name, ": annual mean"))
  ann_mean <- as.matrix(rowMeans(cmaq))
  
  #' daily means
  print(paste(name, ": daily mean"))
  d24h_mean <- daily_exp(mat = cmaq, hrs = 24, func = mean)
  
  #' daily 1-hour max
  print(paste(name, ": daily 1-hour max"))
  d1h_max <- daily_exp(mat = cmaq, hrs = 24, func = max)
  
  #' daily 8-hour max (O3; not used in PM2.5 HIFs)
  print(paste(name, ": daily 8-hour max"))
  d8h_max <- d8h_exp(mat = cmaq, hrs = 24)
  
  exp_list <- list(ann_mean, d24h_mean, d1h_max, d8h_max)
  names(exp_list) <- paste(name, 
                           c("ann_mean", "d24h_mean", "d1h_max", "d8h_max"),
                           sep="_")
  save(exp_list, file=paste(exp_path, name, " ", exp_file, sep=""))
  rm(exp_list, ann_mean, d24h_mean, d1h_max, d8h_max, cmaq)
}

#' =============================================================================
#' Areal averaging at the ZCTA level
#' =============================================================================















