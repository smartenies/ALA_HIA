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
#' This script summarized air pollution exposures at the ZCTA level
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

load("./Data/Spatial Data/ZCTA grid.RData")

#' For now, using dummy CMAQ data-- will need to reconcile this code later
#' Making a raster with the same spatial resolution as the population density
#' grid for now
#' WILL NEED TO MAKE SURE HTE CMAQ OUTPUT AND POPULATION DENSITY DATA MATCH
#' IF NOT-- WILL NEED TO ADJUST POP DENSITY GRID AND THEN REDO THE ZCTA GRID

n <- nrow(zcta_grid) 
reps <- 24*5 #how many hours do I want to simulate
cmaq_pm <- matrix(sample(5:15, n*reps, replace=TRUE), ncol=reps)
cmaq_o3 <- matrix(sample(50:80, n*reps, replace=TRUE), ncol=reps)

rm(n, reps, zcta_grid, zcta_pts)

#' =============================================================================
#' Summarize the hourly data into daily and annual metrics
#' =============================================================================

#' function for daily metrics
daily_met <- function(mat, hrs = 24, fun) {
   days <- ncol(mat) / hrs
   f_mat <- matrix(nrow=(nrow(mat)))
   
   for (day in 1:days) {
     d_mat <- mat[,((day*hrs)-(hrs-1)):(day*hrs)]
     temp <- as.matrix(apply(d_mat, 1, fun))
     f_mat <- cbind(f_mat, temp) 
   }
   return(f_mat[,-1])
}

#' function for 8 h daily max (Based on the ozone NAAQS)


#' Check dimentions of the CMAQ matrix
dim(cmaq_pm)
dim(cmaq_o3)
#' 405002 receptors and 48 hours

#' all of the cmaq outputs we want to calculate metrics for
cmaq_list <- list(cmaq_pm, cmaq_o3)
cmaq_name <- c("pm", "o3")

for (i in 1:length(cmaq_list)) {
  # ID the first matrix
  cmaq <- cmaq_list[[i]]
  name <- cmaq_name[i]
  
  #' annual mean
  ann_mean <- as.matrix(rowMeans(cmaq))
  
  #' daily means
  d24h_mean <- daily_met(mat = cmaq, fun = mean)

  
}






#' Second, need to calculate the daily 8-hr max 
#' (highest 8 h mean in the 24 h period)
#' and average over the 5 year period

o3 <- ap[which(ap$Parameter.Code == p[2]),]

#' Need a wide data set
#wide_ids <- colnames(o3)[-c(17,27)]
wide_ids <- c("datetime")
monitor_ids <- unique(o3$monitor_id)
o3_sort <- o3[order(o3$monitor_id, o3$datetime),
              c("datetime", "monitor_id", "Sample.Measurement")]
o3_wide <-  reshape(o3_sort, timevar = c("monitor_id"),
                    idvar = wide_ids,
                    direction = "wide")

#' Make sure all dates are included
date_df <- data.frame(datetime = format(seq.POSIXt(as.POSIXct("2010-01-01 00:00"), 
                                                   as.POSIXct("2014-12-31 23:00"), 
                                                   by = "1 hour"), 
                                        "%Y/%m/%d %H:%M:%S"))
date_df$datetime <- as.POSIXct(date_df$datetime)
o3_wide <- merge(date_df, o3_wide, by="datetime", all.x=T)
rm(date_df)

#' Add an indentifier for each day and each hour within that day
o3_wide$date <- format(o3_wide$datetime, format="%Y/%m/%d")
dates <- unique(o3_wide$date)
temp <- data.frame()
for (i in 1:length(dates)) {
  date <- dates[i]
  sub <- o3_wide[which(o3_wide$date == date),]
  sub$hour <- seq(1:nrow(sub))
  sub$day <- i
  sub$month <- format(sub$datetime, format="%m")
  sub$year <- format(sub$datetime, format="%Y")
  temp <- rbind(temp, sub)
  if(i %% 100 == 0) print(date)
}

o3_wide <- temp
o3_wide <- o3_wide[,c(1,25:29,2:24)]
colnames(o3_wide) <- gsub("Sample.Measurement", "O3", colnames(o3_wide))

rm(temp)

#' caluclate the max 8-hour average for each day
#' See FR Vol 80 No 206 Pg 65459 for definition of the design value
o3_d8hmax <- data.frame(date=unique(o3_wide[,c("date")]))
o3_d8hmax$date <- as.character(o3_d8hmax$date)
o3_d8hmax$day <- seq(1:nrow(o3_d8hmax))
o3_d8hmax$month <- substr(o3_d8hmax$date, 6, 7)
o3_d8hmax$year <- substr(o3_d8hmax$date, 1, 4)
day_list <- unique(o3_wide$day)

monitor_ids2 <- paste("O3", monitor_ids, sep=".")
col_ids <- colnames(o3_wide)[which(!(colnames(o3_wide) %in% monitor_ids2))]

n <- length(col_ids)

for (i in 1:(ncol(o3_wide)-n)) {
  name <- colnames(o3_wide)[i+n]
  temp_df <- data.frame(day = as.numeric(),
                        max = as.numeric())
  
  for (j in 1:length(day_list)) {
    days <- c(day_list[j], day_list[j+1])
    df <- o3_wide[which(o3_wide$day %in% days), c(3, 4, i+n)]
    df$hour2 <- -1 + seq(1:nrow(df))
    
    avg_list <- list()
    a <- 16 #should have 17 moving averages starting at 7:00am and ending at 11:00pm
    
    for (k in 7:(7+a)) {
      hours <- seq(from=k, to=k+7)
      conc<- df[which(df$hour2 %in% hours),3]
      #' need at least 6 hourly concentrations to calculate mean
      avg_list[k-6] <- ifelse(sum(!is.na(conc)) >= 6, mean(conc, na.rm=T), NA)
    }
    
    b <- unlist(avg_list)
    
    #' only valid if there are >= 13 8-hr means
    max <- ifelse(sum(!is.na(b)) >= 13, max(b, na.rm=T), NA)
    
    temp <- data.frame(day = days[1], max = max)
    temp_df <- rbind(temp_df, temp)
  }
  o3_d8hmax <- merge(o3_d8hmax, temp_df, by="day")
  colnames(o3_d8hmax)[ncol(o3_d8hmax)] <- name
  print(name)
}

save(o3_d8hmax, file="./Data/Air Quality/Daily 8h max ozone.RData")









