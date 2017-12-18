#' =============================================================================
#' Project: American Lung Association HIA
#' Date created: September 26, 2017
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
#' This script generates the exposure data for each ZCTA in the area
#'     1) The ZCTAs are rasterized (resolution = xx m)
#'     2) Ppulation density is assigned to ZCTA cells
#'     3) The 4 km x 4 km grid is downscaled to a XX m grid
#'     4) Daily population-weighted exposure metrics are calculated
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

#' -----------------------------------------------------------------------------
#' 1) Rasterize ZCTAs in Colorado
#' Assigns the ZCTA identifier to the population density grid
#' Used to generate population-weighted exposure metrics for each ZCTA and day 
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/co_zcta_utm_map.RData")

zcta_ras <- rasterize()



#' -----------------------------------------------------------------------------
#' Exposure metric functions
#' -----------------------------------------------------------------------------

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









