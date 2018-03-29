#' -----------------------------------------------------------------------------
#' Project: ECHO Aim 1 
#' Date created: September 11, 2017
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' 
#' This project examines the relationships between spatially-distributed
#' economic, environmental, and social variables and health outcomes meausred
#' in the Healthy Start cohort (UC Denver)
#' 
#' This script summarizes the CMAQ data
#'     - Object for the receptors (4 km grid cell centroids)
#'     - Each exposure metric as a separate R object for analysis
#' -----------------------------------------------------------------------------

library(sp)
library(gstat)
library(rgdal)
library(raster)
library(ggplot2)
library(ggmap)
library(rgeos)
library(maptools)
library(ggthemes)
library(ggrepel)
library(stringr)
library(ncdf4)
library(tidyverse)

#' For ggplots
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=10),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)

windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

geo_data <- "T:/Rsch-MRS/ECHO/SEM Large Data/Spatial Data"
utm_13 <- "+init=epsg:26913"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Set up the analysis
#' Will be done in the "master" script later

cmaq_out <- "southern_colorado.nc"
start_date <- as.Date("01/01/2011", format="%m/%d/%Y")
#' -----------------------------------------------------------------------------

#' Open the netcdf file
#' Has 4 dimensions: lat, long, hour, and day in GMT
#' Has 4 variables: lat, long, ozone, pm
cmaq_name <- paste("./Data/CMAQ Data/", cmaq_out, sep="") 
cmaq <- nc_open(cmaq_name)
print(cmaq)

#' Extract coordinates
cmaq_lon <- ncvar_get(cmaq, varid="lon")
cmaq_lat <- ncvar_get(cmaq, varid="lat")

#' Extract variables
#' Dimensions: lat (19 rows), long (30 columns), hour (24), day (30)
cmaq_pm <- ncvar_get(cmaq, varid="pm")
cmaq_o3 <- ncvar_get(cmaq, varid="ozone")

dim(cmaq_pm)
dim(cmaq_o3)

n_days_pm <- dim(cmaq_pm)[4]
n_days_o3 <- dim(cmaq_o3)[4]

#' What is the "fillvalue" for the data?
cmaq_pm_fill <- ncatt_get(cmaq, "pm", "_FillValue")
cmaq_o3_fill <- ncatt_get(cmaq, "ozone", "_FillValue")

#' set fill values to NA
cmaq_pm[cmaq_pm == cmaq_pm_fill$value] <- NA
cmaq_o3[cmaq_o3 == cmaq_o3_fill$value] <- NA

#' close the netcdf file
nc_close(cmaq)

#' -----------------------------------------------------------------------------
#' Create data frames for analysis
#' -----------------------------------------------------------------------------

#' matrix of grid cell coordinates
lon_lat <- data.frame("lon" = as.vector(cmaq_lon),
                      "lat" = as.vector(cmaq_lat))
dim(lon_lat)

#' function to convert the 4d array into a data frame
array_to_df <- function (nc, coord_df) {
  pol_df <- data.frame()
  hour_seq <- seq(1:dim(nc)[3])
  day_seq <- seq(1:dim(nc)[4])
  
  for (i in 1:length(day_seq)) {
    for (j in 1:length(hour_seq)) {
      temp <- as.data.frame(cbind(as.matrix(coord_df), 
                                  as.vector(nc[,,j,i])))
      temp$day <- day_seq[i]
      temp$hour <- hour_seq[j] - 1
      pol_df <- rbind(pol_df, temp)
      rm(temp)
    }
  }
  colnames(pol_df) <- c("lon", "lat", "value", "day_UTC", "hour_UTC")
  return(pol_df)
}

#' 4-D arrays to list of data frames for summarizing below
df_list <- list("pm" = array_to_df(nc = cmaq_pm, coord_df = lon_lat),
                "o3" = array_to_df(nc = cmaq_o3, coord_df = lon_lat))

rm(cmaq_pm, cmaq_o3)

#' Calculate annual and daily metrics for each pollutant
date_seq <- seq.Date(from = start_date, length.out = n_days_pm, by="days")

for (i in 1:length(df_list)) {
  df <- df_list[[i]]
  df_name <- names(df_list)[i]

  #' ---------------------------------------------------------------------------
  #' Set up the data frame
  #' ---------------------------------------------------------------------------
  
  #' add an arbitrary receptor ID and then add back the coordinates later
  rec <- unique(df[,c("lon", "lat")])
  rec$id <- seq(1:nrow(rec))
  
  #' date and hour indicators
  if ((n_days_pm == n_days_o3) != T) stop()
  df$dt_utc <- as.POSIXct(paste(date_seq[df$day_UTC], df$hour_UTC), 
                          format="%Y-%m-%d %H", tz="GMT")
  df$dt_mst <- format(df$dt_utc, tz="America/Denver")
  df$date <- as.character(as.Date(df$dt_mst, format="%Y-%m-%d"))
  df$day <- as.numeric(as.factor(df$date))
  df$hour <- substr(df$dt_mst, start = 12, stop = 13)
      
  #' adding receptor ID
  df <- merge(df, rec, by=c("lon", "lat"))
  df <- df[with(df, order(date, hour, id)),]
  
  #' ---------------------------------------------------------------------------  
  #' Summarizing HIA exposure metrics at each receptor
  #' ---------------------------------------------------------------------------
  
  #' Daily mean
  df_pol <- df[,c("id", "day", "hour", "lon", "lat", "value")]

  d24h_mean <- ddply(df_pol, .(id, day), summarize, 
                     d24h_mean = mean(value))
  d24h_mean <- merge(d24h_mean, rec, by="id")

  #' Annual mean-- mean of all data available
  ann_mean <- ddply(df_pol, .(id), summarize, 
                    ann_mean = mean(value))
  ann_mean <- merge(ann_mean, rec, by="id")

  #' Daily 1 hour maximum concentration
  #' Not used in PM2.5 analysis, but calculating here anyway
  d1h_max <- ddply(df_pol, .(id, day), summarize, 
                   d1h_max = max(value))
  d1h_max <- merge(d1h_max, rec, by="id")

  #' Daily 8 hour max
  #' Need a wide data set
  df_wide <-  reshape(df_pol[,c("id", "day", "hour", "value")], 
                      timevar = c("id"),
                      idvar = c("day", "hour"), direction = "wide")
  
  #' caluclate the max 8-hour average for each day
  #' See FR Vol 80 No 206 Pg 65459 for definition of the design value
  #' Not used in PM2.5 analysis, but calculating here anyway
  d8h_max_w <- data.frame(day=as.character(unique(df_wide[,c("day")])))

  day_list <- unique(df_wide$day)
  
  col_ids <- c("day", "hour")
  
  n <- length(col_ids)
  
  for (j in 1:(ncol(df_wide)-n)) {
    name <- gsub("value.", "Rec_", colnames(df_wide)[j+n])
    
    temp_df <- data.frame(day = as.numeric(),
                          max = as.numeric())
    
    for (k in 1:length(day_list)) {
      days <- c(day_list[k], day_list[k+1])
      
      #' two days of hourly data at receptor j 
      met_df <- df_wide[which(df_wide$day %in% days), c(1, 2, j+n)] 
      met_df$hour2 <- -1 + seq(1:nrow(met_df))
      
      #should have 17 moving averages starting at 7:00am and ending at 11:00pm
      avg_list <- list()
      a <- 16 
      
      for (l in 7:(7+a)) {
        hours <- seq(from=l, to=l+7)
        conc<- met_df[which(met_df$hour2 %in% hours),3]
        
        #' need at least 6 hourly concentrations to calculate mean
        avg_list[l-6] <- ifelse(sum(!is.na(conc)) >= 6, mean(conc, na.rm=T), NA)
      }
      
      b <- unlist(avg_list)
      
      #' only valid if there are >= 13 8-hr means
      max <- ifelse(sum(!is.na(b)) >= 13, max(b, na.rm=T), NA)
      
      temp <- data.frame(day = days[1], max = max)
      temp_df <- rbind(temp_df, temp)
    }
    
    d8h_max_w <- merge(d8h_max_w, temp_df, by="day")
    colnames(d8h_max_w)[ncol(d8h_max_w)] <- name
    rm(met_df, temp_df)
  }
    
  #' Transpose d8h_max so that rows are receptors
  rownames(d8h_max_w) <- d8h_max_w$day
  d8h_max_w$day <- NULL
  d8h_max_wt <- as.data.frame(t(d8h_max_w))
  
  d8h_max_wt$rec_id <- gsub("Rec_", "", rownames(d8h_max_wt))
  
  #' convert back to a long dataset
  d8h_max <- reshape(d8h_max_wt, 
                     varying=colnames(d8h_max_wt)[-ncol(d8h_max_wt)],
                     v.names = "d8h_max",
                     timevar = "day",
                     times = colnames(d8h_max_wt)[-ncol(d8h_max_wt)],
                     direction = "long")  
  d8h_max$id <- d8h_max$rec_id
  d8h_max$rec_id <- NULL
  
  d8h_max <- merge(d8h_max, rec, by="id")

  exp_list <- list(d24h_mean, ann_mean, d1h_max, d8h_max)
  names(exp_list) <- c("d24h_mean", "ann_mean", "d1h_max", "d8h_max")
    
  save(exp_list,
       file=paste("./HIA Inputs/", df_name, "_exposure_metrics.RData", sep=""))
  
  rm(df_pol, df_wide, d24h_mean, ann_mean, d1h_max, d8h_max)
}

