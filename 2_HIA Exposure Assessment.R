#' =============================================================================
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
#'     - Each exposure metric as a separate R object
#' 
#' NOTE: don't forget the ./ before the directory when reading in files!
#' =============================================================================

library(foreign)
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
library(RColorBrewer)
library(gridExtra)
library(plyr)
library(stringr)
library(ncdf4)

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

#' =============================================================================
#' Set up the analysis
#' Will be done in the "master" script later

cmaq_out <- "southern_colorado.nc"
#' =============================================================================

#' Open the netcdf file
#' Has 4 dimensions: lat, long, hour, and day in GMT
#' Has 4 variables: lat, long, ozone, pm
cmaq <- nc_open(paste("./Data/CMAQ Data/", cmaq_out, sep=""))
print(cmaq)

#' Extract coordinates for the CMAQ receptors
#' Matrix dimensions are 19 x 30
cmaq_lon <- ncvar_get(cmaq, varid="lon")
cmaq_lat <- ncvar_get(cmaq, varid="lat")

dim(cmaq_lat) #' 19 rows x 30 columns receptors

#' Create data frame with ID 
cmaq_df <- data.frame(x = as.vector(cmaq_lon),
                      y = as.vector(cmaq_lat))
cmaq_df$id <- seq(1:nrow(cmaq_df))

#' Merge receptors with population density weights
load("./Data/CMAQ Data/cmaq_pop_denisty.RData")

cmaq_df <- merge(cmaq_df, cmaq_p_df[,c("id", "pop_density")], by="id")






#' Extract pollution variables
cmaq_ozone <- ncvar_get(cmaq, var="ozone")
cmaq_pm <- ncvar_get(cmaq, var="pm")

dim(cmaq_ozone) #19 x 30 receptors, 24 hours x 30 days 
dim(cmaq_pm) #19 x 30 receptors, 24 hours x 30 days 


#nc_close(cmaq)





folder <- "./Data/CMAQ Data/"
all_files <- list.files(path = folder, full.names = F, pattern=".csv")

cities <- lapply(strsplit(all_files, split="_"), `[[`, 1)
save(cities, file="./HIA Inputs/Cities.RData")

for (city in 1:length(cities)) {
  city_name <- unlist(cities[city])
  
  files <- grep(cities[city], all_files, value=T)
  files <- paste(folder, files, sep="")
  
  #' ---------------------------------------------------------------------------
  #' Get all data for that particular city
  #' ---------------------------------------------------------------------------

  df <- do.call(`rbind`,lapply(files, read.csv, header=T, stringsAsFactors=F))
    
  #' ---------------------------------------------------------------------------
  #' Set up the receptor data frames
  #' ---------------------------------------------------------------------------
  rec <- unique(df[,c("longitude", "latitude")])
  rec <- rec[with(rec, order(longitude, latitude)),]
  rec$rec_id <- rev(seq(1:nrow(rec)))
  rec <- rec[order(rec$rec_id),]
  
  save(rec, file=paste("./HIA Inputs/", city_name, 
                        "_receptors.RData", sep=""))
  save(rec, file=paste("./Data/CMAQ Data/", city_name, 
                        "_receptors.RData", sep=""))
  
  #' write out receptors to a shapefile
  rec_sp <- rec
  coordinates(rec_sp) <- c("longitude", "latitude")
  proj4string(rec_sp) <- CRS(ll_wgs84)
  writeOGR(rec_sp, dsn = cmaq_data, 
           layer = paste(city_name, "rec", sep="_"),
           driver="ESRI Shapefile", overwrite_layer = T)
  
  #' date and hour indicators
  df$datetime <- as.POSIXct(df$Date)
  df$Date <- NULL
  df$date <- substr(as.character(df$datetime), start=1, stop=10)
  df$day <- str_pad(as.character(cumsum(!duplicated(df$date))), 
                    width=3, pad="0")
  df$hour <- substr(as.character(df$datetime), start=12, stop=13)
    
  #' adding receptor ID
  df <- merge(df, rec, by=c("longitude", "latitude"))
  df <- df[with(df, order(rec_id, day, hour)),]
  
  rm(rec)
  
  #' ---------------------------------------------------------------------------  
  #' Summarizing PM2.5 metrics at each receptor
  #' ---------------------------------------------------------------------------
  
  #' Daily mean
  df_PM2.5 <- df[,c("rec_id", "day", "hour", "longitude", "latitude", "PM.ug.m3.")]

  d24h_mean <- ddply(df_PM2.5, .(rec_id, day), summarize, 
                     d24h_mean = mean(PM.ug.m3.))
  d24h_mean <- reshape(d24h_mean, timevar = "day", 
                       idvar= "rec_id", direction="wide")
    
  #' Annual mean-- aka mean of all data available
  ann_mean <- ddply(df_PM2.5, .(rec_id), summarize, 
                    ann_mean = mean(PM.ug.m3.))

  pm <- list(d24h_mean, ann_mean)
  names(pm) <- c("d25h_mean", "ann_mean")
  save(pm,
       file=paste("./HIA Inputs/", city_name, "_PM2.5_metrics.RData", sep=""))
  
  rm(df_PM2.5, d24h_mean, ann_mean, pm)
    
  #' ---------------------------------------------------------------------------
  #' Summarizing O3 metrics at each receptor
  #' ---------------------------------------------------------------------------
  
  #' Daily mean
  df_O3 <- df[,c("rec_id", "day", "hour", "longitude", "latitude", "Ozone.ppm.")]
  
  d24h_mean <- ddply(df_O3, .(rec_id, day), summarize, 
                     O3_d24h_mean = mean(Ozone.ppm.))
  d24h_mean <- reshape(d24h_mean, timevar = "day", 
                       idvar= "rec_id", direction="wide")

  #' Annual mean-- aka mean of all data available
  ann_mean <- ddply(df_O3, .(rec_id), summarize, 
                    ann_mean = mean(Ozone.ppm.))
  
  #' Daily 1 hour maximum concentration
  d1h_max <- ddply(df_O3, .(rec_id, day), summarize, 
                   d1h_max = max(Ozone.ppm.))
  d1h_max <- reshape(d1h_max, timevar = "day", 
                     idvar= "rec_id", direction="wide")  
  
  #' Daily 8 hour max
  #' Need a wide data set
  
  o3_wide <-  reshape(df_O3[,c("rec_id", "day", "hour", "Ozone.ppm.")], 
                      timevar = c("rec_id"),
                      idvar = c("day", "hour"), direction = "wide")
  
  #' caluclate the max 8-hour average for each day
  #' See FR Vol 80 No 206 Pg 65459 for definition of the design value
  d8h_max <- data.frame(day=as.character(unique(o3_wide[,c("day")])))

  day_list <- unique(o3_wide$day)
  
  col_ids <- c("day", "hour")
  
  n <- length(col_ids)
  
  for (i in 1:(ncol(o3_wide)-n)) {
    name <- gsub("Ozone.ppm..", "Rec_", colnames(o3_wide)[i+n])
    
    temp_df <- data.frame(day = as.numeric(),
                          max = as.numeric())
    
    for (j in 1:length(day_list)) {
      days <- c(day_list[j], day_list[j+1])
      met_df <- o3_wide[which(o3_wide$day %in% days), c(1, 2, i+n)]
      met_df$hour2 <- -1 + seq(1:nrow(met_df))
      
      avg_list <- list()
      a <- 16 #should have 17 moving averages starting at 7:00am and ending at 11:00pm
      
      for (k in 7:(7+a)) {
        hours <- seq(from=k, to=k+7)
        conc<- met_df[which(met_df$hour2 %in% hours),3]
        
        #' need at least 6 hourly concentrations to calculate mean
        avg_list[k-6] <- ifelse(sum(!is.na(conc)) >= 6, mean(conc, na.rm=T), NA)
      }
      
      b <- unlist(avg_list)
      
      #' only valid if there are >= 13 8-hr means
      max <- ifelse(sum(!is.na(b)) >= 13, max(b, na.rm=T), NA)
      
      temp <- data.frame(day = days[1], max = max)
      temp_df <- rbind(temp_df, temp)
    }
    
    d8h_max <- merge(d8h_max, temp_df, by="day")
    colnames(d8h_max)[ncol(d8h_max)] <- name
  }
    
  #' Transpose d8h_max so that rows are receptors
  rownames(d8h_max) <- d8h_max$day
  d8h_max$day <- NULL
  d8h_max <- as.data.frame(t(d8h_max))
  
  colnames(d8h_max) <- paste("d8h_max", colnames(d8h_max), sep=".")
  d8h_max$rec_id <- as.integer(seq(1:nrow(d8h_max)))
    
  d8h_max <- d8h_max[,c(ncol(d8h_max), 1:(ncol(d8h_max) - 1))]  
  
  o3 <- list(d24h_mean, ann_mean, d1h_max, d8h_max)
  names(o3) <- c("d25h_mean", "ann_mean", "d1h_max", "d8h_max")
    
  save(o3, 
       file=paste("./HIA Inputs/", city_name, "_O3_metrics.RData", sep=""))
  
  rm(df_O3, d24h_mean, ann_mean, d1h_max, d8h_max)
}

