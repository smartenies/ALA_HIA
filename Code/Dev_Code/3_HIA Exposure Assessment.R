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
#' This script assesses ZCTA-level exposures weighted by population density
#' to be used in the HIA
#' -----------------------------------------------------------------------------

library(sp)
library(spatialEco)
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
library(plyr)
library(Hmisc)

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
#' Read in ZCTA shapefiles, and receptor grid with population density
#' -----------------------------------------------------------------------------

load("./Data/CMAQ Data/cmaq_pop_density.RData")
cmaq_p$id <- NULL
cmaq_p_df <- as.data.frame(cmaq_p@data)

load("./Data/Spatial Data/zip codes.RData")

plot(zcta,
     main = "ZCTAs that intersect and are completely within the CMAQ domain")
plot(zcta_within, add=T, col="red")
points(cmaq_p, pch = 20, cex=0.5, col="blue")

#' -----------------------------------------------------------------------------
#' population density weighted-average of CMAQ receptors that fall within each
#' ZCTA
#' -----------------------------------------------------------------------------

#' Pollutant list
pol_list <- c("pm", "o3")
hia_list <- list()

for (i in 1:length(pol_list)) {
  load(paste("./HIA Inputs/", pol_list[i], "_exposure_metrics.RData", sep=""))
  metrics <- names(exp_list)
  
  #' Data frame with averaged exposures for each ZCTA each day
  zcta_avg_df <- data.frame(ZCTA5CE10 = as.character(unique(zcta$GEOID10)))
  zcta_sd_df <- data.frame(ZCTA5CE10 = as.character(unique(zcta$GEOID10)))

  for (j in 1:length(metrics)) {
    exp_df <- merge(cmaq_p@data, exp_list[[j]], by=c("lon", "lat"))
    met <- metrics[j]
    
    #' List of days in the analysis
    day_list <- unique(exp_df$day)
    
    for (k in 1:length(day_list)) {
      day_df <- exp_df[which(exp_df$day == day_list[k]),]
      
      #' Merge day metrics with cmaq receptor points
      cmaq_exp <- merge(cmaq_p[,c("lon", "lat")], day_df, by=c("lon", "lat"))
      
      #' ID which receptors are in each ZCTA
      zcta_exp <- as.data.frame(point.in.poly(cmaq_exp, zcta))
      
      #' Calculate weighted average in each ZCTA
      zcta_wt <- ddply(zcta_exp, .(ZCTA5CE10),
                       function(x) data.frame(wt_mean = wtd.mean(x[,met],
                                                                 x$pop_density),
                                              wt_var = wtd.var(x[,met],
                                                               x$pop_density)))
      zcta_wt$wt_sd <- sqrt(zcta_wt$wt_var)
      
      #' merge average and sd with data frame
      met_name <- paste(met, day_list[k], sep=".")
      zcta_avg_df <- merge(zcta_avg_df, zcta_wt[,c("ZCTA5CE10", "wt_mean")], by="ZCTA5CE10")
      zcta_sd_df <- merge(zcta_sd_df, zcta_wt[,c("ZCTA5CE10", "wt_sd")], by="ZCTA5CE10")
      
      colnames(zcta_avg_df)[ncol(zcta_avg_df)] <- met_name
      colnames(zcta_sd_df)[ncol(zcta_sd_df)] <- met_name
    }
  }
  hia_list[[i]] <- list(zcta_avg_df, zcta_sd_df)
  names(hia_list)[i] <- pol_list[i]
  
  # rm(zcta_avg_df, zcta_sd_df, exp_list, exp_df, met,
  #    day_list, day_df, cmaq_exp, zcta_exp, zcta_wt)
}













