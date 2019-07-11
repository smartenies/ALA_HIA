#' -----------------------------------------------------------------------------
#' Project: American Lung Association HIA
#' Date created: March 28, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' This script calculates what fraction of the overall Pm2.5 and O3 concentrations
#' is reduced by the scenario
#' -----------------------------------------------------------------------------

library(sp)
library(gstat)
library(rgdal)
library(raster)
library(rgeos)
library(maptools)
library(ggplot2)
library(ggrepel)
library(ggmap)
library(ggsn)
library(ggpubr)
library(ggthemes)
library(ncdf4)
library(Hmisc)
library(tidyverse)
library(readxl)
library(writexl)
library(readxl)
library(sf)
library(viridis)
library(DescTools)
library(IC2)
library(metafor)
library(foreach)
library(doParallel)
library(snow)

#' For ggplots
simple_theme <- theme(
  text  = element_text(family="Calibri",size = 15, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color="transparent"),
  panel.grid.major = element_line(colour="transparent"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=10),
  #legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)

windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

geo_data <- "T:/Rsch-MRS/ECHO/SEM Large Data/Spatial Data"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' Unique prefixes for each run
pre <- c("Baseline_Winter_AllCO_",
         "Baseline_Summer_AllCO_")

cmaq_baseline <- c("colorado_winter_2017_18.52.nc",
                   "colorado_summer_2017_196.230.nc")

cmaq_scenario <- c(NA, NA)

start_dates <- c(as.Date("01-18-2011", format="%m-%d-%Y"),
                 as.Date("07-15-2011", format="%m-%d-%Y"))

end_dates <- c(as.Date("02-21-2011", format="%m-%d-%Y"),
               as.Date("08-18-2011", format="%m-%d-%Y"))

#' Which year should we use for the population density? 2010 or 2015
pop_den_tif <- "2010-COloradoPopDensity.tif"

#' exposure scale factor to avoid errors in kriging
exp_scale <- 10**6

#' -----------------------------------------------------------------------------
#' Read in the CMAQ data, get the receptor coordinates, and create data frame
#' -----------------------------------------------------------------------------

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

#' Function to open the netCDF and output a data frame 
cmaq_data_output <- function(cmaq_data) {
  cmaq_name <- paste("./Data/CMAQ Data/", cmaq_data, sep="")
  cmaq <- nc_open(cmaq_name)
  print(cmaq)
  
  #' Extract coordinates
  cmaq_lon <- ncvar_get(cmaq, varid="lon")
  cmaq_lat <- ncvar_get(cmaq, varid="lat")
  
  #' Extract variables
  #' Dimensions: lat, long, , hour (n = 24), day 
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
  
  #' Create receptor data frame with ID 
  cmaq_df <- data.frame(lon = as.vector(cmaq_lon),
                        lat = as.vector(cmaq_lat))
  cmaq_df$id <- seq(1:nrow(cmaq_df))
  
  #' Create a spatial points DF from the CMAQ coordinates
  cmaq_p <- cmaq_df
  coordinates(cmaq_p) <- c("lon", "lat")
  proj4string(cmaq_p) <- CRS(ll_wgs84)
  cmaq_p
  cmaq_p@data <- cmaq_df
  
  #' get the extent of the grid
  cmaq_e <- extent(cmaq_p)
  save(cmaq_e, cmaq_p, file=paste("./HIA Inputs/", pre[s], "cmaq_spatial.RData",
                                  sep=""))
  
  #' matrix of grid cell coordinates
  lon_lat <- data.frame("lon" = as.vector(cmaq_lon),
                        "lat" = as.vector(cmaq_lat))
  dim(lon_lat)
  
  #' 4-D arrays to list of data frames for summarizing below
  #' Convert O3 from ppm to ppb
  df_list <- list("pm" = array_to_df(nc = cmaq_pm, coord_df = lon_lat),
                  "o3" = array_to_df(nc = cmaq_o3*1000, coord_df = lon_lat))
  
  return(df_list)
  rm(cmaq, cmaq_pm, cmaq_o3, lon_lat, cmaq_o3_fill, cmaq_pm_fill)
}

for (s in 1:length(pre)) {
  if(!file.exists(paste("./HIA Inputs/", pre[s], "CMAQ_output.RData", sep=""))) {
    #' Get "baseline" concentrations and, if specified, subtract scenario
    baseline <- cmaq_data_output(cmaq_data = cmaq_baseline[s])
    
    if (!(is.na(cmaq_scenario[s]))) {
      #' get scenario data frame
      scenario <- cmaq_data_output(cmaq_data = cmaq_scenario[s])
      
      if (all.equal(baseline[[1]]$day_UTC, scenario[[1]]$day_UTC) != T) stop()
      
      #' subtract scenario values from baseline values to get exposure differences
      baseline[[1]]$value <- baseline[[1]]$value - scenario[[1]]$value
      baseline[[2]]$value <- baseline[[2]]$value - scenario[[2]]$value
      
      rm(scenario)
    }
    
    save(baseline, file=paste("./HIA Inputs/", pre[s], "CMAQ_output.RData", sep=""))
  }
}


#' -----------------------------------------------------------------------------
#' Calculate annual metrics for each pollutant and krige to ZCTAs
#' -----------------------------------------------------------------------------

for(s in 1:length(pre)) {
  load(paste("./HIA Inputs/", pre[s], "CMAQ_output.RData", sep=""))
  load(paste("./HIA Inputs/", pre[s], "cmaq_spatial.RData", sep=""))
  
  #' Subset CMAQ points based on the HIA boundary and get a new extent
  load("./Data/Spatial Data/hia_boundary.RData")
  hia_boundary <- st_transform(hia_boundary, crs=ll_wgs84) %>% 
    as("Spatial")
  
  plot(cmaq_p)
  plot(hia_boundary, border="red", add=T)
  
  cmaq_p <- cmaq_p[hia_boundary,]
  cmaq_e <- extent(cmaq_p)
  
  cmaq_ids <- unique(cmaq_p$id)
  
  plot(cmaq_p)
  plot(hia_boundary, border="red", add=T)
  
  save(cmaq_e, cmaq_p, cmaq_ids,
       file=paste("./HIA Inputs/", pre[s], "cmaq_spatial.RData", sep=""))
  
  load(file=paste("./HIA Inputs/", pre[s], "CMAQ_output.RData", sep=""))
  
  #' Sequence of dates based on start date and end date of the baseline scenario
  date_seq <- seq.Date(from=start_dates[s], to=end_dates[s], by="day")
  
  for (i in 1:length(baseline)) {
    
    df <- baseline[[i]]
    df_name <- names(baseline)[i]
    
    if(!file.exists(paste("./HIA Inputs/", pre[s], df_name, "_receptor_metrics.RData",
                          sep=""))) {
      print("Setting up data frame for metrics")
      print(df_name)
      
      #' Set up the data frame
      #' add an arbitrary receptor ID and then add back the coordinates later
      rec <- unique(df[,c("lon", "lat")])
      rec$id <- seq(1:nrow(rec))
      
      #' date and hour indicators
      df$dt_utc <- as.POSIXct(paste(date_seq[df$day_UTC], df$hour_UTC), 
                              format="%Y-%m-%d %H", tz="GMT")
      df$dt_mst <- format(df$dt_utc, tz="America/Denver")
      df$date <- as.character(as.Date(df$dt_mst, format="%Y-%m-%d"))
      df$day <- as.numeric(as.factor(df$date))
      df$hour <- substr(df$dt_mst, start = 12, stop = 13)
      
      #' adding receptor ID
      df <- merge(df, rec, by=c("lon", "lat"))
      df <- df[with(df, order(date, hour, id)),]
      
      df_pol <- df[,c("id", "day", "hour", "value")]
      
      #' ---------------------------------------------------------------------------
      #' Summarizing HIA exposure metrics at each receptor
      #' ---------------------------------------------------------------------------
      
      #' (1) "Annual" mean-- mean of all data available
      print("Annual mean")
      print(df_name)
      ann_mean <- df_pol %>%
        group_by(id) %>%
        summarise(ann_mean = mean(value)) 
      
      ann_mean$day <- 1
      ann_mean <- merge(ann_mean, rec, by="id")  
      
      exp_list <- list(ann_mean)
      names(exp_list) <- c("ann_mean")
      
      save(exp_list,
           file=paste("./HIA Inputs/", pre[s], df_name, "_receptor_metrics.RData",
                      sep=""))
      
      #' Clean up the loop
      rm(df_pol, ann_mean, exp_list)  
    }
    
    #' Clean up environment
    rm(df, df_name, rec, a, b, avg_list, 
       day_list, days, hours, i, j, k, l, max)
  }
  
  #' Clean up environment
  rm(baseline)
}

#' -----------------------------------------------------------------------------
#' Krige CMAQ concentrations to the 1 k populaton density grid and use
#' zonal statistics to get the average value for the ZCTAs weighted by area and
#' population density
#' -----------------------------------------------------------------------------

gc()
for (s in 1:length(pre)) {  
  print("Assessing exposures at the ZCTA level")
  load(paste("./HIA Inputs/", pre[s], "cmaq_spatial.RData", sep=""))
  
  #' Read in the population density geotiff
  #' Resolution is 0.00833 deg (approximately 1 km)
  #' Coordinates are in WGS84 (same as CMAQ)
  pop_den_t <- raster(paste("./Data/SEDAC Data/", pop_den_tif, sep=""))
  pop_den_t
  
  plot(pop_den_t)
  
  #' Trim the density raster using the CMAQ extent
  #' Project to UTM 13 N
  pop_den_r_1k <- crop(pop_den_t, cmaq_e)
  pop_den_r_1k
  
  plot(pop_den_r_1k)
  
  #' For a quick code check later
  pop_den_df_1k <- as.data.frame(pop_den_r_1k)
  
  #' Overlay the CMAQ receptors with the population denisty raster
  plot(pop_den_r_1k)
  points(cmaq_p, pch=20, cex=0.5)
  
  #' Create a grid for interpolation based on the population density grid
  pop_den_e <- extent(pop_den_r_1k)
  pop_den_res <- res(pop_den_r_1k)
  
  cmaq_r <- raster(pop_den_e, crs=crs(pop_den_r_1k),
                   nrows = nrow(pop_den_r_1k), ncols = ncol(pop_den_r_1k))
  cmaq_r
  
  #' grid needed for interpolation
  cmaq_g <- as(cmaq_r, "SpatialGrid")
  plot(cmaq_g)
  
  #' Read in the ZCTA shapefile and make sure CRS matches
  load("./Data/Spatial Data/co_zcta.RData")
  co_zcta <- st_transform(co_zcta, crs=ll_wgs84) %>%
    as("Spatial")
  
  plot(co_zcta)
  points(cmaq_p, col="red", pch=20, cex=0.5)
  
  #' #' Subset ZCTA that are completely within the CMAQ domain
  cmaq_bound <- gConvexHull(cmaq_p)
  save(cmaq_bound, file=paste("./HIA Inputs/", pre[s], "CMAQ_bound.RData", 
                              sep=""))
  
  # plot(co_zcta)
  # plot(cmaq_bound, add=T)
  # points(cmaq_p, col="red", pch=20, cex=0.5)
  
  #' All ZCTAs with at least one point
  zcta <- co_zcta[cmaq_bound,]
  zcta <- zcta[,"GEOID10"]
  zcta$id <- 1:nrow(zcta)
  
  zcta_key <- as.data.frame(zcta[,c("GEOID10", "id")])
  
  # plot(zcta)
  # points(cmaq_p, col="red", pch=20, cex=0.5)
  
  #' ZCTA completely within the boundary
  zcta$contains <- gContains(cmaq_bound, zcta, byid=T)[,1]
  zcta_within <- zcta[zcta$contains == T,]
  
  # plot(zcta_within)
  # points(cmaq_p, col="red", pch=20, cex=0.5)
  
  #' Save the spatial objects
  save(zcta, zcta_within,
       file=paste("./HIA Inputs/", pre[s], "zcta.RData", sep=""))
  
  #' Extract population density in each ZCTA using extract() (raster package)
  #' Save this data frame-- going to be the same for each metric, since the
  #' kriged surface is based on the population density raster 
  #' 
  #' NOTE: extract(weights = T) means that the extract function estimates the 
  #' fraction of the ZCTA area occupied by the grid cell. 
  
  if(!file.exists(paste("./HIA Inputs/", pre[s], "zcta_weights.RData", 
                        sep=""))) {
    zcta_weights <- raster::extract(pop_den_r_1k, zcta, weights=T, df=T,
                                    cellnumbers = T)
    zcta_weights <- zcta_weights %>% 
      rename(id = ID, pop_density = X2010.COloradoPopDensity)
    
    save(zcta_weights, file=paste("./HIA Inputs/", pre[s], "zcta_weights.RData", 
                                  sep=""))
  }
  
  load(file=paste("./HIA Inputs/", pre[s], "zcta_weights.RData", sep=""))
  
  #' check to see if area weights within each ZCTA sum to 1
  wt_check <- zcta_weights %>%
    group_by(id) %>%
    summarise(wt_zcta = sum(weight))
  summary(wt_check$wt_zcta)
  
  #' Loop through pollutants, metrics, and days to estimate pop-weighted
  #' average exposures at the ZCTA level
  pol_names <- c("pm", "o3")
  
  # show.vgms() 
  all_models <- c("Sph", "Exp", "Gau", "Wav", "Exc", 
                  "Ste", "Bes", "Mat", "Cir", "Pen")
  
  #' Krige these 4k points to the 1k points
  
  
  for (i in 1:length(pol_names)) {
    
    if(!file.exists(paste("./HIA Inputs/", pre[s], pol_names[i], "_zcta_metrics.RData",
                          sep=""))) {
      #' read in data
      load(paste("./HIA Inputs/", pre[s], pol_names[i], "_receptor_metrics.RData", 
                 sep=""))
      
      metrics <- names(exp_list)
      metrics
      
      zcta_list <- list()
      
      for (j in 1:length(metrics)) {
        
        #' extract data frame from list
        #' make sure to only include receptors within the study boundary
        test_df <- exp_list[[j]]
        test_df <- test_df[which(test_df$id %in% cmaq_ids),]
        
        #' create a spatial object from the metric data frame
        test_p <- test_df
        coordinates(test_p) <- c("lon", "lat")
        crs(test_p) <- CRS(ll_wgs84)
        test_p@data <- test_df
        
        #' list of days to loop through
        days <- sort(as.integer(unique(test_p$day)))
        
        #' run in parallel
        cl <- parallel::makeCluster(4, type = "SOCK")
        registerDoParallel(cl)
        
        met_df <- foreach(k=1:length(days), .combine = rbind,
                          #met_df <- foreach(k=1:2, .combine = rbind,
                          .packages = c("sf", "sp", "gstat", "raster",
                                        "rgdal", "rgeos", "maptools",
                                        "Hmisc", "tidyverse")) %dopar% {
                                          
                                          #' subset to daily metric
                                          test_p_2 <- test_p[which(test_p$day == days[k]),]
                                          
                                          #' scale exposures in order to avoid errors in kriging
                                          #' small values don't do well because issues with truncation!
                                          test_p_2@data[(metrics[j])] <- test_p_2@data[(metrics[j])] * exp_scale
                                          
                                          #' Create a smooth surface using ordinary kriging
                                          #'generate the semivariogram
                                          cmaq_vgm <- variogram(get(metrics[j]) ~ 1, test_p_2)
                                          # plot(cmaq_vgm)
                                          
                                          #' Fitting the semivariogram
                                          cmaq_fit <- fit.variogram(cmaq_vgm, model=vgm(all_models))
                                          # plot(cmaq_vgm, cmaq_fit)
                                          
                                          #' Ordinary kriging using the fitted semivariogram
                                          #' Use 24 nearest points (important for ozone)
                                          cmaq_ok <- krige(get(metrics[j]) ~ 1, test_p_2, cmaq_g, cmaq_fit,
                                                           nmax = 24)
                                          
                                          # spplot(cmaq_ok, "var1.pred",
                                          #        main = paste("pol= ", pol_names[i], ", met= ", metrics[j],
                                          #                     ", day= ", days[k], sep=""))
                                          
                                          
                                          #' Turn grid back into raster for zonal statistics
                                          #' Want to make sure the grid cells are in the same order as the
                                          #' population density raster when they get converted to a data frame
                                          cmaq_r <- raster(cmaq_ok)
                                          
                                          #' Zonal statistics using weights from the population-density raster
                                          cmaq_df <- as.data.frame(cmaq_r)
                                          cmaq_df$cell <- rownames(cmaq_df)
                                          
                                          #' Check! Data frame for kriging should match population density
                                          if (nrow(cmaq_df) != nrow(pop_den_df_1k)) stop()
                                          
                                          #' calculate weighted means and SD
                                          #' Need to "unscale" exposure concentrations to get back to original units
                                          #' See just before the  for details ()
                                          #' (Hmisc)
                                          zcta_conc <- merge(zcta_weights, cmaq_df, by="cell")
                                          zcta_conc <- rename(zcta_conc, conc = var1.pred) %>%
                                            arrange(id) %>%
                                            mutate(conc = conc / exp_scale)
                                          
                                          #' Calculate weighted averages at the ZCTA level
                                          #' New weight-- population density * the fraction of the polygon covered
                                          #' by the cell
                                          
                                          zcta_cmaq <- zcta_conc %>%
                                            mutate(wt_prod = pop_density * weight) %>%
                                            group_by(id) %>%
                                            summarise(wt_conc = wtd.mean(x = conc, weights = wt_prod),
                                                      wt_conc_sd = sqrt(wtd.var(x = conc, weights = wt_prod))) %>%
                                            left_join(zcta_key, by="id")
                                          head(zcta_cmaq)
                                          summary(zcta_cmaq)
                                          
                                          #' add indicators for pollutant, metric, and day
                                          zcta_cmaq$pol <- pol_names[i]
                                          zcta_cmaq$metric <- metrics[j]
                                          zcta_cmaq$day <- days[k]
                                          
                                          zcta_cmaq
                                        }
        stopCluster(cl)
        
        zcta_list[[j]] <- met_df
        names(zcta_list)[length(zcta_list)] <- metrics[j]
        
        rm(met_df)
      }
      save(zcta_list,
           file=paste("./HIA Inputs/", pre[s], pol_names[i], "_zcta_metrics.RData",
                      sep="")) 
    }
  }
}

#' -----------------------------------------------------------------------------
#' Calculate ratio of delta-PM/Baseline PM and delta-O3/Baseline O3
#' -----------------------------------------------------------------------------

#' Winter PM
load("./HIA Inputs/Baseline_Winter_AllCO_pm_zcta_metrics.RData")
w_baseline_pm <- zcta_list[[1]] %>% 
  rename("baseline" = "wt_conc") %>% 
  select(GEOID10, baseline)

load("./HIA Inputs/HIA_CF_BL_Winter_AllCO_pm_zcta_metrics.RData")
w_delta_pm <- zcta_list[[1]] %>% 
  rename("delta" = "wt_conc") %>% 
  select(GEOID10, delta)

winter_pm <- left_join(w_baseline_pm, w_delta_pm, by = "GEOID10") %>% 
  mutate(ratio = abs(delta) / baseline)
summary(winter_pm$ratio)

#' Winter O3
load("./HIA Inputs/Baseline_Winter_AllCO_o3_zcta_metrics.RData")
w_baseline_o3 <- zcta_list[[1]] %>% 
  rename("baseline" = "wt_conc") %>% 
  select(GEOID10, baseline)

load("./HIA Inputs/HIA_CF_BL_Winter_AllCO_o3_zcta_metrics.RData")
w_delta_o3 <- zcta_list[[1]] %>% 
  rename("delta" = "wt_conc") %>% 
  select(GEOID10, delta)

winter_o3 <- left_join(w_baseline_o3, w_delta_o3, by = "GEOID10") %>% 
  mutate(ratio = abs(delta) / baseline)
summary(winter_o3$ratio)

#' Summer PM
load("./HIA Inputs/Baseline_Summer_AllCO_pm_zcta_metrics.RData")
s_baseline_pm <- zcta_list[[1]] %>% 
  rename("baseline" = "wt_conc") %>% 
  select(GEOID10, baseline)

load("./HIA Inputs/HIA_CF_BL_Summer_AllCO_pm_zcta_metrics.RData")
s_delta_pm <- zcta_list[[1]] %>% 
  rename("delta" = "wt_conc") %>% 
  select(GEOID10, delta)

summer_pm <- left_join(s_baseline_pm, s_delta_pm, by = "GEOID10") %>% 
  mutate(ratio = abs(delta) / baseline)
summary(summer_pm$ratio)

#' Summer O3
load("./HIA Inputs/Baseline_Summer_AllCO_o3_zcta_metrics.RData")
s_baseline_o3 <- zcta_list[[1]] %>% 
  rename("baseline" = "wt_conc") %>% 
  select(GEOID10, baseline)

load("./HIA Inputs/HIA_CF_BL_Summer_AllCO_o3_zcta_metrics.RData")
s_delta_o3 <- zcta_list[[1]] %>% 
  rename("delta" = "wt_conc") %>% 
  select(GEOID10, delta)

summer_o3 <- left_join(s_baseline_o3, s_delta_o3, by = "GEOID10") %>% 
  mutate(ratio = abs(delta) / baseline)
summary(summer_o3$ratio)

#' -----------------------------------------------------------------------------
#' Map ratios for each season
#' -----------------------------------------------------------------------------

windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

map_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 10, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.major = element_line(colour = "transparent"),
  panel.grid.minor = element_line(colour = "transparent"),
  panel.border = element_blank(),
  panel.background = element_blank(), 
  axis.ticks = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_rect(fill = NA),
  legend.background=element_blank()
)

#' Get list of ZCTAs 
load("./HIA Inputs/Baseline_Winter_AllCO_zcta.RData")
zcta_sf <- st_as_sf(zcta) %>% 
  st_zm(drop = T)
head(zcta_sf)

load("./Data/Spatial Data/power_plants.RData")
pp <- st_transform(pp, crs = ll_wgs84)

winter_pm_zcta <- left_join(zcta_sf, winter_pm, by = "GEOID10")
winter_o3_zcta <- left_join(zcta_sf, winter_o3, by = "GEOID10")
summer_pm_zcta <- left_join(zcta_sf, summer_pm, by = "GEOID10")
summer_o3_zcta <- left_join(zcta_sf, summer_o3, by = "GEOID10")

winter_pm_ratio <- ggplot() +
  geom_sf(data = winter_pm_zcta, aes(fill = ratio), inherit.aes = F,
          color = NA) +
  geom_text_repel(data = pp, aes(label = id, geometry = geometry),
                  stat = "sf_coordinates", direction = "x", nudge_x = -2,
                  colour = "white", segment.colour = "white", segment.size = 0.3,
                  inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Delta-PM/Baseline PM") +
  map_theme +
  theme(legend.position = "right",
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  north(x.min = -109, x.max = -102,
        y.min =  37, y.max = 41,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -102.75, y = 36.75)) +
  scalebar(x.min = -109, x.max = -102,
           y.min =  37, y.max = 41,
           dist = 100, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -102.6, y = 36.3), st.dist = 0.04)
winter_pm_ratio

winter_o3_ratio <- ggplot() +
  geom_sf(data = winter_o3_zcta, aes(fill = ratio), inherit.aes = F,
          color = NA) +
  geom_text_repel(data = pp, aes(label = id, geometry = geometry),
                  stat = "sf_coordinates", direction = "x", nudge_x = -2,
                  colour = "white", segment.colour = "white", segment.size = 0.3,
                  inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Delta-O3/Baseline O3") +
  map_theme +
  theme(legend.position = "right",
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  north(x.min = -109, x.max = -102,
        y.min =  37, y.max = 41,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -102.75, y = 36.75)) +
  scalebar(x.min = -109, x.max = -102,
           y.min =  37, y.max = 41,
           dist = 100, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -102.6, y = 36.3), st.dist = 0.04)
winter_o3_ratio

summer_pm_ratio <- ggplot() +
  geom_sf(data = summer_pm_zcta, aes(fill = ratio), inherit.aes = F,
          color = NA) +
  geom_text_repel(data = pp, aes(label = id, geometry = geometry),
                  stat = "sf_coordinates", direction = "x", nudge_x = -2,
                  colour = "white", segment.colour = "white", segment.size = 0.3,
                  inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Delta-PM/Baseline PM") +
  map_theme +
  theme(legend.position = "right",
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  north(x.min = -109, x.max = -102,
        y.min =  37, y.max = 41,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -102.75, y = 36.75)) +
  scalebar(x.min = -109, x.max = -102,
           y.min =  37, y.max = 41,
           dist = 100, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -102.6, y = 36.3), st.dist = 0.04)
summer_pm_ratio

summer_o3_ratio <- ggplot() +
  geom_sf(data = summer_o3_zcta, aes(fill = ratio), inherit.aes = F,
          color = NA) +
  geom_text_repel(data = pp, aes(label = id, geometry = geometry),
                  stat = "sf_coordinates", direction = "x", nudge_x = -2,
                  colour = "white", segment.colour = "white", segment.size = 0.3,
                  inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Delta-O3/Baseline O3") +
  map_theme +
  theme(legend.position = "right",
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  north(x.min = -109, x.max = -102,
        y.min =  37, y.max = 41,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -102.75, y = 36.75)) +
  scalebar(x.min = -109, x.max = -102,
           y.min =  37, y.max = 41,
           dist = 100, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -102.6, y = 36.3), st.dist = 0.04)
summer_o3_ratio

ratio_plots <- ggarrange(
  annotate_figure(ggarrange(winter_pm_ratio, summer_pm_ratio, 
                            labels = c("A: Winter PM2.5", "B: Summer PM2.5"), 
                            ncol = 2, nrow = 1)),
  annotate_figure(ggarrange(winter_o3_ratio, summer_o3_ratio, 
                            labels = c("C: Winter O3", "D: Summer O3"), 
                            ncol = 2, nrow = 1)),
  ncol = 1, nrow = 2)
ratio_plots

ggsave(ratio_plots,
       filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Ratio_Plots.jpeg", 
       device = "jpeg", dpi=500, units = "in", height = 10, width = 10)


















