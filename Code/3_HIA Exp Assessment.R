#' -----------------------------------------------------------------------------
#' Project: American Lung Association HIA
#' Date created: March 28, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' This script generates population-weighted exposures at the ZCTA level
#' for the American Lung Association HIA project.
#' 
#' Analytical steps:
#' 1) The specified netCDF file (CMAQ output) is read in
#' 2) The 4-d array is flattened to a data frame
#' 3) Daily exposure metrics at each receptor are calculated (by pollutant)
#' 4) For each pollutant, metric and day:
#'     a) a smooth surface is generated
#'     b) population-weighted zonal statistics are calculated
#'     c) ZCTA-level exposures are saved to a list of data frames
#' 5) The final lists are saved in order to run the HIA next
#' -----------------------------------------------------------------------------

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

#' -----------------------------------------------------------------------------
#' Calculate annual and daily metrics for each pollutant
#' -----------------------------------------------------------------------------
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
    
    #' (2) Daily mean
    print("Daily mean")
    print(df_name)
    d24h_mean <- df_pol %>%
      group_by(id, day) %>%
      summarise(d24h_mean = mean(value)) 
    d24h_mean <- merge(d24h_mean, rec, by="id")
    
    if (df_name == "o3") {
      
      #' (3) Daily 1 hour maximum concentration
      #' Not used in PM2.5 analysis
      print("Daily 1 h max")
      print(df_name)
      d1h_max <- df_pol %>%
        group_by(id, day) %>%
        summarise(d1h_max = max(value)) 
      d1h_max <- merge(d1h_max, rec, by="id")
      
      #' (4) Daily 8 hour max-- a little more involved
      #' See CFR Title 40 Vol 2 Part 50 Appendix P for details
      #' https://www.gpo.gov/fdsys/pkg/CFR-2017-title40-vol2/xml/CFR-2017-title40-vol2-part50.xml
      print("Daily 8 h max")
      print(df_name)
      
      rec_list <- unique(df_pol$id)
      temp_df <- data.frame()
      
      for (j in 1:length(rec_list)) {
        df1 <- filter(df_pol, id == rec_list[j]) %>%
          arrange(day, hour)
        
        day_list <- unique(df1$day)
        
        for (k in 1:(length(day_list)-1)) {
          days <- c(day_list[k], day_list[k+1])
          df2 <- filter(df1, day %in% days)
          
          if(df2$hour[1] != "00") next
          
          #should have 24 moving averages starting at 0:00 and ending at 23:00
          avg_list <- list()
          a <- 24 
          
          for (l in 1:a) {
            hours <- seq(from=l, to=l+7)
            df3 <- slice(df2, hours) 
            
            #' need at least 6 hourly concentrations (75%) to calculate mean
            avg_list[l] <- ifelse(sum(!is.na(df3$value)) >= 6, 
                                  mean(df3$value, na.rm=T), 
                                  NA)
          }
          
          rm(df3)
          b <- unlist(avg_list)
          
          #' only valid if there are >= 18 (75%) 8-hr means
          max <- ifelse(sum(!is.na(b)) >= 13, max(b, na.rm=T), NA)
          
          temp <- data.frame(id = rec_list[j],
                             day = days[1], 
                             d8h_max = max)
          temp_df <- rbind(temp_df, temp)
          rm(temp, df2)
        }
        rm(df1)
      }
      d8h_max <- merge(temp_df, rec, by="id")
      rm(temp_df)
    }
    
    if (df_name == "pm") {
      exp_list <- list(ann_mean, d24h_mean)
      names(exp_list) <- c("ann_mean", "d24h_mean")
    } else {
      exp_list <- list(ann_mean, d24h_mean, d1h_max, d8h_max)
      names(exp_list) <- c("ann_mean", "d24h_mean", "d1h_max", "d8h_max")
    }
    
    save(exp_list,
         file=paste("./HIA Inputs/", pre[s], df_name, "_receptor_metrics.RData",
                    sep=""))
    
    #' Clean up the loop
    rm(df_pol, d24h_mean, ann_mean, exp_list)  
  }
  
  #' Clean up environment
  rm(df, df_name, rec, a, b, avg_list, date_seq,
     day_list, days, hours, i, j, k, l, max)
}

#' Clean up environment
rm(baseline)

#' -----------------------------------------------------------------------------
#' Krige CMAQ concentrations to the 1 k populaton density grid and use
#' zonal statistics to get the average value for the ZCTAs weighted by area and
#' population density
#' 
#' Takes a bit of time, but gives decent estimates of population exposures
#' 
#' 1) Read in the SEDAC population density grid
#'     Going to use 2010 since we'll be modeling 2011 as the baseline year and
#'     2015 might be too far out for comparability
#'     Cropped the GEOTiff in Arcmap prior to reading into R
#' 2) use the pop density raster to create an empty grid for kriging
#' 3) identify which ZCTAs are within the study area 
#'     Both overlapping and completly within
#'     Start with ZCTAs in the Front Range airsheds
#'     Subset to those within the CMAQ domain
#' 4) Use zonal statistics to extract population density for each ZCTA. This 
#'     does not change from metric to metric
#' 5) Loop through each pollutant and day to:
#'     a) krige a smooth surface
#'     b) extract CMAQ concentrations using zonal statistics
#'     c) calculate population-weighted mean exposures at the ZCTA level
#'     d) save exposures to a data frame for the HIA
#' -----------------------------------------------------------------------------

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
      
      #' #' Run normally
      #' #' krige cmaq points to the grid
      #' for (k in 1:length(days)) {
      #' 
      #'   print(paste("Pollutant ", i, " of ", length(pol_names),
      #'               "; Metric ", j, " of ", length(metrics),
      #'               "; Day ", k, " of ", length(days), sep=""))
      #' 
      #'   #' subset to daily metric
      #'   test_p_2 <- test_p[which(test_p$day == days[k]),]
      #' 
      #'   #' Plot for report
      #'   # test_p_sf <- st_as_sf(test_p_2)
      #'   # ggplot() +
      #'   #   ggtitle("A: CMAQ Receptor Concentrations") +
      #'   #   geom_sf(data = test_p_sf, aes(color=ann_mean)) +
      #'   #   geom_sf(data = st_as_sf(zcta), fill=NA, color="grey50") +
      #'   #   scale_color_continuous(name = "Concentration",
      #'   #                          low="yellow", high="red") +
      #'   #   theme(legend.position = "bottom") +
      #'   #   simple_theme
      #'   # ggsave(filename = "./Maps/For Report/CMAQ Receptors.jpeg",
      #'   #        device = "jpeg", dpi = 300)
      #' 
      #'   #' scale exposures in order to avoid errors in kriging
      #'   #' small values don't do well because issues with truncation!
      #'   test_p_2@data[(metrics[j])] <- test_p_2@data[(metrics[j])] * exp_scale
      #' 
      #'   #' Create a smooth surface using ordinary kriging
      #'   #'generate the semivariogram
      #'   cmaq_vgm <- variogram(get(metrics[j]) ~ 1, test_p_2)
      #'   # plot(cmaq_vgm)
      #' 
      #'   #' Fitting the semivariogram
      #'   cmaq_fit <- fit.variogram(cmaq_vgm, model=vgm(all_models))
      #'   # plot(cmaq_vgm, cmaq_fit)
      #' 
      #'   #' Ordinary kriging using the fitted semivariogram
      #'   #' Use 24 nearest points (important for ozone)
      #'   cmaq_ok <- krige(get(metrics[j]) ~ 1, test_p_2, cmaq_g, cmaq_fit,
      #'                    nmax = 24)
      #' 
      #'   # spplot(cmaq_ok, "var1.pred",
      #'   #        main = paste("pol= ", pol_names[i], ", met= ", metrics[j],
      #'   #                     ", day= ", days[k], sep=""))
      #' 
      #'   #' Plot for report
      #'   # test_ok_sf <- st_as_sf(as(cmaq_ok, "SpatialPointsDataFrame"))
      #'   # ggplot() +
      #'   #   ggtitle("B: Kriged 1 km Point Estimates") +
      #'   #   geom_sf(data = test_ok_sf, aes(color=var1.pred / exp_scale)) +
      #'   #   geom_sf(data = st_as_sf(zcta), fill=NA, color="grey50") +
      #'   #   scale_color_continuous(name = "Concentration",
      #'   #                          low="yellow", high="red") +
      #'   #   theme(legend.position = "bottom") +
      #'   #   simple_theme
      #'   # ggsave(filename = "./Maps/For Report/Kriged Receptors.jpeg",
      #'   #        device = "jpeg", dpi = 300)
      #' 
      #'   #' Turn grid back into raster for zonal statistics
      #'   #' Want to make sure the grid cells are in the same order as the
      #'   #' population density raster when they get converted to a data frame
      #'   cmaq_r <- raster(cmaq_ok)
      #' 
      #'   #' Zonal statistics using weights from the population-density raster
      #'   cmaq_df <- as.data.frame(cmaq_r)
      #'   cmaq_df$cell <- rownames(cmaq_df)
      #' 
      #'   #' Check! Data frame for kriging should match population density
      #'   if (nrow(cmaq_df) != nrow(pop_den_df_1k)) stop()
      #' 
      #'   #' calculate weighted means and SD
      #'   #' Need to "unscale" exposure concentrations to get back to original units
      #'   #' See just before the  for details ()
      #'   #' (Hmisc)
      #'   zcta_conc <- merge(zcta_weights, cmaq_df, by="cell")
      #'   zcta_conc <- rename(zcta_conc, conc = var1.pred) %>%
      #'     arrange(id) %>%
      #'     mutate(conc = conc / exp_scale)
      #' 
      #'   #' Calculate weighted averages at the ZCTA level
      #'   #' New weight-- population density * the fraction of the polygon covered
      #'   #' by the cell
      #' 
      #'   zcta_cmaq <- zcta_conc %>%
      #'     mutate(wt_prod = pop_density * weight) %>%
      #'     group_by(id) %>%
      #'     summarise(wt_conc = wtd.mean(x = conc, weights = wt_prod),
      #'               wt_conc_sd = sqrt(wtd.var(x = conc, weights = wt_prod))) %>%
      #'     left_join(zcta_key, by="id")
      #'   head(zcta_cmaq)
      #'   summary(zcta_cmaq)
      #' 
      #'   #' Plot for report
      #'   # test_zcta_sf <- st_as_sf(zcta) %>%
      #'   #   select(GEOID10) %>%
      #'   #   left_join(zcta_cmaq, by="GEOID10")
      #'   # ggplot() +
      #'   #   ggtitle("C: Population-weighted ZCTA Concentrations") +
      #'   #   geom_sf(data = test_zcta_sf, aes(fill=wt_conc)) +
      #'   #   geom_sf(data = st_as_sf(zcta), fill=NA, color="grey50") +
      #'   #   scale_fill_continuous(name = "Concentration",
      #'   #                          low="yellow", high="red") +
      #'   #   theme(legend.position = "bottom") +
      #'   #   simple_theme
      #'   # ggsave(filename = "./Maps/For Report/ZCTA Concentrations.jpeg",
      #'   #        device = "jpeg", dpi = 300)
      #' 
      #'   #' Plot exposure concentration
      #'   #zcta_1 <- merge(zcta, zcta_cmaq, by="GEOID10")
      #'   #zcta_within_1 <- merge(zcta_within, zcta_cmaq, by="GEOID10")
      #'   #spplot(zcta_1, "wt_conc",
      #'   #       main="Predicted monthly average PM2.5\n(ZCTA level using zonal statistics)")
      #' 
      #'   #spplot(zcta_within_1, "wt_conc",
      #'   #       main="Predicted monthly average PM2.5\n(ZCTAs completely within the modeling domain)")
      #' 
      #'   #' add indicators for pollutant, metric, and day
      #'   zcta_cmaq$pol <- pol_names[i]
      #'   zcta_cmaq$metric <- metrics[j]
      #'   zcta_cmaq$day <- days[k]
      #' 
      #'   met_df <- rbind(met_df, zcta_cmaq)
      #'   rm(zcta_conc, zcta_cmaq)
      #' }
      
      zcta_list[[j]] <- met_df
      names(zcta_list)[length(zcta_list)] <- metrics[j]
      
      rm(met_df)
    }
    save(zcta_list,
         file=paste("./HIA Inputs/", pre[s], pol_names[i], "_zcta_metrics.RData",
                    sep="")) 
  }
}

#' Clean up environment
rm(zcta_list, cmaq_fit, cmaq_vgm, test_df, wt_check, cmaq_bound,
   cmaq_g, cmaq_e, cmaq_p, cmaq_r, co_zcta, i, 
   pop_den_e, pop_den_r_1k, pop_den_res, pop_den_t)