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
#' Read in the CMAQ data and get the receptor coordinates
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

#' Create receptor data frame with ID 
cmaq_df <- data.frame(lon = as.vector(cmaq_lon),
                      lat = as.vector(cmaq_lat))
cmaq_df$id <- seq(1:nrow(cmaq_df))

#' plot grid cell centroids 
#' The grid of points is "irregular" due to the geographic coordinate system 
ggplot(cmaq_df) +
  geom_point(aes(x=lon, y=lat)) +
  simple_theme
#head(cmaq_df)

#' Create a spatial points DF from the CMAQ coordinates
cmaq_p <- cmaq_df
coordinates(cmaq_p) <- c("lon", "lat")
proj4string(cmaq_p) <- CRS(ll_wgs84)
cmaq_p
cmaq_p@data <- cmaq_df

#' get the extent of the grid
cmaq_e <- extent(cmaq_p)
cmaq_e

#' -----------------------------------------------------------------------------
#' Create data frames of hourly data at each receptor for analysis
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
#' Convert O3 from ppm to ppb
df_list <- list("pm" = array_to_df(nc = cmaq_pm, coord_df = lon_lat),
                "o3" = array_to_df(nc = cmaq_o3*1000, coord_df = lon_lat))

rm(cmaq, cmaq_pm, cmaq_o3, lon_lat, cmaq_o3_fill, cmaq_pm_fill)

#' -----------------------------------------------------------------------------
#' Calculate annual and daily metrics for each pollutant
#' -----------------------------------------------------------------------------
date_seq <- seq.Date(from = start_date, length.out = n_days_pm, by="days")

for (i in 1:length(df_list)) {
  df <- df_list[[i]]
  df_name <- names(df_list)[i]
  
  #' Set up the data frame
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
  
  #' Summarizing HIA exposure metrics at each receptor
  df_pol <- df[,c("id", "day", "hour", "lon", "lat", "value")]
  
  #' (1) Annual mean-- mean of all data available
  ann_mean <- df_pol %>%
    group_by(id) %>%
    summarise(ann_mean = mean(value)) 
  
  ann_mean$day <- 1
  ann_mean <- merge(ann_mean, rec, by="id")  
  
  #' (2) Daily mean
  d24h_mean <- df_pol %>%
    group_by(id, day) %>%
    summarise(d24h_mean = mean(value)) 
  d24h_mean <- merge(d24h_mean, rec, by="id")
  
  #' (3) Daily 1 hour maximum concentration
  #' Not used in PM2.5 analysis, but calculating here anyway
  d1h_max <- df_pol %>%
    group_by(id, day) %>%
    summarise(d1h_max = max(value)) 
  d1h_max <- merge(d1h_max, rec, by="id")
  
  #' (4) Daily 8 hour max-- a little more involved
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
  
  exp_list <- list(ann_mean, d24h_mean, d1h_max, d8h_max)
  names(exp_list) <- c("ann_mean", "d24h_mean", "d1h_max", "d8h_max")
  
  save(exp_list,
       file=paste("./HIA Inputs/", pre, df_name, "_receptor_metrics.RData",
                  sep=""))
  
  rm(df_pol, df_wide, d24h_mean, ann_mean, d1h_max, d8h_max, exp_list,
     d8h_max_w, d8h_max_wt)
}

#' Clean up environment
rm(df, df_name, rec, temp, a, b, avg_list, col_ids, conc, date_seq,
   day_list, days, hours, i, j, k, l, max, n, name)

#' -----------------------------------------------------------------------------
#' Interpolate CMAQ concentrations to the 1 k populaton density grid and use
#' zonal statistics to get the average value for the ZCTAs weighted by area and
#' population density
#' 
#' Takes a bit of time, but gives decent estimates
#' 
#' 1) Read in the SEDAC population density grid
#'     Going to use 2010 since we'll be modeling 2011 as the baseline year and
#'     2015 might be too far out for comparability
#'     Cropped the GEOTiff in Arcmap prior to reading into R
#' 2) use the pop density raster to create an empty grid for kriging
#' 3) identify which ZCTAs are within the study area 
#'     Both overlapping and completly within
#' 4) Use zonal statistics to extract population density for each ZCTA. This 
#'     does not change from metric to metric
#' 5) Loop through each pollutant and day to:
#'     a) krige a smooth surface
#'     b) extract CMAQ concentrations using zonal statistics
#'     c) calculate population-weighted mean exposures at the ZCTA level
#'     d) save exposures to a data frame for the HIA
#' -----------------------------------------------------------------------------

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

#' Get ZCTAs that are covered by the CAMQ raster

#' Read in the ZCTA shapefile and make sure CRS matches
co_zcta <- readOGR(dsn = geo_data, layer = "CO_ZCTA_2014")
co_zcta <- spTransform(co_zcta, CRS=proj4string(pop_den_r_1k))

plot(co_zcta)
points(cmaq_p, col="red", pch=20, cex=0.5)

#' Subset ZCTA that are completely within the CMAQ domain
cmaq_bound <- gConvexHull(cmaq_p)
co_zcta$contains <- gContains(cmaq_bound, co_zcta, byid = T)

plot(co_zcta)
plot(cmaq_bound, add=T)

#' All ZCTAs with at least one point
zcta <- co_zcta[cmaq_bound,]

plot(zcta)
points(cmaq_p, col="red", pch=20, cex=0.5)

#' ZCTAs that are completely within the CMAQ domain
zcta_within <- zcta[which(zcta$contains == T),]

plot(zcta_within)
points(cmaq_p, col="red", pch=20, cex=0.5)

#' Save the spatial objects
save(zcta, zcta_within, file="./HIA Inputs/zcta.RData")

#' Extract population density in each ZCTA using extract() (raster package)
#' Save this data frame-- going to be the same for each metric 
#' 
#' NOTE: extract(weights = T) means that the extract function estimates the 
#' fraction of the ZCTA area occupied by the grid cell. 

a1 <- Sys.time()
zcta_pop_l <- raster::extract(pop_den_r_1k, zcta, weights=T)
names(zcta_pop_l) <- zcta@data$GEOID10

undo_lists <- function(x) {
  df <- data.frame()
  for (i in 1:length(x)) {
    name <- names(x)[i]
    data <- as.data.frame(x[[i]])
    data$id <- names(x)[[i]]
    df <- rbind(df, data)
    rm(data)
  }
  return(df)
}

zcta_pop <- undo_lists(zcta_pop_l)
colnames(zcta_pop) <- c("pop_den", "pop_area_wt", "pop_GEOID10")

#' Time to extract ZCTA populations:
Sys.time() - a1

#' #' check to see if area weights within each ZCTA sum to 1
wt_check <- zcta_pop %>%
  group_by(pop_GEOID10) %>%
  summarise(wt_zcta = sum(pop_area_wt))
summary(wt_check$wt_zcta)

#' Loop through pollutants, metrics, and days to estimate pop-weighted
#' average exposures at the ZCTA level
pol_names <- c("pm", "o3")

#' loop through each pollutant and metric
a2 <- Sys.time()

for (i in 1:length(pol_names)) {

  #' read in data
  load(paste("./HIA Inputs/", pre, pol_names[i], "_receptor_metrics.RData", 
             sep=""))
  
  metrics <- names(exp_list)
  if(pol_names[[i]] == "pm") {
    metrics <- names(exp_list)[1:2]
  } 
  
  metrics
  
  zcta_list <- list()
  
  for (j in 1:length(metrics)) {
    
    #' extract data frame from list
    test_df <- exp_list[[j]]
    
    #' create a spatial object from the metric data frame
    test_p <- test_df
    coordinates(test_p) <- c("lon", "lat")
    crs(test_p) <- CRS(ll_wgs84)
    test_p@data <- test_df
    
    #' list of days to loop through
    days <- sort(as.integer(unique(test_p$day)))
    met_df <- data.frame()
    
    #' krige cmaq points to the grid
    for (k in 1:length(days)) {
      
      print(paste("Pollutant ", i, " of ", length(pol_names), 
                  "; Metric ", j, " of ", length(metrics), 
                  "; Day ", k, " of ", length(days), sep=""))
      
      #' subset to daily metric
      test_p_2 <- test_p[which(test_p$day == days[k]),]
      
      #' If there are missing values, skip this iteration
      if (anyNA(as.data.frame(test_p_2[,metrics[j]]))) next 
      
      #' Create a smooth surface using ordinary kriging
      cmaq_vgm <- variogram(get(metrics[j]) ~ 1, test_p_2) #'generate the semivariogram
      plot(cmaq_vgm)
      
      #show.vgms() 
      
      #' Fitting the semivariogram
      cmaq_fit <- fit.variogram(cmaq_vgm, 
                                model=vgm(c("Sph", "Exp", "Gau", "Wav", "Exc", 
                                            "Ste", "Bes", "Mat", "Cir", "Pen"))) 
      cmaq_fit 
      
      plot(cmaq_vgm, cmaq_fit)
      
      #' Ordinary kriging using the fitted semivariogram
      #' Use 24 nearest points (important for ozone)
      cmaq_ok <- krige(get(metrics[j]) ~ 1, test_p_2, cmaq_g, cmaq_fit,
                       nmax=24)
      
      spplot(cmaq_ok, "var1.pred",
             main = paste("pol= ", pol_names[i], ", met= ", metrics[j],
                          ", day= ", days[k], sep=""))
      
      #' Turn grid back into raster for zonal statistics
      cmaq_r <- raster(cmaq_ok)
      #cmaq_r
      
      #' Zonal statistics
      #' Extract CMAQ concentrations and calculate weighted means and SD 
      #' (Hmisc) 
      zcta_conc_l <- raster::extract(cmaq_r, zcta, weights=T)
      names(zcta_conc_l) <- zcta@data$GEOID10
      
      #' Undo lists and combine
      zcta_conc <- undo_lists(zcta_conc_l)
      colnames(zcta_conc) <- c("conc", "conc_area_wt", "GEOID10")
      
      # Combine the ZCTA concentrations with the population density dataframe
      zcta_conc <- cbind(zcta_conc, zcta_pop)
      
      #' Calculate weighted averages at the ZCTA level
      #' New weight-- population density * the fraction of the pop_den cell in the 
      #' polygon *  the fraction of the cmaq cell in the polygon
      zcta_cmaq <- zcta_conc %>%
        mutate(wt_prod = conc_area_wt * pop_area_wt * pop_den) %>%
        group_by(GEOID10) %>%
        summarise(wt_conc = wtd.mean(x = conc, weights = wt_prod),
                  wt_conc_sd = sqrt(wtd.var(x = conc, weights = wt_prod)))
      #head(zcta_cmaq)
      #summary(zcta_cmaq)
      
      #' Plot exposure concentration
      #zcta_1 <- merge(zcta, zcta_cmaq, by="GEOID10")
      #zcta_within_1 <- merge(zcta_within, zcta_cmaq, by="GEOID10")
      #spplot(zcta_1, "wt_conc",
      #       main="Predicted monthly average PM2.5\n(ZCTA level using zonal statistics)")
      
      #spplot(zcta_within_1, "wt_conc",
      #       main="Predicted monthly average PM2.5\n(ZCTAs completely within the modeling domain)")
      
      #' add indicators for pollutant, metric, and day
      zcta_cmaq$pol <- pol_names[i]
      zcta_cmaq$metric <- metrics[j]
      zcta_cmaq$day <- days[k]
      
      met_df <- rbind(met_df, zcta_cmaq)
      rm(zcta_conc_l, zcta_conc, zcta_cmaq)
    }
  zcta_list[[j]] <- met_df
  names(zcta_list)[length(zcta_list)] <- metrics[j]
  
  rm(met_df)
  }
  save(zcta_list,
       file=paste("./HIA Inputs/", pre, pol_names[i], "_zcta_metrics.RData",
                  sep=""))
  rm(zcta_list)
}

#' Time to summarize ZCTA exposures:
Sys.time() - a2