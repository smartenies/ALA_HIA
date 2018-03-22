#' -----------------------------------------------------------------------------
#' Project: American Lung Association HIA
#' Date created: March 21, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' 
#' This script generates population weighted exposures at the ZCTA level
#' for the American Lung Association HIA project.
#' Much of this code adapted from prior work by Ryan Gan 
#'     
#'     Analysis steps:
#'     1) CMAq receptors are extracted from the netcdf file
#'     2) The 4d array in the NetCDF file is converted to a data frame 
#'     3) Annual and daily exposures are summarized for each CMAQ receptor
#'     
#'       
#'     4) The SEDAC population density data (~1 km grid) are regridded to 
#'        match the CMAQ outputs (which are in WGS84 and are not on a regular grid)
#'     5) Population density is extracted at each CAMQ receptor
#'     6) The object is saved to be used in the exposure assessment code
#' -----------------------------------------------------------------------------

library(sp)
library(gstat)
library(spatialEco)
library(rgdal)
library(raster)
library(ggplot2)
library(ggmap)
library(rgeos)
library(maptools)
library(ggthemes)
library(plyr)
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
mercator <- "+proj=merc +a=6370000.0 +b=6370000.0 +lat_ts=33 +lon_0=0"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Set up the analysis
#' Will be done in the "master" script later

cmaq_out <- "southern_colorado.nc"
pop_den_tif <- "2010-COloradoPopDensity.tif"
start_date <- as.Date("01/01/2011", format="%m/%d/%Y")
#' -----------------------------------------------------------------------------

#' -----------------------------------------------------------------------------
#' Read in the CMAQ data and get the coordinates
#' Ali has generated a netcdf file for one month--- will use to get the coords
#' Later, the CMAQ output will be specified in the Master script
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
head(cmaq_df)

#' Create a spatial points DF from the CMAQ coordinates
cmaq_p <- cmaq_df
coordinates(cmaq_p) <- c("lon", "lat")
proj4string(cmaq_p) <- CRS(ll_wgs84)
summary(cmaq_p)
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
df_list <- list("pm" = array_to_df(nc = cmaq_pm, coord_df = lon_lat),
                "o3" = array_to_df(nc = cmaq_o3, coord_df = lon_lat))

rm(cmaq_pm, cmaq_o3, lon_lat, cmaq_o3_fill, cmaq_pm_fill)

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
  ann_mean <- ddply(df_pol, .(id), summarize, 
                    ann_mean = mean(value))
  ann_mean <- merge(ann_mean, rec, by="id")
  
  #' (2) Daily mean
  d24h_mean <- ddply(df_pol, .(id, day), summarize, 
                     d24h_mean = mean(value))
  d24h_mean <- merge(d24h_mean, rec, by="id")
  
  #' (3) Daily 1 hour maximum concentration
  #' Not used in PM2.5 analysis, but calculating here anyway
  d1h_max <- ddply(df_pol, .(id, day), summarize, 
                   d1h_max = max(value))
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
       file=paste("./HIA Inputs/", df_name, "_exposure_metrics.RData", sep=""))
  
  rm(df_pol, df_wide, d24h_mean, ann_mean, d1h_max, d8h_max, exp_list,
     d8h_max_w, d8h_max_wt)
}

#' Clean up environment
rm(df, df_name, rec, temp, a, b, avg_list, col_ids, conc, date_seq,
   day_list, days, hours, i, j, k, l, max, n, n_days_o3, n_days_pm,
   name)


#' -----------------------------------------------------------------------------
#' Interpolate CMAQ concentrations to the 1 k populaton density grid and use
#' zonal statistics to get the average value for the ZCTAs
#' 
#' The original plan was to regrid the population density data to the CMAQ 
#' grid, but this left us with several ZCTAs with only one CMAQ receptor and
#' three with none
#' 
#' Sheryl and I discussed an alternative plan that would allow us to downscale
#' the CMAQ grid and take areal averages weighted by population density
#' 
#' Going to start here with just the annual average PM2.5-- in the future, this
#' will be done for each pollutant, metric, and day (loop? function? TBD)
#' -----------------------------------------------------------------------------

df_names <- c("pm", "o3")

#' annual average PM2.5 at each CMAQ receptor
load(file = paste("./HIA Inputs/", df_names[1], "_exposure_metrics.RData", 
                  sep=""))
test_df <- exp_list[[1]]

#' merge annual mean with receptor points
test_p <- merge(cmaq_p, test_df, by=c("lon", "lat"))

#' plot annual mean at cmaq receptors
ggplot(as.data.frame(test_p)) +
  geom_point(aes(x=lon.1, y=lat.1, color=ann_mean)) +
  simple_theme

#' -----------------------------------------------------------------------------
#' Read in the SEDAC population density grid
#' Going to use 2010 since we'll be modeling 2011 as the baseline year and
#' 2015 might be too far out for comparability
#' Cropped the GEOTiff in Arcmap prior to reading into R
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
points(test_p, pch=20, cex=0.5)

#' Create a grid for interpolation based on the population density grid
pop_den_e <- extent(pop_den_r_1k)
pop_den_res <- res(pop_den_r_1k)

cmaq_r <- raster(pop_den_e, crs=crs(pop_den_r_1k),
                 nrows = nrow(pop_den_r_1k), ncols = ncol(pop_den_r_1k))

cmaq_r

#' grid needed for interpolation
cmaq_g <- as(cmaq_r, "SpatialGrid")

#' First, trying IDW from CMAQ receptors to empty grid
cmaq_idw <- gstat::krige(ann_mean ~ 1, test_p, newdata=cmaq_g)

#' Based on the plot, we see some high concentration in the 
#' "urban" centers of Colorado Springs and Pueblo
#' Need to check on model validity-- need area monitoring data for Jan 2011
#' Concentrations up to 25 ug/m3 might be on the high side
#' Also... the IDW plot doesn't look as smooth as it could be
spplot(cmaq_idw, "var1.pred", 
       main="Predicted monthly average PM2.5 (IDW Interpol.)")

#' Second, going to give ordinary kriging a try
cmaq_vgm <- variogram(ann_mean ~ 1, test_p) #'generate the semivariogram
plot(cmaq_vgm)

show.vgms() 

#' Testing Exp, Sph, Gau, Wav, and Per models
cmaq_fit <- fit.variogram(cmaq_vgm, 
                          model=vgm(c("Sph", "Exp", "Gau", "Wav", "Per"))) 
cmaq_fit #' Gaussian it is

plot(cmaq_vgm, cmaq_fit)

cmaq_ok <- krige(ann_mean ~ 1, test_p, cmaq_g, cmaq_fit)

#' Based on the OK plot, we still see some high concentration in the 
#' "urban" centers of Colorado Springs and Pueblo (max is 20.6 ug/m3)
#' Going to proceed with OK for now, since it gives us a much smoother surface
#' In the future, will need to make sure this can be dynamic (multiple pols and
#' metrics need to be kriged)
spplot(cmaq_ok, "var1.pred", 
       main="Predicted monthly average PM2.5 (Ordinary Kriging)")

#' Turn grid back into raster for zonal statistics
cmaq_r <- raster(cmaq_ok)
cmaq_r

#' clean up the environment at bit
rm(cmaq_vgm, cmaq_e, cmaq_g, pop_den_e, pop_den_t, pop_den_res)

#' -----------------------------------------------------------------------------
#' Get ZCTAs that are covered by the CAMQ raster
#' -----------------------------------------------------------------------------

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

#' Does every ZCTA contain a CMAQ receptor?
rec_count <- colSums(gContains(zcta, cmaq_p, byid=T))
rec_count <- setNames(rec_count, zcta@data$GEOID10)
rec_count

#' How many ZCTAs are there?
nrow(zcta)

#' How many with at least one receptor?
length(rec_count[rec_count > 0])

#' How many with just one?
length(rec_count[rec_count == 1])

#' -----------------------------------------------------------------------------
#' Zonal statistics to get population-weighted average concentration
#' -----------------------------------------------------------------------------

#' Extract pop-weighted cmaq concentration and population density in each ZCTA 
#' using extract() in the raster package and weighted means and SD from
#' the Hmisc package

zcta_conc_l <- extract(cmaq_r, zcta, weights=T, normalizeWeights = T)
names(zcta_conc_l) <- zcta@data$GEOID10

zcta_pop_l <- extract(pop_den_r_1k, zcta, weights=T, normalizeWeights = T)
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

#' Undo lists and combine
zcta_conc <- undo_lists(zcta_conc_l)
colnames(zcta_conc) <- c("conc", "area_wt1", "GEOID10")

zcta_pop <- undo_lists(zcta_pop_l)
colnames(zcta_pop) <- c("pop_den", "area_wt2", "id2")

zcta_conc <- cbind(zcta_conc, zcta_pop)

#' New weight-- population density * the area of the cell within the polygon
zcta_conc$wt_pop_den <- ifelse(zcta_conc$area_wt1 != zcta_conc$area_wt2, NA,
                               zcta_conc$area_wt1 * zcta_conc$pop_den)

#' Calculate weighted averages at the ZCTA level
library(dplyr)
library(Hmisc)
zcta_cmaq <- zcta_conc %>%
  group_by(GEOID10) %>%
  summarise(wt_conc = wtd.mean(x = conc, weights = wt_pop_den),
            wt_conc_sd = sqrt(wtd.var(x = conc, weights = wt_pop_den)))

#' Plot concentrations
zcta <- merge(zcta, zcta_cmaq, by="GEOID10")
spplot(zcta, "wt_conc",
       main="Predicted monthly average PM2.5\n(ZCTA level using zonal statistics)")

zcta_within <- merge(zcta_within, zcta_cmaq, by="GEOID10")
spplot(zcta_within, "wt_conc",
       main="Predicted monthly average PM2.5\n(ZCTAs completely in the modeling domain)")

spplot(cmaq_ok, "var1.pred", 
       main="Predicted monthly average PM2.5 (Ordinary Kriging)")
















