#' -----------------------------------------------------------------------------
#' Project: American Lung Association HIA
#' Date created: March 21, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' This script generates population-weighted exposures at the ZCTA level
#' for the American Lung Association HIA project.
#' Much of this code adapted from prior work by Ryan Gan with some help from
#' other spatial analysts on campus
#'     
#' Once we agree that this is the route we want to take, I'll loop through 
#' the daily metrics for each pollutant and then generate a final data set for
#' the HIA     
#' -----------------------------------------------------------------------------

library(sp)
library(gstat)
library(rgdal)
library(raster)
library(spatialEco)
library(ggplot2)
library(ggmap)
library(rgeos)
library(maptools)
library(ggthemes)
library(ncdf4)
library(plyr)

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

start_time <- Sys.time()

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

library(dplyr)
library(Hmisc)

#' Extract population density in each ZCTA using extract() (raster package)
#' Save this data frame-- going to be the same for each metric in the final
#' loop
#' 
#' NOTE: extract(weights = T) means that the extract function estimates the 
#' fraction of the ZCTA area occupied by the grid cell. 
#' The new weighted mean weight is population density * area weight

zcta_pop_l <- extract(pop_den_r_1k, zcta, weights=T)
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

#' check to see if area weights within each ZCTA sum to 1
wt_check <- zcta_pop %>%
  group_by(pop_GEOID10) %>%
  summarise(wt_zcta = sum(pop_area_wt))
summary(wt_check$wt_zcta)

#' Extract CMAQ concentrations and calculate weighted means and SD (Hmisc) 
#' cmaq_r is the smoothed surface
zcta_conc_l <- extract(cmaq_r, zcta, weights=T)
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
head(zcta_cmaq)
summary(zcta_cmaq)

#' Plot exposure concentration
zcta_1 <- merge(zcta, zcta_cmaq, by="GEOID10")
zcta_within_1 <- merge(zcta_within, zcta_cmaq, by="GEOID10")

spplot(cmaq_ok, "var1.pred", 
       main="Predicted monthly average PM2.5 (Ordinary Kriging)")

spplot(zcta_1, "wt_conc",
       main="Predicted monthly average PM2.5\n(ZCTA level using zonal statistics)")

spplot(zcta_within_1, "wt_conc",
       main="Predicted monthly average PM2.5\n(ZCTAs completely within the modeling domain)")

#' How long does this code take?
#' Remember- a lot of this only needs to be done once 
#' (e.g., metrics, pop-density)
stop_time <- Sys.time()
stop_time - start_time

#' -----------------------------------------------------------------------------
#' How to these concentrations compare with the alternative methods of averaging
#' CMAQ points within the ZCTA?
#' 
#' AM ONE: Kriging to census block centroids, then population weighted average
#' at the ZCTA level (recommended by Ana Rappold)
#'    1) ID census blocks in the study area
#'    2) krige to these locations
#'    3) use block populations to population-weight ZCTA averages
#' 
#' AM TWO: point in polygon approach (partially based on Ryan's code)
#'    1) Regrid population density to the CMAQ grid
#'    2) extract population density for each CMAQ point
#'    3) population-weight exposures at the ZCTA level based on points-in-poly
#' -----------------------------------------------------------------------------

#' ALTERNATIVE METHOD 1: KRIGE TO CENSUS BLOCKS AND POPULATON WEIGHT TO ZCTA
#' Read in census block shapefile (from 2010)
co_blocks <- readOGR(dsn = geo_data, layer="tabblock2010_08_pophu")
cmaq_bound <- spTransform(cmaq_bound, crs(co_blocks))

#' Subset to blocks that touch the study domain
#' Make sure cordinate systems match
blocks <- co_blocks[cmaq_bound,]
blocks <- spTransform(blocks, crs(test_p))
blocks$BLOCKID10 <- as.character(blocks$BLOCKID10)

rm(co_blocks)

# plot(blocks)
# points(test_p, pch=20, cex=0.5, col="red")

#' Block centroids for krigings
block_cent <- gCentroid(blocks, byid=T, id=as.character(blocks$BLOCKID10))
plot(block_cent)
points(test_p, pch=20, cex=0.5, col="red")

#' Assign ZCTAs to census block centroids
block_cent_zcta <- sp::over(block_cent, zcta)
block_zctas <- data.frame(BLOCKID10 = as.character(rownames(block_cent_zcta)),
                          ZCTA5CE10 = as.character(block_cent_zcta$ZCTA5CE10))

#' Ordinary kriging to the census block centroids
cmaq_vgm_2 <- variogram(ann_mean ~ 1, test_p) #'generate the semivariogram
plot(cmaq_vgm_2)

show.vgms() 

#' Testing Exp, Sph, Gau, Wav, and Per models
cmaq_fit_2 <- fit.variogram(cmaq_vgm_2, 
                            model=vgm(c("Sph", "Exp", "Gau", "Wav", "Per"))) 
cmaq_fit_2 

plot(cmaq_vgm_2, cmaq_fit_2)

cmaq_ok_2 <- krige(ann_mean ~ 1, test_p, block_cent, cmaq_fit)
cmaq_ok_2$BLOCKID10 <- as.character(blocks@data$BLOCKID10)

#' Merge OK results with blocks and then aggregate to ZCTA
blocks <- merge(blocks, cmaq_ok_2, by="BLOCKID10")
blocks <- merge(blocks, block_zctas, by="BLOCKID10")
blocks$GEOID10 <- blocks$ZCTA5CE10

#' Based on the OK plot, we see the same high concentrations in the
#' "urban" centers of Colorado Springs and Pueblo (max is 20.6 ug/m3)
spplot(cmaq_ok_2, "var1.pred", 
       main="Predicted monthly average PM2.5 at census blocks (Ordinary Kriging)")

# spplot(blocks, "var1.pred", 
#        main="Predicted monthly average PM2.5 at census blocks (Ordinary Kriging)")

#' Population-weighted average at the ZCTA level
zcta_cmaq_2 <- as.data.frame(blocks) %>%
  group_by(GEOID10) %>%
  filter(sum(POP10) > 0) %>% #' drop ZCTAs where population == 0 (should be 1)
  summarise(wt_conc = wtd.mean(x = var1.pred, weights = POP10),
            wt_conc_sd = sqrt(wtd.var(x = var1.pred, weights = POP10)))
head(zcta_cmaq_2)
summary(zcta_cmaq_2)

#' Plot exposure concentrations
#' Three of the population ZCTAs don't contain a receptor
zcta_2 <- merge(zcta, zcta_cmaq_2, by="GEOID10")
zcta_within_2 <- merge(zcta_within, zcta_cmaq_2, by="GEOID10")

spplot(zcta_2, "wt_conc",
       main="Predicted monthly average PM2.5\n(ZCTA level using block pop weights)")

spplot(zcta_within_2, "wt_conc",
       main="Predicted monthly average PM2.5\n(ZCTAs completely within the modeling domain)")

#' ALTERNATIVE METHOD 2: AVERAGE OF POINTS IN POLYGONS
#' Start with the 1k population density grid
plot(pop_den_r_1k)
points(cmaq_p, pch=20, cex=0.5)

#' Convert the raster to points
pop_den_df <- as.data.frame(rasterToPoints(pop_den_r_1k))
names(pop_den_df) <- c("lon", "lat", "pop_denisty")
pop_den_p <- pop_den_df
coordinates(pop_den_p) <- c("lon", "lat")
proj4string(pop_den_p) <- proj4string(pop_den_r_1k)

plot(pop_den_p, pch=20, cex=0.5)

#' Create an empty raster with the same dimensions and CRS as the CMAQ points
cmaq_e <- extent(cmaq_p)
cmaq_r3 <- raster(cmaq_e, crs=crs(cmaq_p),
                  nrows = nrow(cmaq_lat), ncols = ncol(cmaq_lat))
cmaq_r3

#' Rasterize the SEDAC data to this empty grid using average of points in cell
#' Range for population density is much smaller when averaging to the ~4 km grid
pop_den_r_4k <- rasterize(pop_den_df[,1:2], cmaq_r3, pop_den_df[,3], fun=mean)

pop_den_r_1k
pop_den_r_4k

plot(pop_den_r_4k)
points(cmaq_p, pch=20, cex=0.5)

#' Extract population density at each CMAQ receptor
cmaq_pop <- cmaq_p
cmaq_pop<- extract(pop_den_r_4k, cmaq_p, sp=T)
names(cmaq_pop)[ncol(cmaq_pop)] <- "pop_density"

#' plot CMAQ population density
ggplot(as.data.frame(cmaq_pop), aes(x=lon, y=lat, col = pop_density)) +
  geom_point() +
  simple_theme

#' Merge annual mean with population density points
cmaq_exp_3 <- merge(cmaq_pop[,c("lon", "lat", "pop_density")], 
                    test_p, by=c("lon", "lat"))

#' ID which receptors are in each ZCTA
zcta_exp_3 <- as.data.frame(point.in.poly(cmaq_exp_3, zcta))

#' Calculate weighted average in each ZCTA
zcta_cmaq_3 <- zcta_exp_3 %>%
  group_by(GEOID10) %>%
  summarise(wt_conc = wtd.mean(x = ann_mean, weights = pop_density),
            wt_conc_sd = sqrt(wtd.var(x = ann_mean, weights = pop_density)))
head(zcta_cmaq_3)
summary(zcta_cmaq_3)

#' Plot exposure concentrations
#' Three of the population ZCTAs don't contain a receptor
zcta_3 <- merge(zcta, zcta_cmaq_3, by="GEOID10")
zcta_within_3 <- merge(zcta_within, zcta_cmaq_3, by="GEOID10")

spplot(zcta_3, "wt_conc",
       main="Predicted monthly average PM2.5\n(ZCTA level using point-in-polygon)")

spplot(zcta_within_3, "wt_conc",
       main="Predicted monthly average PM2.5\n(ZCTAs completely within the modeling domain)")


#' How do the alternative approachs compare with the zonal statistics approach?
summary(zcta_cmaq)
summary(zcta_cmaq_2)
summary(zcta_cmaq_3)

compare <- zcta_cmaq[,c("GEOID10", "wt_conc")]
compare <- merge(compare, zcta_cmaq_2[,c("GEOID10", "wt_conc")], by="GEOID10", all.x=T)
compare <- merge(compare, zcta_cmaq_3[,c("GEOID10", "wt_conc")], by="GEOID10", all.x=T)

colnames(compare) <- c("GEOID10", "wt_conc_1", "wt_conc_2", "wt_conc_3")

compare$pd_1v2 <- (compare$wt_conc_1 - compare$wt_conc_2) / compare$wt_conc_1 * 100
compare$pd_1v3 <- (compare$wt_conc_1 - compare$wt_conc_3) / compare$wt_conc_3 * 100

compare
summary(compare)

#' Coefficient of variation: Zonal statistics method
sd(compare$wt_conc_1, na.rm=TRUE)/ mean(compare$wt_conc_1, na.rm=TRUE)*100

#' Coefficient of variation: census block pop weighting
sd(compare$wt_conc_2, na.rm=TRUE)/ mean(compare$wt_conc_2, na.rm=TRUE)*100

#' Coefficient of variation: point-in-polygon
sd(compare$wt_conc_3, na.rm=TRUE)/ mean(compare$wt_conc_3, na.rm=TRUE)*100

#' Cumulative distributions
plot(ecdf(compare$wt_conc_1),
     main="Cumulative distribution: Zonal statistics")
plot(ecdf(compare$wt_conc_2),
     main="Cumulative distribution: Block-pop weighting")
plot(ecdf(compare$wt_conc_3),
     main="Cumulative distribution: Point-in-polygon")


# Where are the biggest differences?
zcta_comp <- merge(zcta, compare, by="GEOID10")

spplot(zcta_comp, "pd_1v2",
       main="Percent difference: zonal statistics vs. block-pop weighting")

spplot(zcta_comp, "pd_1v3",
       main="Percent difference: zonal statistics vs. point-in-polygon")







