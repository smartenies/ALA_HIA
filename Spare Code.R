#' # -----------------------------------------------------------------------------
#' # Rasterize ZCTAs in Colorado
#' #     - Assigns the ZCTA identifier to the population density grid
#' #     - Used to generate population-weighted exposure metrics for each ZCTA
#' #     and day
#' # -----------------------------------------------------------------------------
#' # 
#' # Read in the population density geotiff
#' #' This was trimmed in ArcMap before working in R
#' #' Resolution is ~1 km (0.0083333 deg)
#' pop_den_r <- raster("./Data/SEDAC Data/2010-ColoradoPopDensity.tif")
#' pop_den_r
#' 
#' #' Read in the ZCTA shapefile
#' load("./Data/Spatial Data/co_zcta_latlong.RData")
#' co_zcta_s <- spTransform(co_zcta, CRS=ll_wgs84) #' match CRS
#' co_zcta_s@data$ALAND10 <- NULL
#' rm(co_zcta)
#' 
#' 
#' #' Visualziing the Colorado data
#' #' Can see Denver, Fort Collins, CO Springs, and Pueblo centers
#' plot(pop_den_r)
#' plot(co_zcta_s, add=T)
#' 
#' #' add a numeric identifier
#' co_zcta_s$GEOID_NUM <- as.numeric(co_zcta_s$GEOID10)
#' link <- co_zcta_s@data[,c("GEOID10", "GEOID_NUM")]
#' 
#' #' rasterize CO ZCTA using the population density raster
#' #' cell value is the ZCTA number
#' 
#' ext <- extent(pop_den_r)
#' co_zcta_r <- raster(ext, res=res(pop_den_r))
#' crs(co_zcta_r) <- crs(ll_wgs84)
#' co_zcta_r <- rasterize(co_zcta_s, co_zcta_r, field='GEOID_NUM')
#' co_zcta_r
#' rm(ext)
#' 
#' #' Visualize the rasterized shapefile
#' #' See holes for the military base and Denver International
#' plot(co_zcta_r,colNA="black")
#' plot(co_zcta_s, col=NA, border="blue", add=T)
#' 
#' #' Check out how well the rasterization worked in GIS
#' sedac_data <- "./Data/SEDAC Data"
#' writeOGR(co_zcta_s, dsn = sedac_data, layer = "co_zcta_shapefile",
#'          driver="ESRI Shapefile", overwrite_layer = T)
#' writeRaster(co_zcta_r, filename=paste(sedac_data,"/co_zcta_ras.tif",sep=""),
#'             format="GTiff", overwrite=TRUE)
#' 
#' #' Create a spatial grid data frame with both co_zcta ID and population denisty
#' #' Will be used later to create a weighting matrix
#' pop_den_g <- as(pop_den_r, 'SpatialGridDataFrame')
#' 
#' co_zcta_g <- as(co_zcta_r, 'SpatialGridDataFrame')
#' co_zcta_g <- cbind(co_zcta_g, pop_den_g)
#' names(co_zcta_g@data) <- c("GEOID10", "pop_den")
#' summary(co_zcta_g)
#' 
#' plot(co_zcta_g["pop_den"])
#' plot(co_zcta_g["GEOID10"])
#' 
#' save(co_zcta_g, co_zcta_r, pop_den_r, pop_den_g, link,
#'      file="./Data/SEDAC Data/co_zcta grid.RData")
#' 
#' #' Clear up some space
#' rm(co_zcta_g, co_zcta_r, co_zcta_s,
#'    pop_den_g, pop_den_r, link)
#' 
#' #' -----------------------------------------------------------------------------
#' #' Use IDW to interpolate to SEDAC/ZCTA grid centroids
#' #' -----------------------------------------------------------------------------
#' 
#' #' Read in the ZCTA and population data grids
#' load("./Data/SEDAC Data/co_ZCTA grid.RData")
#' 
#' #' Create a spatial object from the CAMQ coordinates
#' cmaq_p <- cmaq_df
#' coordinates(cmaq_p) <- c("x", "y")
#' proj4string(cmaq_p) <- CRS(ll_wgs84)
#' summary(cmaq_p)
#' 
#' cmaq_e <- extent(cmaq_p)
#' cmaq_e
#' 
#' #' Trim the density raster using the CMAQ extent
#' pop_den_r <- crop(pop_den_r, cmaq_e)
#' pop_den_r
#' 
#' plot(pop_den_r)
#' points(cmaq_p, add=T)
#' 
#' #' Spatial points for SEDAC raster grids
#' pop_den_p <- as.data.frame(rasterToPoints(pop_den_r))
#' names(pop_den_p) <- c("x", "y", "pop_den")
#' coordinates(pop_den_p) <- c("x", "y")
#' proj4string(pop_den_p) <- proj4string(pop_den_r)
#' summary(pop_den_p)
#' 
#' #' Visualize the raster and poiints
#' plot(pop_den_r)
#' points(pop_den_p, col="red")
#' points(cmaq_p, col="blue")