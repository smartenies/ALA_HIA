# ------------------------------------------------------------------------------
# Title: Colorado Population Density 2015
# Author: Ryan Gan
# Date Created: 2017-12-14
# ------------------------------------------------------------------------------

# load libraries ----
library(raster)
library(sf)
library(tidyverse)

# read in 2015 population density geotiff as a raster -----
path <- paste0("./Data/gpw-v4-population-density-2015/",
  "gpw-v4-population-density_2015.tif")

# create raster object
popden_15 <- raster(path) 

# extract coord ref system from raster
wgs84 <- crs(popden_15)@projargs

# read in colorado county simple feature ----
county_path <- paste0("./Data/colorado_county")
# read colorado county simple feature
county_sf <- st_read(dsn=county_path, layer = "colorado_county") %>% 
  # make fips variable
  mutate(FIPS = paste0(STATEFP, COUNTYFP)) %>% 
  # assign wgs84
  st_transform(wgs84)
# checking simple features plot
ggplot(county_sf) + geom_sf() + theme_minimal()

# extract spatial extent of simple features
colorado_e <- extent(as.numeric(st_bbox(county_sf)[c(1,3,2,4)]))

# crop raster, globe to colorado based on the extent of sf bbox
colorado_popden_15 <- crop(popden_15, colorado_e)

# convert raster to polygon, then sf object (easier to map with ggplot)
co_pop_sf <- st_as_sf(rasterToPolygons(colorado_popden_15))
# rename long population density variable name
co_pop_sf <- rename(co_pop_sf, popden = gpw.v4.population.density_2015)

# ggplot map of population density overlayed on counties sf file
plot <- ggplot(county_sf) +
  geom_sf(color = "#ff0084", fill = "black") +
  theme_minimal()

plot

# density plot taking too long with geom_sf; going to subset to study counties
ala_study_sf <- county_sf %>% 
  filter(NAME %in% c("Teller", "El Paso", "Custer", "Huerfano", 
                     "Pueblo", "Las Animas"))

# subset density grid to the study region
pop_ala <- co_pop_sf[ala_study_sf,]

plot(pop_ala)

# still taking too long; subset to el paso and pueblo
el_pueblo <- ala_study_sf %>% 
  filter(NAME %in% c("El Paso", "Pueblo"))

# subset population of these counties
el_pueblo_pop <- pop_ala[el_pueblo,] %>% 
  filter(popden > 10)  # get rid of cells with 0 people
  
  
summary(el_pueblo_pop)

# lat/lon of power plants ----
plant_id <- c("pueblo", "colo_springs")
lat <- c(38.2081, 38.8244)
lon <- c(-104.5747, -104.8331)

plant_locations <- tibble(plant_id, lat, lon) 

# read in shapefile of roads
road_path <- paste0("./Data/tl_2013_08_prisecroads")
# read colorado county simple feature
roads_sf <- st_read(dsn=road_path, layer = "tl_2013_08_prisecroads") %>% 
  st_transform(wgs84)

# subset roads to el paso and pueblo county
el_pueblo_roads <- st_intersection(roads_sf, el_pueblo) %>% 
  # remove points created during intersection
  filter(st_geometry_type(el_pueblo_roads)!="POINT")


# plot study area
plot <- ggplot(el_pueblo) + 
  geom_sf(color = "#6441a5", fill = "#2a0845") +
  geom_sf(data=el_pueblo_pop, aes(fill = popden), color = NA) +
      scale_fill_gradient(name="Population Density", 
                          low="#89fffd", high="#ef32d9") +
  geom_sf(data=el_pueblo_roads, color="white", alpha = 0.8) +
  geom_point(data=plant_locations, aes(x=lon,y=lat), color = "red") +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(panel.background = element_rect(fill="white", color = NULL),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "black")) 

plot
# save plot
ggsave(filename = "./population_density.pdf", plot, 
       width=7, height=8, device = "pdf")

