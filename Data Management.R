#' -----------------------------------------------------------------------------
#' Project: American Lung Association HIA
#' Date created: September 26, 2017
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' 
#' This project estimates the health impacts attributable to two coal-fired 
#' power plants in the front range region of CO: Comanche (in Pueblo, CO) and 
#' Martin Drake (in Colorado Springs, CO). The facilities are slated to be 
#' decommissioned by 2025.
#' -----------------------------------------------------------------------------

library(foreign)
library(sp)
library(Hmisc)
library(gstat)
library(rgdal)
library(ggplot2)
library(ggmap)
library(raster)
library(rgeos)
library(maptools)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(gridExtra)
library(plyr)
library(stringr)
library(readxl)

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

#' -----------------------------------------------------------------------------
#' Census tract shapefiles and spatial objects for R
#' Use the "_map" objects for ggplots
#' -----------------------------------------------------------------------------

#' Front Range counties: Adams (001), Arapahoe (005), Boulder (013), Broomfield
#' (014), Clear Creek (019), Denver (031), Douglas (035), Elbert (039), El Paso 
#' (041), Fremont (043), Gilpin (047), Jefferson (059), Larimer (069), Park (093), 
#' Pueblo (101), Teller (119), Weld (123), 

fr <- c("001", "005", "013", "014", "019", "031", "035", "039", "043", "041", 
        "047", "059", "069", "093", "101", "119", "123")

#' Colorado Census Tracts
co_tracts <- readOGR(dsn = "./Data/Shapefiles", layer = "co_tracts_2015")
co_tracts$GEOID <- as.character(co_tracts$GEOID)
plot(co_tracts)
save(co_tracts, file="./Data/Spatial Data/co_tracts_latlong.RData")

co_tracts_utm <- spTransform(co_tracts, 
                             CRS("+init=epsg:26912")) #Project to UTM 13N
co_tracts_utm@data$id <- rownames(co_tracts_utm@data)
co_tracts_utm_polygons <- fortify(co_tracts_utm, region="id")
co_tracts_utm_map <- merge(co_tracts_utm_polygons, co_tracts_utm@data, by="id")
rm(co_tracts_utm_polygons)
save(co_tracts_utm_map, co_tracts_utm, 
     file="./Data/Spatial Data/co_tracts_utm_map.RData")

co_bound_utm <- unionSpatialPolygons(co_tracts_utm, 
                                     IDs=co_tracts_utm@data$STATEFP,
                                     avoidUnaryUnion = T)
co_bound_utm_polygons <- fortify(co_bound_utm, region="id")
co_bound_utm_map <- co_bound_utm_polygons
rm(co_bound_utm_polygons)
save(co_bound_utm_map, co_bound_utm, 
     file="./Data/Spatial Data/co_bound_utm_map.RData")


#' Front Range Census Tracts
fr_tracts <- co_tracts[which(co_tracts$COUNTYFP %in% fr),]
plot(fr_tracts)
save(fr_tracts, 
     file="./Data/Spatial Data/fr_tracts_latlong.RData")

fr_tracts_utm <- spTransform(fr_tracts, 
                             CRS("+init=epsg:26912")) #Project to UTM 13N
fr_tracts_utm@data$id <- rownames(fr_tracts_utm@data)
fr_tracts_utm_polygons <- fortify(fr_tracts_utm, region="id")
fr_tracts_utm_map <- merge(fr_tracts_utm_polygons, fr_tracts_utm@data, by="id")
rm(fr_tracts_utm_polygons)
save(fr_tracts_utm_map, fr_tracts_utm, 
     file="./Data/Spatial Data/fr_tracts_utm_map.RData")

fr_bound_utm <- unionSpatialPolygons(fr_tracts_utm, 
                                     IDs=fr_tracts_utm@data$STATEFP,
                                     avoidUnaryUnion = T)
fr_bound_utm_polygons <- fortify(fr_bound_utm, region="id")
fr_bound_utm_map <- fr_bound_utm_polygons
rm(fr_bound_utm_polygons)
save(fr_bound_utm_map, fr_bound_utm, 
     file="./Data/Spatial Data/fr_bound_utm_map.RData")

load("./Data/Spatial Data/fr_tracts_utm_map.RData")
load("./Data/Spatial Data/fr_bound_utm_map.RData")

area_map <- ggplot() +
  ggtitle("Census tracts in the Front Range Urban Corridor") +
  geom_polygon(data=fr_tracts_utm_map, aes(x=long/1000, y=lat/1000, group=group),
               color="black", fill="lightblue", alpha=0.2) +
  geom_polygon(data=fr_bound_utm_map, aes(x=long/1000, y=lat/1000, group=group),
               color="blue", fill=NA, size=1.5) +
  xlab("UTM X (km)") + ylab("UTM Y (km)") +
  simple_theme
print(area_map)
ggsave(area_map, filename = "./Maps/Study Area.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")

#' -----------------------------------------------------------------------------
#' Grid for health data
#' Based on the 3 km x 3 km grid Ryan Gan uses in the wildfires project
#' -----------------------------------------------------------------------------

h_grid <- readOGR(dsn = "./Data/Shapefiles", layer = "co_grid")
summary(h_grid)
save(h_grid, file="./Data/Spatial Data/3x3_grid_latlong.RData")

proj4string(h_grid) <- CRS("+init=epsg:4326") #' long/lat

h_grid_utm <- spTransform(h_grid, CRS("+init=epsg:26912")) #UTM 13N
save(h_grid_utm, file="./Data/Spatial Data/3x3_grid_UTM.RData")

#' -----------------------------------------------------------------------------
#' Reading in the ACS files and mapping key demographic/SES variables
#' .txt files extracted from the ACS-TIGER/Line geodatabase
#' See /Data/ACS_* for the ArcMap file and original .gbd
#' Make sure there are no XML files in the data folder (they mess with the loop)
#' -----------------------------------------------------------------------------

years <- "2011_2015"

file_list <- list.files(paste("./Data/ACS_", years, sep=""), pattern="X")
dataset <- data.frame()
for (i in 1:length(file_list)){
  file <- file_list[i]
  temp <- read.table(paste("./Data/ACS_", years, "/", file, sep=""),
                     header=TRUE, sep=",")
  temp$OBJECTID <-NULL
  temp$GEOID <- as.character(temp$GEOID)
  
  if (i == 1) {
    dataset <- temp
    print(file)
  } else {
    dataset <- merge(dataset, temp, "GEOID") #' joining columns
    print(file)
    rm(temp)
  }
}

acs <- dataset
acs$GEOID <- gsub("14000US", "", acs$GEOID)
rm(dataset, file_list, file)

save(acs, file=paste("./Data/ACS_", years, "/ACS.RData", sep=""))

#' -----------------------------------------------------------------------------
#' Creating new demographic and SES variables for mapping
#' Using data dictionary for the ACS TIGER/Line files
#' Available:
#' 
#'  ACS Data Dictionary
#'  Variable     Definition
#'  B01001e1     Total population
#'  B01001e3     Male population under 5 years
#'  B01001e27    Female population under 5 years
#'  
#'  B01001e20    Male population 65-66
#'  B01001e21    Male population 67-69
#'  B01001e22    Male population 70-74
#'  B01001e23    Male population 75-79
#'  B01001e24    Male population 80-84
#'  B01001e25    Male populiation 85+
#'  B01001e44    Female population 65-66
#'  B01001e45    Female population 67-69
#'  B01001e46    Female population 70-74
#'  B01001e47    Female population 75-79
#'  B01001e48    Female population 80-84
#'  B01001e49    Female populiation 85+  
#'  
#'  B03002e3     Non-Hispanic white
#'  
#'  B05002e13    Foreign born
#'  
#'  B15003e1     Population 25 years and older
#'  B15003e2     No schooling
#'  B15003e3     Nursery school
#'  B15003e4     K
#'  B15003e5     1st grade
#'  B15003e6     2nd grade
#'  B15003e7     3rd grade
#'  B15003e8     4th grade
#'  B15003e9     5th grade
#'  B15003e10    6th grade
#'  B15003e11    7th grade
#'  B15003e12    8th grade
#'  B15003e13    9th grade
#'  B15003e14    10th grade
#'  B15003e15    11th grade
#'  B15003e16    12th grade, no diploma
#'  B15003e17    High school diploma
#'  B15003e18    GED or alternative
#'  B15003e19    Some college (1 year), no degree
#'  B15003e20    Some college (>1 year), no degree
#'  B15003e21    Associate's
#'  B15003e22    Bachelor's
#'  B15003e23    Master's
#'  B15003e22    Professional
#'  B15003e25    Doctorate
#'    
#'  B16002e1     Total households
#'  B16002e4     Spanish speaking households, limited English
#'  B16002e7     Other Indo_European speaking households, limited English
#'  B16002e10    Asian/PI speaking households, limited English
#'  B16002e13    Other language speaking households, limited English
#'  
#'  B17017e2     Households in poverty
#'  
#'  B19013e1     Median household income (2014 inflation-adjusted $)
#' -----------------------------------------------------------------------------

years <- "2011_2015"

load(file=paste("./Data/ACS_", years, "/ACS.RData", sep=""))

# Total census tract population
acs$total_pop <- acs$B01001e1 

# Population under 5 years
acs$under5 <- acs$B01001e3 + acs$B01001e27
acs$pct_under5 <- (acs$under5 / acs$total_pop) * 100

# Population 65 years and older
acs$over64 <- (acs$B01001e20 + acs$B01001e21 + acs$B01001e22 + acs$B01001e23 + 
                 acs$B01001e24 + acs$B01001e25 + acs$B01001e44 + acs$B01001e45 + 
                 acs$B01001e46 + acs$B01001e47 + acs$B01001e48 + acs$B01001e49) 
acs$pct_over64 <- (acs$over64 / acs$total_pop) * 100

# Non-white population (persons of color)
acs$nhw <- acs$B03002e3 # non-Hispanic White only
acs$poc <- acs$total_pop - acs$nhw # Total pop minus NHW
acs$pct_poc <- (acs$poc / acs$total_pop) * 100

#' Foreign born populations
acs$fb <- acs$B05002e13 #Foreign born
acs$pct_fb <- (acs$fb / acs$total_pop) * 100

#' Educational attainment (adults ages 25 and older)
acs$over24 <- acs$B15003e1
acs$less_hs <- acs$B15003e2 + acs$B15003e3 + acs$B15003e4 +
  acs$B15003e5 + acs$B15003e6 + acs$B15003e7 + acs$B15003e8 +
  acs$B15003e9 + acs$B15003e10 + acs$B15003e11 + acs$B15003e12 +
  acs$B15003e13 + acs$B15003e14 + acs$B15003e15 +
  acs$B15003e16
acs$hs_grad <- acs$B15003e17 + acs$B15003e18
acs$some_col <- acs$B15003e19 + acs$B15003e20
acs$assoc <- acs$B15003e21
acs$bach <- acs$B15003e22
acs$advanced <- acs$B15003e23 + acs$B15003e24 + acs$B15003e25

acs$pct_less_hs <- (acs$less_hs / acs$over24) * 100
acs$pct_hs <- (acs$hs_grad / acs$over24) * 100
acs$pct_college <- ((acs$bach + acs$advanced) / acs$over24) * 100

#' Linguistic isolation (limited English at household level)
acs$total_hh <- acs$B16002e1
acs$limited_eng <- acs$B16002e4 + acs$B16002e7 + acs$B16002e10 +
  acs$B16002e13
acs$pct_limited_eng <- (acs$limited_eng / acs$total_hh) * 100

#' Households with past year income below poverty level
acs$hh_pov <- acs$B17017e2
acs$pct_hh_pov <- (acs$hh_pov / acs$total_hh) * 100

#' Median household income (2014 inflation-adjusted dollars)
acs$med_income <- acs$B19013e1

#' Save new variables
save(acs, file=paste("./Data/ACS_", years, "/ACS.RData", sep=""))

#' mapping some key variables (Colorado)
load(paste("./Data/ACS_", years, "/ACS.RData", sep=""))

ses_vars <- c("pct_under5", "pct_over64", "pct_poc", "pct_fb",
              "pct_less_hs", "pct_limited_eng", "pct_hh_pov",
              "med_income")
titles <- c("Population under 5 years of age",
            "Population 65 years of age or older",
            "Persons of color", "Foreign born population",
            "Adults 25 or older with less than a high school diploma",
            "Households that speak limited English",
            "Households with last year income below the poverty level",
            "Median household income (2014$)")

#' First, across all of CO
load("./Data/Spatial Data/co_tracts_utm_map.RData")
for (i in 1:length(ses_vars)) {
  ses_var <- ses_vars[i]
  ses_name <- ifelse(ses_var=="med_income", "2014$", "Percentage of population:")
  
  ses_map_df <- merge(co_tracts_utm_map, acs[,c("GEOID", ses_var)], by="GEOID")
  ses_map_df$breaks <- cut(ses_map_df[,ses_var], breaks = 5, na.rm=T)
  
  
  col_list <- c("#d7191c", "#fdae61", "#ffff66", "#abd9e9", "#2c7bb6")
  if(ses_var != "med_income") {col_list <- rev(col_list)}
  
  ses_map <- ggplot() +
    ggtitle(titles[i]) +
    geom_polygon(data=ses_map_df, aes(x=long/1000, y=lat/1000, 
                                      group=group, fill=breaks),
                 color=NA, show.legend = T) +
    scale_fill_manual(name=ses_name, values=col_list, na.value="grey90") +
    xlab("UTM X (km)") + ylab("UTM Y (km)") +
    theme(legend.position = "right") +
    simple_theme
  print(ses_map)
  print(ses_var)
  
  ggsave(ses_map, filename = paste("./Maps/CO ", ses_var, " ", years, ".jpeg", 
                                   sep=""), 
         device = "jpeg", dpi=400, width = 9, height = 6, units="in")
}

#' Then, just Front Range
load("./Data/Spatial Data/fr_tracts_utm_map.RData")
for (i in 1:length(ses_vars)) {
  ses_var <- ses_vars[i]
  ses_name <- ifelse(ses_var=="med_income", "2014$", "Percentage of population:")
  
  ses_map_df <- merge(fr_tracts_utm_map, acs[,c("GEOID", ses_var)], by="GEOID")
  ses_map_df$breaks <- cut(ses_map_df[,ses_var], breaks = 5, na.rm=T)
  
  
  col_list <- c("#d7191c", "#fdae61", "#ffff66", "#abd9e9", "#2c7bb6")
  if(ses_var != "med_income") {col_list <- rev(col_list)}
  
  ses_map <- ggplot() +
    ggtitle(titles[i]) +
    geom_polygon(data=ses_map_df, aes(x=long/1000, y=lat/1000, 
                                      group=group, fill=breaks),
                 color=NA, show.legend = T) +
    scale_fill_manual(name=ses_name, values=col_list, na.value="grey90") +
    xlab("UTM X (km)") + ylab("UTM Y (km)") +
    theme(legend.position = "right") +
    simple_theme
  print(ses_map)
  print(ses_var)
  
  ggsave(ses_map, filename = paste("./Maps/FR ", ses_var, " ", years, ".jpeg", 
                                   sep=""), 
         device = "jpeg", dpi=400, width = 9, height = 6, units="in")
}












