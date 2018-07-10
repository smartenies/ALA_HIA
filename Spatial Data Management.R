#' =============================================================================
#' Project: American Lung Association HIA
#' Date created: September 26, 2017
#' Date Updated: June 26, 2018
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description:
#' 
#' This project estimates the health impacts attributable to two coal-fired 
#' power plants in the front range region of CO: Comanche (in Pueblo, CO) and 
#' Martin Drake (in Colorado Springs, CO). The facilities are slated to be 
#' decommissioned by 2025.
#' 
#' This scrip specifies the spatial data used in the HIA
#' =============================================================================

library(sf)
library(Hmisc)
library(ggplot2)
library(raster)
library(rgeos)
library(ggthemes)
library(dplyr)
library(stringr)
library(readxl)

#' For ggplots
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_line(color="transparent"),
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

geo_data <- "T:/Rsch-MRS/ECHO/SEM Large Data/Spatial Data/"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

#' =============================================================================
#' Object for the location of the power plants
#' =============================================================================

pp_df <- data.frame(id = c("Martin Drake", "Comanche"),
                    lat = c(38.8244, 38.2081),
                    long = c(-104.8331, -104.5747))

pp <- st_as_sf(pp_df, coords=c("long", "lat"), crs = "+init=epsg:4326") %>%
  st_transform(crs = albers)

plot(st_geometry(pp))

save(pp, file="./Data/Spatial Data/power_plants.RData")

#' =============================================================================
#' Census tract shapefiles and sf for R
#' =============================================================================

#' Colorado ZIP code tabulation areas
co_zcta <- st_read(paste(geo_data, "CO_ZCTA_2014.shp", sep="")) %>%
  mutate(GEOID10 = as.character(GEOID10)) %>%
  st_transform(crs = albers)

plot(st_geometry(co_zcta))
save(co_zcta, file="./Data/Spatial Data/co_zcta.RData")

#' CO Counties
co_counties <- st_read(paste(geo_data, "us_counties_2010.shp", sep="")) %>%
select(GEOID10) %>% 
  mutate(GEOID10 = as.character(GEOID10)) %>% 
  mutate(state = substr(GEOID10, 1, 2)) %>%
  filter(state == "08") %>%
  st_transform(crs = albers)
head(co_counties)
plot(st_geometry(co_counties))

save(co_counties, file="./Data/Spatial Data/co_counties.RData")

#' Front Range airsheds: Arkansas, South Platt, Republican
fr_airsheds <- st_read(paste(geo_data, "co_airsheds.shp", sep="")) %>%
  filter(AIRSHED %in% c("Arkanasas", "South Platt", "Republican")) %>%
  st_transform(crs = albers)
head(fr_airsheds)
plot(st_geometry(fr_airsheds))

#' Airshed ZCTAs
fr_zcta <- st_join(co_zcta, fr_airsheds) %>%
  filter(!is.na(AIRSHED)) %>%
  select(GEOID10) %>%
  distinct()
plot(st_geometry(fr_zcta))
save(fr_zcta, file="./Data/Spatial Data/fr_zcta.RData")

#' Southern Front Range counties: Custer (027), El Paso (041), Fremont (043),
#' Pueblo (101), Teller (119),
sfr <- c("08027", "08041", "08043", "08101", "08119")

sfr_counties <- filter(co_counties, GEOID10 %in% sfr) %>%
  rename(county = GEOID10)
plot(st_geometry(sfr_counties))

save(sfr_counties, file="./Data/Spatial Data/sfr_counties.RData")
  
#' Southern Front Range ZCTAs
sfr_zcta <- st_join(co_zcta, sfr_counties) %>%
  filter(!is.na(county))
save(sfr_zcta, file="./Data/Spatial Data/sfr_zcta.RData")

#' HIA boundary for CMAQ points
hia_boundary <- st_read(paste(geo_data, "ALA_HIA_bound.shp", sep="")) %>%
  st_transform(crs = albers)
head(hia_boundary)
plot(st_geometry(hia_boundary))
save(hia_boundary, file="./Data/Spatial Data/hia_boundary.RData")

plot(st_geometry(sfr_counties))
plot(st_geometry(hia_boundary), border="red", add=T)

#' Map of the study area
ggplot() +
  ggtitle("ZCTAs in the Southern Front Range area") +
  geom_sf(data=sfr_counties, color="red", fill=NA, size=1.5) +
  geom_sf(data=sfr_zcta, color="black", fill="lightblue", alpha=0.2) +
  geom_sf(data=pp, color="red", pch=16, cex=3) +
  simple_theme
ggsave(filename = "./Maps/Study Area.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")


#' =============================================================================
#' ACS demographic and socioeconomic variables
#' =============================================================================

#' -----------------------------------------------------------------------------
#' Reading in the ACS files and mapping key demographic/SES variables
#' .txt files extracted from the ACS-TIGER/Line geodatabase
#' See /Data/ACS_* for the ArcMap file and original .gbd
#' Make sure there are no XML files in the data folder (they mess with the loop)
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/co_zcta.RData")
zcta_list <- as.character(unique(co_zcta$GEOID10))
years <- "2010_2014"

file_list <- list.files(paste("./Data/ACS_", years, sep=""), pattern="X")
dataset <- data.frame()
for (i in 1:length(file_list)){
  file <- file_list[i]
  temp <- read.table(paste("./Data/ACS_", years, "/", file, sep=""),
                     header=TRUE, sep=",")
  temp$OBJECTID <-NULL
  temp$GEOID <- gsub("86000US", "", as.character(temp$GEOID))
  temp <- temp[which(temp$GEOID %in% zcta_list),] # Just CO ZCTAs
  temp <- temp[order(temp$GEOID),]
  
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
acs$GEOID <- as.character(acs$GEOID)
#acs$GEOID <- gsub("14000US", "", acs$GEOID)
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

years <- "2010_2014"

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
ses_vars <- c("GEOID", "total_pop", "under_5", "pct_under5", 
              "over64", "pct_over64", "nhw", "poc", "pct_poc", 
              "fb", "pct_fb", "over24", "less_hs", "hs_grad", 
              "some_col", "assoc", "bach", "advanced",
              "pct_less_hs", "pct_hs", "pct_college",
              "total_hh", "limited_eng", "pct_limited_eng", 
              "hh_pov", "pct_hh_pov", "med_income")

acs <- acs[,c(1,11228:11253)]

save(acs, file=paste("./Data/ACS_", years, "/ACS.RData", sep=""))
write.table(acs, file="./HIA Inputs/ses indicators.txt")
