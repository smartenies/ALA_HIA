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

geo_data <- "T:/Rsch-MRS/ECHO/SEM Large Data/Spatial Data/"

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

co_zcta_bound <- st_union(co_zcta)
plot(st_geometry(co_zcta_bound), border = "red", add = T)

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
if (zcta_type == "all_co") {
  hia_boundary <- co_zcta_bound %>% 
    st_transform(crs = albers)
  
  head(hia_boundary)
  plot(st_geometry(hia_boundary))
  save(hia_boundary, file="./Data/Spatial Data/hia_boundary.RData")
  
} else {
  hia_boundary <- st_read(paste(geo_data, "ALA_HIA_bound.shp", sep="")) %>%
    st_transform(crs = albers)
  head(hia_boundary)
  plot(st_geometry(hia_boundary))
  save(hia_boundary, file="./Data/Spatial Data/hia_boundary.RData")
  
  plot(st_geometry(sfr_counties))
  plot(st_geometry(hia_boundary), border="red", add=T)
}

#' Map of the study area
ggplot() +
  ggtitle("ZCTAs in the Southern Front Range area") +
  geom_sf(data=sfr_counties, color="red", fill=NA, size=1.5) +
  geom_sf(data=sfr_zcta, color="black", fill="lightblue", alpha=0.2) +
  geom_sf(data=pp, color="red", pch=16, cex=3) +
  simple_theme

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
#'  
#'  B23025e3     Population 16+ in the civilian work force
#'  B23025e5     Population 16+ in the civilian work force that are unemployed
#'  
#'  B25035e1	   Median year built for housing
#' -----------------------------------------------------------------------------

years <- "2010_2014"

load(file=paste("./Data/ACS_", years, "/ACS.RData", sep=""))

acs2 <- acs

# Total census tract population
acs2$total_pop <- acs2$B01001e1 

# Population under 5 years
acs2$under5 <- acs2$B01001e3 + acs2$B01001e27
acs2$pct_under5 <- (acs2$under5 / acs2$total_pop) * 100

# Population 65 years and older
acs2$over64 <- (acs2$B01001e20 + acs2$B01001e21 + acs2$B01001e22 + acs2$B01001e23 + 
                 acs2$B01001e24 + acs2$B01001e25 + acs2$B01001e44 + acs2$B01001e45 + 
                 acs2$B01001e46 + acs2$B01001e47 + acs2$B01001e48 + acs2$B01001e49) 
acs2$pct_over64 <- (acs2$over64 / acs2$total_pop) * 100

# Non-white population (persons of color)
acs2$nhw <- acs2$B03002e3 # non-Hispanic White only
acs2$poc <- acs2$total_pop - acs2$nhw # Total pop minus NHW
acs2$pct_nhw <- (acs2$nhw / acs2$total_pop) * 100
acs2$pct_poc <- (acs2$poc / acs2$total_pop) * 100

#' Foreign born populations
acs2$fb <- acs2$B05002e13 #Foreign born
acs2$pct_fb <- (acs2$fb / acs2$total_pop) * 100

#' Educational attainment (adults ages 25 and older)
acs2$over24 <- acs2$B15003e1
acs2$less_hs <- acs2$B15003e2 + acs2$B15003e3 + acs2$B15003e4 +
  acs2$B15003e5 + acs2$B15003e6 + acs2$B15003e7 + acs2$B15003e8 +
  acs2$B15003e9 + acs2$B15003e10 + acs2$B15003e11 + acs2$B15003e12 +
  acs2$B15003e13 + acs2$B15003e14 + acs2$B15003e15 +
  acs2$B15003e16
acs2$hs_grad <- acs2$B15003e17 + acs2$B15003e18
acs2$some_col <- acs2$B15003e19 + acs2$B15003e20
acs2$assoc <- acs2$B15003e21
acs2$bach <- acs2$B15003e22
acs2$advanced <- acs2$B15003e23 + acs2$B15003e24 + acs2$B15003e25

acs2$pct_less_hs <- (acs2$less_hs / acs2$over24) * 100
acs2$pct_hs <- (acs2$hs_grad / acs2$over24) * 100
acs2$pct_college <- ((acs2$bach + acs2$advanced) / acs2$over24) * 100

acs2$pct_hs_grad <- 100 - acs2$pct_less_hs

#' Linguistic isolation (limited English at household level)
acs2$total_hh <- acs2$B16002e1
acs2$limited_eng <- acs2$B16002e4 + acs2$B16002e7 + acs2$B16002e10 +
  acs2$B16002e13
acs2$pct_hh_limited_eng <- (acs2$limited_eng / acs2$total_hh) * 100
acs2$pct_hh_not_limited_eng <- 100 - acs2$pct_hh_limited_eng

#' Uneploylment
acs2$civ_wf <- acs2$B23025e3
acs2$unemp <- acs2$B23025e5
acs2$pct_unemployed <- (acs2$unemp / acs2$civ_wf) * 100
acs2$pct_employed <- 100 - acs2$pct_unemployed

#' Households with past year income below poverty level
acs2$hh_pov <- acs2$B17017e2
acs2$pct_hh_pov <- (acs2$hh_pov / acs2$total_hh) * 100
acs2$pct_hh_above_pov <- 100 - acs2$pct_hh_pov

#' Median household income (2014 inflation-adjusted dollars)
acs2$med_income <- acs2$B19013e1

#' Save new variables
var_cols <- c("GEOID", "total_pop", "pct_under5", "pct_over64", 
              "pct_nhw", "pct_poc", "pct_fb",
              "pct_less_hs", "pct_hs_grad",
              "pct_hh_pov", "pct_hh_above_pov",
              "pct_unemployed", "pct_employed", 
              "pct_hh_limited_eng", "pct_hh_not_limited_eng", 
              "med_income")

acs2 <- acs2[,var_cols]

save(acs2, file=paste("./Data/acs_", years, "/acs2.RData", sep=""))
write.table(acs2, file="./HIA Inputs/ses indicators.txt")
