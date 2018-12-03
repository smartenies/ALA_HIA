#' -----------------------------------------------------------------------------
#' Project: American Lung Association HIA
#' Date created: January 23, 2018
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
#' This script makes a map of the study area and all other figures for the 
#' report and mansucript
#' -----------------------------------------------------------------------------

library(sf)
library(raster)
library(ggplot2)
library(ggmap)
library(ggsn)
library(ggthemes)
library(tidyverse)
library(readxl)
library(writexl)
library(viridis)

#' Google API for ggmap
google_api_key <- "AIzaSyA_eKisw1HJSG0umLvCzgQI8bekuhpN5Cc"
register_google(key = google_api_key)

#' For ggplots
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
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

map_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' Study Area Map
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/power_plants.RData")
pp_sf <- st_transform(pp, ll_wgs84)

#' Get list of ZCTAs from the Winter Baseline case
load("./HIA Inputs/HIA_Winter_zcta.RData")
zcta_sf <- st_as_sf(zcta)
head(zcta_sf)

zcta_union <- st_union(zcta_sf)
head(zcta_union)
plot(st_geometry(zcta_union))

#' Get receptor points from the Winter Baseline case
load("./HIA Inputs/HIA_Winter_cmaq_spatial.RData")
cmaq_sf <- st_as_sf(cmaq_p) %>% 
  st_transform(crs = ll_wgs84)

cmaq_envelop <- st_convex_hull(st_union(cmaq_sf))

base_map <- get_map(location = "Colorado Springs, Colorado", zoom = 7)
ggmap(base_map)

ggmap(base_map) +
  geom_sf(data = zcta_union, aes(color = "zcta"), fill=NA, size = 0.5,
          inherit.aes = F) +
  geom_sf(data = cmaq_envelop, aes(color = "cmaq"), fill=NA, size = 0.5,
          inherit.aes = F) +
  geom_sf(data = pp_sf, color="red", size = 2, inherit.aes = F) +
  geom_sf_text(data = pp_sf, aes(label = id), inherit.aes = F,
               nudge_y = -0.1, color = "red") +
  scale_color_manual(name = "Boundary",
                     labels = c("zcta" = "Study area",
                                "cmaq" = "CMAQ subgrid"),
                     values = c("zcta" = "blue",
                                "cmaq" = "magenta")) +
  map_theme +
  theme(legend.position = c(0.8, 0.8),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(36.8, 40.5), 
           xlim = c(-106.5, -102)) +
  north(x.min = -106.5, x.max = -102,
        y.min =  36.8, y.max = 40.5,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -102.75, y = 37.25)) +
  scalebar(x.min = -106.5, x.max = -102,
           y.min =  36.8, y.max = 40.5,
           dist = 30, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -102.75, y = 37.0))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Study Area Map.jpeg", 
       device = "jpeg", dpi=500, units = "in", height = 5, width = 5)

#' -----------------------------------------------------------------------------
#' SES variables for the entire study area
#' -----------------------------------------------------------------------------

load("./HIA Inputs/HIA_Winter_zcta.RData")
zcta_ids <- unique(zcta$GEOID10)
rm(zcta)

#' Plot SES variables
ses_file <- "./HIA Inputs/ses indicators.txt"
ses <- read.table(ses_file, header=T, stringsAsFactors = F) %>%
  mutate(GEOID = gsub("86000US", "", GEOID)) %>%
  dplyr::rename(zcta = GEOID) %>%
  dplyr::select(zcta, total_pop, pct_poc, pct_nhw, med_income)

load("./Data/Spatial Data/co_zcta.RData")
zcta <- filter(co_zcta, GEOID10 %in% zcta_ids) %>%
  select(GEOID10) %>%
  rename(zcta = GEOID10) %>%
  left_join(ses, by="zcta")

plot(st_geometry(zcta))

ggplot(data=zcta) +
  ggtitle(paste("Median income at the ZCTA level")) +
  geom_sf(aes(fill=med_income)) +
  geom_sf(data = pp, color = "red") +
  scale_fill_viridis(name = "Median income\n(2014$)") +
  simple_theme
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Median Income.jpeg",
       device = "jpeg", dpi = 600)

ggplot(data=zcta) +
  ggtitle(paste("Persons of Color")) +
  geom_sf(aes(fill=pct_poc)) +
  geom_sf(data = pp, color = "red") +
  scale_fill_viridis(name = "Percentage\nof ZCTA population") +
  simple_theme
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Percent POC.jpeg",
       device = "jpeg", dpi = 600)

ggplot(data=zcta) +
  ggtitle(paste("Persons that are non-Hispanic white alone")) +
  geom_sf(aes(fill=pct_nhw)) +
  geom_sf(data = pp, color = "red") +
  scale_fill_viridis(name = "Percentage\nof ZCTA population") +
  simple_theme
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Percent NHW.jpeg",
       device = "jpeg", dpi = 600)

#' -----------------------------------------------------------------------------
#' SES variables and avoided deaths and hospitalizations for Pueblo and 
#' Colorado Springs
#' -----------------------------------------------------------------------------

#' City boundaries
co_cities <- st_read("T:/Rsch-MRS/ECHO/SEM Large Data/Spatial Data/MuniBounds.shp") %>% 
  st_transform(albers)
plot(st_geometry(co_cities)) 

pueblo <- filter(co_cities, first_city == "Pueblo") %>% 
  st_transform(ll_wgs84)
plot(st_geometry(pueblo))

springs <- filter(co_cities, first_city == "Colorado Springs") %>% 
  st_transform(ll_wgs84)
plot(st_geometry(springs))

#' sf object with all of the variables

rate_pop <- 10000

load("./HIA Inputs/HIA_Winter_zcta.RData")
zcta_ids <- unique(zcta$GEOID10)
rm(zcta)

ses_file <- "./HIA Inputs/ses indicators.txt"
ses <- read.table(ses_file, header=T, stringsAsFactors = F) %>%
  mutate(GEOID = gsub("86000US", "", GEOID)) %>%
  dplyr::rename(zcta = GEOID) %>%
  dplyr::select(zcta, total_pop, pct_poc, pct_nhw, med_income)

load("./HIA Outputs/HIA_Winter_HIA_Summer_zcta_combined.RData")
impacts <- select(combined_df, zcta, pol, outcome, median_scaled) %>% 
  left_join(ses, by="zcta") %>% 
  mutate(rate = (median_scaled / total_pop) * rate_pop) %>% 
  filter(outcome %in% c("mort_ac", "st_mort_na", "hosp_res", "hosp_cvd")) 
  
load("./Data/Spatial Data/co_zcta.RData")
zcta_impacts <- filter(co_zcta, GEOID10 %in% zcta_ids) %>%
  select(GEOID10) %>%
  rename(zcta = GEOID10) %>%
  left_join(impacts, by="zcta") %>% 
  st_transform(ll_wgs84)

pueblo_zips <- c("81001", "81003", "81004", "81005", "81006",
                 "81007", "81008", "81022", "81025")

pueblo_impacts <- filter(zcta_impacts, zcta %in% pueblo_zips)
plot(st_geometry(pueblo_impacts))
plot(st_geometry(pueblo), border="red", add=T)

springs_zips <- c("80938", "80939", "80951", "80918", "80919", "80920",
                  "80921", "80922", "80923", "80924", "80925", "80926",
                  "80927", "80929", "80903", "80904", "80905", "80906", 
                  "80907", "80908", "80909", "80910", "80911", "80914",
                  "80915", "80916", "80917", "80809", "80829", "80831",
                  "80840", "80913", "80817", "80925", "80929")

springs_impacts <- filter(zcta_impacts, zcta %in% springs_zips)
plot(st_geometry(springs_impacts))
plot(st_geometry(springs), border="red", add=T)

#' Maps for Pueblo
ggplot(data = pueblo_impacts) +
  ggtitle(paste("Persons that are non-Hispanic white alone")) +
  geom_sf(aes(fill = pct_nhw), color = NA) +
  geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "Percentage") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.25, -103.75), ylim = c(37.6, 38.7))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Pueblo NHW.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)
  
ggplot(data = pueblo_impacts) +
  ggtitle(paste("Persons of color")) +
  geom_sf(aes(fill = pct_poc), color = NA) +
  geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "Percentage") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.25, -103.75), ylim = c(37.6, 38.7))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Pueblo POC.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)

ggplot(data = pueblo_impacts) +
  ggtitle(paste("Median income")) +
  geom_sf(aes(fill = med_income/10000), color = NA) +
  geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "2014$ (10,000's)") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.25, -103.75), ylim = c(37.6, 38.7))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Pueblo Income.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)

ggplot(data = filter(pueblo_impacts, pol == "pm" & outcome == "mort_ac")) +
  ggtitle(paste("Avoided premature deaths (PM\u2082.\u2085)")) +
  geom_sf(aes(fill = rate), color = NA) +
  geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "Rate per 10,000") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.25, -103.75), ylim = c(37.6, 38.7))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Pueblo Avoided PM Deaths.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)

ggplot(data = filter(pueblo_impacts, pol == "pm" & outcome == "hosp_cvd")) +
  ggtitle(paste("Avoided CVD hospitalizations (PM\u2082.\u2085)")) +
  geom_sf(aes(fill = rate), color = NA) +
  geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "Rate per 10,000") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.25, -103.75), ylim = c(37.6, 38.7))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Pueblo Avoided PM CVD.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)

ggplot(data = filter(pueblo_impacts, pol == "pm" & outcome == "hosp_res")) +
  ggtitle(paste("Avoided RES hospitalizations (PM\u2082.\u2085)")) +
  geom_sf(aes(fill = rate), color = NA) +
  geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "Rate per 10,000") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.25, -103.75), ylim = c(37.6, 38.7))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Pueblo Avoided PM RES.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)

ggplot(data = filter(pueblo_impacts, pol == "o3" & outcome == "st_mort_na")) +
  ggtitle(paste("Avoided premature deaths (O\u2083)")) +
  geom_sf(aes(fill = rate), color = NA) +
  geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "Rate per 10,000") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.25, -103.75), ylim = c(37.6, 38.7))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Pueblo Avoided O3 Deaths.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)

ggplot(data = filter(pueblo_impacts, pol == "o3" & outcome == "hosp_res")) +
  ggtitle(paste("Avoided RES hospitalizations (O\u2083)")) +
  geom_sf(aes(fill = rate), color = NA) +
  geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "Rate per 10,000") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.25, -103.75), ylim = c(37.6, 38.7))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Pueblo Avoided O3 RES.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)

#' Maps for Colorado Springs
ggplot(data = springs_impacts) +
  ggtitle(paste("Persons that are non-Hispanic white alone")) +
  geom_sf(aes(fill = pct_nhw), color = NA) +
  geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "Percentage") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.4, -104.1), ylim = c(38.3, 39.2))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Colorado Springs NHW.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)

ggplot(data = springs_impacts) +
  ggtitle(paste("Persons of color")) +
  geom_sf(aes(fill = pct_poc), color = NA) +
  geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "Percentage") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.4, -104.1), ylim = c(38.3, 39.2))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Colorado Springs POC.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)

ggplot(data = springs_impacts) +
  ggtitle(paste("Median income")) +
  geom_sf(aes(fill = med_income/10000), color = NA) +
  geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "2014$ (10,000's)") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.4, -104.1), ylim = c(38.3, 39.2))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Colorado Springs Income.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)

ggplot(data = filter(springs_impacts, pol == "pm" & outcome == "mort_ac")) +
  ggtitle(paste("Avoided premature deaths (PM\u2082.\u2085)")) +
  geom_sf(aes(fill = rate), color = NA) +
  geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "Rate per 10,000") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.4, -104.1), ylim = c(38.3, 39.2))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Colorado Springs Avoided PM Deaths.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)

ggplot(data = filter(springs_impacts, pol == "pm" & outcome == "hosp_cvd")) +
  ggtitle(paste("Avoided CVD hospitalizations (PM\u2082.\u2085)")) +
  geom_sf(aes(fill = rate), color = NA) +
  geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "Rate per 10,000") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.4, -104.1), ylim = c(38.3, 39.2))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Colorado Springs Avoided PM CVD.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)

ggplot(data = filter(springs_impacts, pol == "pm" & outcome == "hosp_res")) +
  ggtitle(paste("Avoided RES hospitalizations (PM\u2082.\u2085)")) +
  geom_sf(aes(fill = rate), color = NA) +
  geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "Rate per 10,000") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.4, -104.1), ylim = c(38.3, 39.2))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Colorado Springs Avoided PM RES.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)

ggplot(data = filter(springs_impacts, pol == "o3" & outcome == "st_mort_na")) +
  ggtitle(paste("Avoided premature deaths (O\u2083)")) +
  geom_sf(aes(fill = rate), color = NA) +
  geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "Rate per 10,000") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.4, -104.1), ylim = c(38.3, 39.2))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Colorado Springs Avoided O3 Deaths.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)

ggplot(data = filter(springs_impacts, pol == "o3" & outcome == "hosp_res")) +
  ggtitle(paste("Avoided RES hospitalizations (O\u2083)")) +
  geom_sf(aes(fill = rate), color = NA) +
  geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp, color = "lightblue", size = 3) +
  scale_fill_viridis(name = "Rate per 10,000") +
  simple_theme +
  theme(legend.position = "bottom",
        legend.text = element_text(angle = 90, hjust = 1),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  coord_sf(crs = ll_wgs84, xlim = c(-105.4, -104.1), ylim = c(38.3, 39.2))
ggsave(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Report/Figures/Colorado Springs Avoided O3 RES.jpeg",
       device = "jpeg", dpi = 600, units = "in", height = 5, width = 5)
