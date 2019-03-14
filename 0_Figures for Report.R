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
library(ggrepel)
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
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(), 
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
  panel.grid.major = element_line(colour = "transparent"),
  panel.grid.minor = element_line(colour = "transparent"),
  panel.border = element_blank(),
  panel.background = element_blank(), 
  axis.ticks = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_rect(fill = NA),
  legend.background=element_blank()
)

map_theme2 <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 9, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.major = element_line(colour = "transparent"),
  panel.grid.minor = element_line(colour = "transparent"),
  panel.border = element_blank(),
  panel.background = element_blank(), 
  axis.ticks = element_blank(),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_rect(fill = NA),
  legend.background=element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
ll_nad83 <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

#' -----------------------------------------------------------------------------
#' "prefixes"
#' For paper, need 7-10
#' -----------------------------------------------------------------------------

#' Unique prefixes for each run
pre <- c("BoD_Winter_", 
         "BoD_Summer_",
         "BoD_Winter_2017_", 
         "BoD_Summer_2017_",
         "BoD_Winter_2035_", 
         "BoD_Summer_2035_",
         "HIA_Winter_",
         "HIA_Summer_",
         "HIA_CF_Baseline_Winter_",
         "HIA_CF_Baseline_Summer_",
         "HIA_MD_BL2011_CMCH_Off_Winter",
         "HIA_MD_BL2011_CMCH_Off_Summer",
         "HIA_MD_BL2017_CMCH_Off_Winter",
         "HIA_MD_BL2017_CMCH_Off_Summer",
         "HIA_MD_Off_CMCH_BL2011_Winter",
         "HIA_MD_Off_CMCH_BL2011_Summer")

#' -----------------------------------------------------------------------------
#' Study Area Map
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/power_plants.RData")
pp_sf <- st_transform(pp, ll_wgs84)

#' Get list of ZCTAs from the Winter Baseline case
load("./HIA Inputs/HIA_Winter_zcta.RData")
zcta_sf <- st_as_sf(zcta) %>% 
  st_zm(drop = T)
head(zcta_sf)

zcta_union <- st_union(zcta_sf)
head(zcta_union)
plot(st_geometry(zcta_union))

#' Springs ZCTAs and Pueblo ZCTAs
springs_zips <- c("80938", "80939", "80951", "80918", "80919", "80920",
                  "80921", "80922", "80923", "80924", "80925", "80926",
                  "80927", "80929", "80903", "80904", "80905", "80906", 
                  "80907", "80908", "80909", "80910", "80911", "80914",
                  "80915", "80916", "80917", "80809", "80829", "80831",
                  "80840", "80913", "80817", "80925", "80929")

springs_zcta <- filter(zcta_sf, GEOID10 %in% springs_zips)
plot(st_geometry(springs_zcta))

pueblo_zips <- c("81001", "81003", "81004", "81005", "81006",
                 "81007", "81008", "81022", "81025")

pueblo_zcta <- filter(zcta_sf, GEOID10 %in% pueblo_zips)
plot(st_geometry(pueblo_zcta))

#' Get receptor points from the Winter Baseline case
load("./HIA Inputs/HIA_Winter_cmaq_spatial.RData")
cmaq_sf <- st_as_sf(cmaq_p) %>% 
  st_transform(crs = ll_wgs84)

cmaq_envelop <- st_convex_hull(st_union(cmaq_sf))

base_map <- get_map(location = "Colorado Springs, Colorado", zoom = 7)
ggmap(base_map)

ggmap(base_map, darken = c(0.45, "white")) +
  geom_sf(data = zcta_union, aes(color = "zcta", fill="zcta"), size = 0.5,
          inherit.aes = F, show.legend = "polygon") +
  geom_sf(data = springs_zcta, aes(color = "springs", fill="springs"), 
          inherit.aes = F, show.legend = "polygon", alpha = 0.3, size = 0.25) +
  geom_sf(data = pueblo_zcta, aes(color = "pueblo", fill="pueblo"), 
          inherit.aes = F, show.legend = "polygon", alpha = 0.3, size = 0.25) +
  geom_sf(data = pp_sf, color="red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_text_repel(data = pp_sf, aes(label = id, geometry = geometry),
                  stat = "sf_coordinates", direction = "x", nudge_x = 2,
                  colour = "red", segment.colour = "red",
                  inherit.aes = F, show.legend = F) +
  scale_color_manual(name = NULL,
                     labels = c("zcta" = "All ZCTAs",
                                "springs" = "Colorado Springs ZCTAs",
                                "pueblo" = "Pueblo ZCTAs"),
                     values = c("zcta" = "blue",
                                "springs" = "darkgreen",
                                "pueblo" = "orange")) +
  scale_fill_manual(name = NULL,
                     labels = c("zcta" = "All ZCTAs",
                                "springs" = "Colorado Springs ZCTAs",
                                "pueblo" = "Pueblo ZCTAs"),
                     values = c("zcta" = NA,
                                "springs" = "darkgreen",
                                "pueblo" = "orange"),
                     guide = guide_legend(override.aes = list(linetype = c("blank", "blank", "solid"), 
                                                              color = c("orange", "darkgreen", "blue"),
                                                              shape = c(NA, NA, NA)))) +
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
#' Changes in exposure
#' -----------------------------------------------------------------------------

# Second, map change in exposure ----
#' Winter and summer ozone and pm ("annual" averages)
pol_names <- c("pm", "o3")
pol_map <- c("PM\u2082.\u2085", "O\u2083")
unit_map <- c("(\u03BCg/m\u00B3)", "(ppb)")
season_map <- c("winter", "summer")

load("./Data/Spatial Data/power_plants.RData")
pp <- st_transform(pp, crs = ll_wgs84)

load("./Data/Spatial Data/co_zcta.RData")

#' PM2.5, Winter
s <- 7
i <- 1

load(paste("./HIA Inputs/", pre[s], "zcta.RData", sep=""))
zcta_ids <- unique(zcta$GEOID10)
zcta_within_ids <- unique(zcta_within$GEOID10)
rm(zcta, zcta_within)

#' Read in change in "annual" concentration
load(paste("./HIA Inputs/", pre[s], pol_names[i], "_zcta_metrics.RData", sep=""))
mean_df <- zcta_list[["ann_mean"]]

#' Plot changes in exposure
pm_winter_zcta <- filter(co_zcta, GEOID10 %in% zcta_ids) %>%
  select(GEOID10) %>%
  st_transform(crs = ll_wgs84) %>% 
  left_join(mean_df, by="GEOID10") %>% 
  filter(!is.na(wt_conc))
plot(st_geometry(pm_winter_zcta))
plot(pm_winter_zcta["wt_conc"])

# pm_winter <- ggmap(base_map, darken = c(0.75, "white")) +
pm_winter <- ggplot() +
  geom_sf(data = pm_winter_zcta, aes(fill = wt_conc), inherit.aes = F,
          color = NA) +
  # geom_sf(data = pp_sf, color="red", size = 0.5, inherit.aes = F, 
  #         show.legend = "point") +
  geom_text_repel(data = pp_sf, aes(label = id, geometry = geometry),
                  stat = "sf_coordinates", direction = "x", nudge_x = 2,
                  colour = "red", segment.colour = "red", segment.size = 0.3,
                  inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = paste(pol_map[i], unit_map[i])) +
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
           dist = 60, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -102.6, y = 37.0), st.dist = 0.025)
pm_winter

#' PM2.5, Summer
s <- 8
i <- 1

load(paste("./HIA Inputs/", pre[s], "zcta.RData", sep=""))
zcta_ids <- unique(zcta$GEOID10)
zcta_within_ids <- unique(zcta_within$GEOID10)
rm(zcta, zcta_within)

#' Read in change in "annual" concentration
load(paste("./HIA Inputs/", pre[s], pol_names[i], "_zcta_metrics.RData", sep=""))
mean_df <- zcta_list[["ann_mean"]]

#' Plot changes in exposure
pm_summer_zcta <- filter(co_zcta, GEOID10 %in% zcta_ids) %>%
  select(GEOID10) %>%
  st_transform(crs = ll_wgs84) %>% 
  left_join(mean_df, by="GEOID10") %>% 
  filter(!is.na(wt_conc))
plot(st_geometry(pm_summer_zcta))
plot(pm_summer_zcta["wt_conc"])

pm_summer <- ggplot() +
  geom_sf(data = pm_summer_zcta, aes(fill = wt_conc), inherit.aes = F,
          color = NA) +
  # geom_sf(data = pp_sf, color="red", size = 0.5, inherit.aes = F, 
  #         show.legend = "point") +
  geom_text_repel(data = pp_sf, aes(label = id, geometry = geometry),
                  stat = "sf_coordinates", direction = "x", nudge_x = 2,
                  colour = "red", segment.colour = "red", segment.size = 0.3,
                  inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = paste(pol_map[i], unit_map[i]), 
                     breaks = c(min(pm_summer_zcta$wt_conc), 0.1, 0.2, 0.3, 0.4),
                     labels = c(round(min(pm_summer_zcta$wt_conc),1), 
                                "0.1", "0.2", "0.3", "0.4")) + 
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
           dist = 60, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -102.6, y = 37.0), st.dist = 0.025)
pm_summer

#' ozone, Winter
s <- 7
i <- 2

load(paste("./HIA Inputs/", pre[s], "zcta.RData", sep=""))
zcta_ids <- unique(zcta$GEOID10)
zcta_within_ids <- unique(zcta_within$GEOID10)
rm(zcta, zcta_within)

#' Read in change in "annual" concentration
load(paste("./HIA Inputs/", pre[s], pol_names[i], "_zcta_metrics.RData", sep=""))
mean_df <- zcta_list[["ann_mean"]]

#' Plot changes in exposure
o3_winter_zcta <- filter(co_zcta, GEOID10 %in% zcta_ids) %>%
  select(GEOID10) %>%
  st_transform(crs = ll_wgs84) %>% 
  left_join(mean_df, by="GEOID10") %>% 
  filter(!is.na(wt_conc))
plot(st_geometry(o3_winter_zcta))
plot(o3_winter_zcta["wt_conc"])

o3_winter <- ggplot() +
  geom_sf(data = o3_winter_zcta, aes(fill = wt_conc), inherit.aes = F,
          color = NA) +
  # geom_sf(data = pp_sf, color="red", size = 0.5, inherit.aes = F, 
  #         show.legend = "point") +
  geom_text_repel(data = pp_sf, aes(label = id, geometry = geometry),
                  stat = "sf_coordinates", direction = "x", nudge_x = 2,
                  colour = "red", segment.colour = "red", segment.size = 0.3,
                  inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = paste(pol_map[i], unit_map[i]),
                     breaks = c(max(o3_winter_zcta$wt_conc), -0.1, -0.2, -0.3, -0.4),
                     labels = c(round(max(o3_winter_zcta$wt_conc),1), 
                                "-0.1", "-0.2", "-0.3", "-0.4")) + 
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
           dist = 60, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -102.6, y = 37.0), st.dist = 0.025)
o3_winter

#' O3, Summer
s <- 8
i <- 2

load(paste("./HIA Inputs/", pre[s], "zcta.RData", sep=""))
zcta_ids <- unique(zcta$GEOID10)
zcta_within_ids <- unique(zcta_within$GEOID10)
rm(zcta, zcta_within)

#' Read in change in "annual" concentration
load(paste("./HIA Inputs/", pre[s], pol_names[i], "_zcta_metrics.RData", sep=""))
mean_df <- zcta_list[["ann_mean"]]

#' Plot changes in exposure
o3_summer_zcta <- filter(co_zcta, GEOID10 %in% zcta_ids) %>%
  select(GEOID10) %>%
  st_transform(crs = ll_wgs84) %>% 
  left_join(mean_df, by="GEOID10") %>% 
  filter(!is.na(wt_conc))
plot(st_geometry(o3_summer_zcta))
plot(o3_summer_zcta["wt_conc"])

o3_summer <- ggplot() +
  geom_sf(data = o3_summer_zcta, aes(fill = wt_conc), inherit.aes = F,
          color = NA) +
  # geom_sf(data = pp_sf, color="red", size = 0.5, inherit.aes = F, 
  #         show.legend = "point") +
  geom_text_repel(data = pp_sf, aes(label = id, geometry = geometry),
                  stat = "sf_coordinates", direction = "x", nudge_x = 2,
                  colour = "red", segment.colour = "red", segment.size = 0.3,
                  inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = paste(pol_map[i], unit_map[i])) + 
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
           dist = 60, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -102.6, y = 37.0), st.dist = 0.025)
o3_summer


#' Combine them!
library(ggpubr)

hs1_maps_summer <- ggarrange(pm_summer, o3_summer, 
                      labels = c("A", "B"),
                      ncol = 2, nrow = 1)
hs1_maps_summer
ggsave(hs1_maps_summer,
       filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/HS1_Summer_Change_in_Exposure.jpeg", 
       device = "jpeg", dpi=500, units = "in", height = 5, width = 7)

hs1_maps_winter <- ggarrange(pm_winter, o3_winter, 
                             labels = c("A", "B"),
                             ncol = 2, nrow = 1)
hs1_maps_winter
ggsave(hs1_maps_winter,
       filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/HS1_Winter_Change_in_Exposure.jpeg", 
       device = "jpeg", dpi=500, units = "in", height = 5, width = 7)

#' -----------------------------------------------------------------------------
#' Avoided premature deaths
#' -----------------------------------------------------------------------------

rate_pop <- 10000

#' Health scenario 1 (s == 7 & 8)
s <- 7

load(paste("./HIA Outputs/", pre[s], "zcta_impacts.RData", sep=""))
out_df_1 <- out_df

load(paste("./HIA Outputs/", pre[s+1], "zcta_impacts.RData", sep=""))
out_df_2 <- out_df

rm(out_df)

combined_df <- bind_rows(out_df_1, out_df_2) %>%
  group_by(zcta, pol, outcome) %>%
  summarise(median = sum(median, na.rm=T),
            p2.5 = sum(p2.5, na.rm=T),
            p97.5 = sum(p97.5, na.rm=T),
            median_scaled = sum(median_scaled, na.rm=T),
            p2.5_scaled = sum(p2.5_scaled, na.rm=T),
            p97.5_scaled = sum(p97.5_scaled, na.rm=T),
            median_value = sum(median_value, na.rm=T),
            p2.5_value = sum(p2.5_value, na.rm=T),
            p97.5_value = sum(p97.5_value, na.rm=T))

#' Summarize impacts
total_df <- combined_df %>%
  group_by(pol, outcome) %>%
  summarise(median = round(sum(median, na.rm=T),2),
            p2.5 = round(sum(p2.5, na.rm=T),2),
            p97.5 = round(sum(p97.5, na.rm=T),2),
            median_scaled = round(sum(median_scaled, na.rm=T),0),
            p2.5_scaled = round(sum(p2.5_scaled, na.rm=T),0),
            p97.5_scaled = round(sum(p97.5_scaled, na.rm=T),0),
            median_value = round(sum(median_value, na.rm=T),0),
            p2.5_value = round(sum(p2.5_value, na.rm=T),0),
            p97.5_value = round(sum(p97.5_value, na.rm=T),0))

#' Map outcomes
#' Load the inequality indicators
ses_file <- "./HIA Inputs/ses indicators.txt"
ses <- read.table(ses_file, header=T, stringsAsFactors = F) %>%
  mutate(GEOID = gsub("86000US", "", GEOID)) %>%
  dplyr::rename(zcta = GEOID) %>%
  dplyr::select(zcta, total_pop, pct_poc, pct_nhw, med_income, 
                pct_hs_grad, pct_employed, pct_hh_not_limited_eng)

#' Clean up median impacts and calculate benefit rate
all_impacts <- ungroup(combined_df) %>%
  left_join(ses, by="zcta") %>% 
  select(zcta, pol, outcome, median_scaled, total_pop) %>% 
  mutate(pol = as.character(pol)) %>%
  mutate(rate = (median_scaled / total_pop) * rate_pop)

springs_impacts <- ungroup(combined_df) %>%
  left_join(ses, by="zcta") %>% 
  filter(zcta %in% springs_zips) %>% 
  select(zcta, pol, outcome, median_scaled, total_pop) %>% 
  mutate(pol = as.character(pol)) %>%
  mutate(rate = (median_scaled / total_pop) * rate_pop)

pueblo_impacts <- ungroup(combined_df) %>%
  left_join(ses, by="zcta") %>% 
  filter(zcta %in% pueblo_zips) %>% 
  select(zcta, pol, outcome, median_scaled, total_pop) %>% 
  mutate(pol = as.character(pol)) %>%
  mutate(rate = (median_scaled / total_pop) * rate_pop)

zcta_ids <- unique(all_impacts$zcta)

#' Map outcome rates across ZCTA's
load("./Data/Spatial Data/co_zcta.RData")

hs1_zcta_sf <- filter(co_zcta, GEOID10 %in% zcta_ids) %>%
  dplyr::rename(zcta = GEOID10) %>%
  select(zcta) %>%
  mutate(zcta = as.character(zcta)) %>%
  right_join(all_impacts, by="zcta") %>% 
  st_transform(crs = ll_wgs84)

hs1_ac_mort_sf <- filter(hs1_zcta_sf, outcome == "mort_ac")
plot(hs1_ac_mort_sf["rate"])

hs1_na_mort_sf <- filter(hs1_zcta_sf, outcome == "st_mort_na")
plot(hs1_na_mort_sf["rate"])

hs1_ac_mort <- ggplot() +
  geom_sf(data = hs1_ac_mort_sf, aes(fill = rate), inherit.aes = F,
          color = NA) +
  geom_text_repel(data = pp_sf, aes(label = id, geometry = geometry),
                  stat = "sf_coordinates", direction = "x", nudge_x = 2,
                  colour = "red", segment.colour = "red", segment.size = 0.3,
                  inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Avoided deaths\nper 10,000") + 
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
           dist = 60, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -102.6, y = 37.0), st.dist = 0.025)
hs1_ac_mort


hs1_na_mort <- ggplot() +
  geom_sf(data = hs1_na_mort_sf, aes(fill = rate), inherit.aes = F,
          color = NA) +
  geom_text_repel(data = pp_sf, aes(label = id, geometry = geometry),
                  stat = "sf_coordinates", direction = "x", nudge_x = 2,
                  colour = "red", segment.colour = "red", segment.size = 0.3,
                  inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Avoided deaths\nper 10,000") + 
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
           dist = 60, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -102.6, y = 37.0), st.dist = 0.025)
hs1_na_mort

#' Health scenario 2 (s == 9 & 10)
s <- 9

load(paste("./HIA Outputs/", pre[s], "zcta_impacts.RData", sep=""))
out_df_1 <- out_df

load(paste("./HIA Outputs/", pre[s+1], "zcta_impacts.RData", sep=""))
out_df_2 <- out_df

rm(out_df)

combined_df <- bind_rows(out_df_1, out_df_2) %>%
  group_by(zcta, pol, outcome) %>%
  summarise(median = sum(median, na.rm=T),
            p2.5 = sum(p2.5, na.rm=T),
            p97.5 = sum(p97.5, na.rm=T),
            median_scaled = sum(median_scaled, na.rm=T),
            p2.5_scaled = sum(p2.5_scaled, na.rm=T),
            p97.5_scaled = sum(p97.5_scaled, na.rm=T),
            median_value = sum(median_value, na.rm=T),
            p2.5_value = sum(p2.5_value, na.rm=T),
            p97.5_value = sum(p97.5_value, na.rm=T))

#' Summarize impacts
total_df <- combined_df %>%
  group_by(pol, outcome) %>%
  summarise(median = round(sum(median, na.rm=T),2),
            p2.5 = round(sum(p2.5, na.rm=T),2),
            p97.5 = round(sum(p97.5, na.rm=T),2),
            median_scaled = round(sum(median_scaled, na.rm=T),0),
            p2.5_scaled = round(sum(p2.5_scaled, na.rm=T),0),
            p97.5_scaled = round(sum(p97.5_scaled, na.rm=T),0),
            median_value = round(sum(median_value, na.rm=T),0),
            p2.5_value = round(sum(p2.5_value, na.rm=T),0),
            p97.5_value = round(sum(p97.5_value, na.rm=T),0))

#' Map outcomes
#' Load the inequality indicators
ses_file <- "./HIA Inputs/ses indicators.txt"
ses <- read.table(ses_file, header=T, stringsAsFactors = F) %>%
  mutate(GEOID = gsub("86000US", "", GEOID)) %>%
  dplyr::rename(zcta = GEOID) %>%
  dplyr::select(zcta, total_pop, pct_poc, pct_nhw, med_income, 
                pct_hs_grad, pct_employed, pct_hh_not_limited_eng)

#' Clean up median impacts and calculate benefit rate
impacts <- ungroup(combined_df) %>%
  left_join(ses, by="zcta") %>% 
  select(zcta, pol, outcome, median_scaled, total_pop) %>% 
  mutate(pol = as.character(pol)) %>%
  mutate(rate = (median_scaled / total_pop) * rate_pop)

zcta_ids <- unique(impacts$zcta)

#' Map outcome rates across ZCTA's
load("./Data/Spatial Data/co_zcta.RData")

hs2_zcta_sf <- filter(co_zcta, GEOID10 %in% zcta_ids) %>%
  dplyr::rename(zcta = GEOID10) %>%
  select(zcta) %>%
  mutate(zcta = as.character(zcta)) %>%
  right_join(impacts, by="zcta") %>% 
  st_transform(crs = ll_wgs84)

hs2_ac_mort_sf <- filter(hs2_zcta_sf, outcome == "mort_ac")
plot(hs2_ac_mort_sf["rate"])

hs2_na_mort_sf <- filter(hs2_zcta_sf, outcome == "st_mort_na")
plot(hs2_na_mort_sf["rate"])

hs2_ac_mort <- ggplot() +
  geom_sf(data = hs2_ac_mort_sf, aes(fill = rate), inherit.aes = F,
          color = NA) +
  geom_text_repel(data = pp_sf, aes(label = id, geometry = geometry),
                  stat = "sf_coordinates", direction = "x", nudge_x = 2,
                  colour = "red", segment.colour = "red", segment.size = 0.3,
                  inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Avoided deaths\nper 10,000") + 
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
           dist = 60, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -102.6, y = 37.0), st.dist = 0.025)


hs2_na_mort <- ggplot() +
  geom_sf(data = hs2_na_mort_sf, aes(fill = rate), inherit.aes = F,
          color = NA) +
  geom_text_repel(data = pp_sf, aes(label = id, geometry = geometry),
                  stat = "sf_coordinates", direction = "x", nudge_x = 2,
                  colour = "red", segment.colour = "red", segment.size = 0.3,
                  inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Avoided deaths\nper 10,000") + 
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
           dist = 60, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -102.6, y = 37.0), st.dist = 0.025)

#' Combine them!
library(ggpubr)

hs1_mortality <- ggarrange(hs1_ac_mort, hs1_na_mort, 
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1)
hs1_mortality
ggsave(hs1_mortality,
       filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/HS1_Avoided_Deaths.jpeg", 
       device = "jpeg", dpi=500, units = "in", height = 6, width = 8)

hs2_mortality <- ggarrange(hs2_ac_mort, hs2_na_mort, 
                           labels = c("A", "B"),
                           ncol = 2, nrow = 1)
hs2_mortality
ggsave(hs2_mortality,
       filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/HS2_Avoided_Deaths.jpeg", 
       device = "jpeg", dpi=500, units = "in", height = 6, width = 8)


#' -----------------------------------------------------------------------------
#' SES variables and impacts for Pueblo and Colorado Springs
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
  dplyr::select(zcta, total_pop, pct_poc, pct_nhw, med_income,
                pct_less_hs, pct_unemployed, pct_hh_limited_eng)

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
pueblo_nhw <- ggplot(data = pueblo_impacts) +
  geom_sf(data = pueblo_impacts, aes(fill = pct_nhw), color = NA) +
  # geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Comanche"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Percentage") +
  map_theme2 +
  theme(legend.position = c(0.2, 0.8),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(37.8, 38.62),
           xlim = c(-105.2, -104.0)) +
  north(x.min = -105.2, x.max = -104.0,
        y.min =  37.8, y.max = 38.62,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -105.0, y = 37.88)) +
  scalebar(x.min = -105.2, x.max = -104.0,
           y.min =  37.8, y.max = 38.62,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.8, y = 37.8), st.dist = 0.025)
pueblo_nhw

pueblo_poc <- ggplot(data = pueblo_impacts) +
  geom_sf(data = pueblo_impacts, aes(fill = pct_poc), color = NA) +
  # geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Comanche"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Percentage") +
  map_theme2 +
  theme(legend.position = c(0.2, 0.8),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(37.8, 38.62),
           xlim = c(-105.2, -104.0)) +
  north(x.min = -105.2, x.max = -104.0,
        y.min =  37.8, y.max = 38.62,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -105.0, y = 37.88)) +
  scalebar(x.min = -105.2, x.max = -104.0,
           y.min =  37.8, y.max = 38.62,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.8, y = 37.8), st.dist = 0.025)

pueblo_income <- ggplot(data = pueblo_impacts) +
  geom_sf(data = pueblo_impacts, aes(fill = med_income/10000), color = NA) +
  # geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Comanche"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "2014$ (10,000's)") +
  map_theme2 +
  theme(legend.position = c(0.2, 0.8),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(37.8, 38.62),
           xlim = c(-105.2, -104.0)) +
  north(x.min = -105.2, x.max = -104.0,
        y.min =  37.8, y.max = 38.62,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -105.0, y = 37.88)) +
  scalebar(x.min = -105.2, x.max = -104.0,
           y.min =  37.8, y.max = 38.62,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.8, y = 37.8), st.dist = 0.025)

pueblo_low_ed <- ggplot(data = pueblo_impacts) +
  geom_sf(data = pueblo_impacts, aes(fill = pct_less_hs), color = NA) +
  # geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Comanche"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  geom_sf_text(data = pp_sf, aes(label = id), inherit.aes = F,
               nudge_y = -0.05, color = "red") +
  scale_fill_viridis(name = "Percentage") +
  map_theme2 +
  theme(legend.position = c(0.2, 0.8),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(37.8, 38.62),
           xlim = c(-105.2, -104.0)) +
  north(x.min = -105.2, x.max = -104.0,
        y.min =  37.8, y.max = 38.62,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -105.0, y = 37.88)) +
  scalebar(x.min = -105.2, x.max = -104.0,
           y.min =  37.8, y.max = 38.62,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.8, y = 37.8), st.dist = 0.025)

pueblo_unemp <- ggplot(data = pueblo_impacts) +
  geom_sf(data = pueblo_impacts, aes(fill = pct_unemployed), color = NA) +
  # geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Comanche"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Percentage") +
  map_theme2 +
  theme(legend.position = c(0.2, 0.8),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(37.8, 38.62),
           xlim = c(-105.2, -104.0)) +
  north(x.min = -105.2, x.max = -104.0,
        y.min =  37.8, y.max = 38.62,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -105.0, y = 37.88)) +
  scalebar(x.min = -105.2, x.max = -104.0,
           y.min =  37.8, y.max = 38.62,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.8, y = 37.8), st.dist = 0.025)

pueblo_limited_eng <- ggplot(data = pueblo_impacts) +
  geom_sf(data = pueblo_impacts, aes(fill = pct_hh_limited_eng), color = NA) +
  # geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Comanche"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Percentage") +
  map_theme2 +
  theme(legend.position = c(0.2, 0.8),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(37.8, 38.62),
           xlim = c(-105.2, -104.0)) +
  north(x.min = -105.2, x.max = -104.0,
        y.min =  37.8, y.max = 38.62,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -105.0, y = 37.88)) +
  scalebar(x.min = -105.2, x.max = -104.0,
           y.min =  37.8, y.max = 38.62,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.8, y = 37.8), st.dist = 0.025)

pueblo_ac_mort_sf <- filter(pueblo_impacts, outcome == "mort_ac")
pueblo_ac_mort <- ggplot() +
  geom_sf(data = pueblo_ac_mort_sf, aes(fill = rate), color = NA) +
  # geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Comanche"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Avoided deaths per 10,000") +
  map_theme2 +
  theme(legend.position = c(0.25, 0.8),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(37.8, 38.62),
           xlim = c(-105.2, -104.0)) +
  north(x.min = -105.2, x.max = -104.0,
        y.min =  37.8, y.max = 38.62,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -105.0, y = 37.88)) +
  scalebar(x.min = -105.2, x.max = -104.0,
           y.min =  37.8, y.max = 38.62,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.8, y = 37.8), st.dist = 0.025)

pueblo_na_mort_sf <- filter(pueblo_impacts, outcome == "st_mort_na")
pueblo_na_mort <- ggplot() +
  geom_sf(data = pueblo_na_mort_sf, aes(fill = rate), color = NA) +
  # geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Comanche"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Avoided deaths per 10,000") +
  map_theme2 +
  theme(legend.position = c(0.25, 0.8),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(37.8, 38.62),
           xlim = c(-105.2, -104.0)) +
  north(x.min = -105.2, x.max = -104.0,
        y.min =  37.8, y.max = 38.62,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -105.0, y = 37.88)) +
  scalebar(x.min = -105.2, x.max = -104.0,
           y.min =  37.8, y.max = 38.62,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.8, y = 37.8), st.dist = 0.025)

pueblo_cvd_sf <- filter(pueblo_impacts, outcome == "hosp_cvd")
pueblo_cvd <- ggplot() +
  geom_sf(data = pueblo_cvd_sf, aes(fill = rate), color = NA) +
  # geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Comanche"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Avoided cases per 10,000") +
  map_theme2 +
  theme(legend.position = c(0.25, 0.8),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(37.8, 38.62),
           xlim = c(-105.2, -104.0)) +
  north(x.min = -105.2, x.max = -104.0,
        y.min =  37.8, y.max = 38.62,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -105.0, y = 37.88)) +
  scalebar(x.min = -105.2, x.max = -104.0,
           y.min =  37.8, y.max = 38.62,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.8, y = 37.8), st.dist = 0.025)

pueblo_res_sf <- filter(pueblo_impacts, outcome == "hosp_res")
pueblo_res <- ggplot() +
  geom_sf(data = pueblo_res_sf, aes(fill = rate), color = NA) +
  # geom_sf(data = pueblo, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Comanche"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = 0.2, nudge_y = 0.2,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Avoided cases per 10,000") +
  map_theme2 +
  theme(legend.position = c(0.25, 0.8),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(37.8, 38.62),
           xlim = c(-105.2, -104.0)) +
  north(x.min = -105.2, x.max = -104.0,
        y.min =  37.8, y.max = 38.62,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -105.0, y = 37.88)) +
  scalebar(x.min = -105.2, x.max = -104.0,
           y.min =  37.8, y.max = 38.62,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.8, y = 37.8), st.dist = 0.025)


#' Maps for Colorado Springs
springs_nhw <- ggplot(data = springs_impacts) +
  geom_sf(data = springs_impacts, aes(fill = pct_nhw), color = NA) +
  # geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Martin Drake"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = -0.2, nudge_y = -0.1,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Percentage") +
  map_theme2 +
  theme(legend.position = c(0.8, 0.3),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(38.5, 39.2),
           xlim = c(-105.1, -104.3)) +
  north(x.min = -105.1, x.max = -104.3,
        y.min =  38.5, y.max = 39.2,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -104.3, y = 38.55)) +
  scalebar(x.min = -105.1, x.max = -104.3,
           y.min =  38.5, y.max = 39.2,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.3, y = 38.5), st.dist = 0.025)

springs_poc <- ggplot(data = springs_impacts) +
  geom_sf(data = springs_impacts, aes(fill = pct_poc), color = NA) +
  # geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Martin Drake"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = -0.2, nudge_y = -0.1,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Percentage") +
  map_theme2 +
  theme(legend.position = c(0.8, 0.3),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(38.5, 39.2),
           xlim = c(-105.1, -104.3)) +
  north(x.min = -105.1, x.max = -104.3,
        y.min =  38.5, y.max = 39.2,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -104.3, y = 38.55)) +
  scalebar(x.min = -105.1, x.max = -104.3,
           y.min =  38.5, y.max = 39.2,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.3, y = 38.5), st.dist = 0.025)
springs_poc

springs_income <- ggplot(data = springs_impacts) +
  geom_sf(data = springs_impacts, aes(fill = med_income/10000), color = NA) +
  # geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Martin Drake"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = -0.2, nudge_y = -0.1,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "2014$ (10,000's)") +
  map_theme2 +
  theme(legend.position = c(0.8, 0.3),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(38.5, 39.2),
           xlim = c(-105.1, -104.3)) +
  north(x.min = -105.1, x.max = -104.3,
        y.min =  38.5, y.max = 39.2,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -104.3, y = 38.55)) +
  scalebar(x.min = -105.1, x.max = -104.3,
           y.min =  38.5, y.max = 39.2,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.3, y = 38.5), st.dist = 0.025)

springs_low_ed <- ggplot(data = springs_impacts) +
  geom_sf(data = springs_impacts, aes(fill = pct_less_hs), color = NA) +
  # geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Martin Drake"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = -0.2, nudge_y = -0.1,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Percentage") +
  map_theme2 +
  theme(legend.position = c(0.8, 0.3),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(38.5, 39.2),
           xlim = c(-105.1, -104.3)) +
  north(x.min = -105.1, x.max = -104.3,
        y.min =  38.5, y.max = 39.2,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -104.3, y = 38.55)) +
  scalebar(x.min = -105.1, x.max = -104.3,
           y.min =  38.5, y.max = 39.2,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.3, y = 38.5), st.dist = 0.025)

springs_unemp <- ggplot(data = springs_impacts) +
  geom_sf(data = springs_impacts, aes(fill = pct_unemployed), color = NA) +
  # geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Martin Drake"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = -0.2, nudge_y = -0.1,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Percentage") +
  map_theme2 +
  theme(legend.position = c(0.8, 0.3),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(38.5, 39.2),
           xlim = c(-105.1, -104.3)) +
  north(x.min = -105.1, x.max = -104.3,
        y.min =  38.5, y.max = 39.2,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -104.3, y = 38.55)) +
  scalebar(x.min = -105.1, x.max = -104.3,
           y.min =  38.5, y.max = 39.2,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.3, y = 38.5), st.dist = 0.025)

springs_limited_eng <- ggplot(data = springs_impacts) +
  geom_sf(data = springs_impacts, aes(fill = pct_hh_limited_eng), color = NA) +
  # geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Martin Drake"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = -0.2, nudge_y = -0.1,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Percentage") +
  map_theme2 +
  theme(legend.position = c(0.8, 0.3),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(38.5, 39.2),
           xlim = c(-105.1, -104.3)) +
  north(x.min = -105.1, x.max = -104.3,
        y.min =  38.5, y.max = 39.2,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -104.3, y = 38.55)) +
  scalebar(x.min = -105.1, x.max = -104.3,
           y.min =  38.5, y.max = 39.2,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.3, y = 38.5), st.dist = 0.025)

springs_ac_mort_sf <- filter(springs_impacts, outcome == "mort_ac")
springs_ac_mort <- ggplot() +
  geom_sf(data = springs_ac_mort_sf, aes(fill = rate), color = NA) +
  # geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Martin Drake"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = -0.2, nudge_y = -0.1,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Avoided deaths\nper 10,000") +
  map_theme2 +
  theme(legend.position = c(0.8, 0.3),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(38.5, 39.2),
           xlim = c(-105.1, -104.3)) +
  north(x.min = -105.1, x.max = -104.3,
        y.min =  38.5, y.max = 39.2,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -104.3, y = 38.55)) +
  scalebar(x.min = -105.1, x.max = -104.3,
           y.min =  38.5, y.max = 39.2,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.3, y = 38.5), st.dist = 0.025)

springs_na_mort_sf <- filter(springs_impacts, outcome == "st_mort_na")
springs_na_mort <- ggplot() +
  geom_sf(data = springs_na_mort_sf, aes(fill = rate), color = NA) +
  # geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Martin Drake"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = -0.2, nudge_y = -0.1,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Avoided deaths\nper 10,000") +
  map_theme2 +
  theme(legend.position = c(0.8, 0.3),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(38.5, 39.2),
           xlim = c(-105.1, -104.3)) +
  north(x.min = -105.1, x.max = -104.3,
        y.min =  38.5, y.max = 39.2,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -104.3, y = 38.55)) +
  scalebar(x.min = -105.1, x.max = -104.3,
           y.min =  38.5, y.max = 39.2,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.3, y = 38.5), st.dist = 0.025)

springs_cvd_sf <- filter(springs_impacts, outcome == "hosp_cvd")
springs_cvd <- ggplot() +
  geom_sf(data = springs_cvd_sf, aes(fill = rate), color = NA) +
  # geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Martin Drake"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = -0.2, nudge_y = -0.1,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Avoided cases\nper 10,000") +
  map_theme2 +
  theme(legend.position = c(0.8, 0.3),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(38.5, 39.2),
           xlim = c(-105.1, -104.3)) +
  north(x.min = -105.1, x.max = -104.3,
        y.min =  38.5, y.max = 39.2,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -104.3, y = 38.55)) +
  scalebar(x.min = -105.1, x.max = -104.3,
           y.min =  38.5, y.max = 39.2,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.3, y = 38.5), st.dist = 0.025)

springs_res_sf <- filter(springs_impacts, outcome == "hosp_res")
springs_res <- ggplot() +
  geom_sf(data = springs_res_sf, aes(fill = rate), color = NA) +
  # geom_sf(data = springs, fill=NA, color="red") +
  geom_sf(data = pp_sf, color = "red", size = 2, inherit.aes = F, 
          show.legend = "point") +
  geom_label_repel(data = filter(pp_sf, id == "Martin Drake"), 
                   aes(label = id, geometry = geometry),
                   stat = "sf_coordinates", direction = "x", 
                   nudge_x = -0.2, nudge_y = -0.1,
                   colour = "red", segment.colour = "red", segment.size = 0.3,
                   inherit.aes = F, show.legend = F) +
  scale_fill_viridis(name = "Avoided cases\nper 10,000") +
  map_theme2 +
  theme(legend.position = c(0.8, 0.3),
        plot.margin=grid::unit(c(0,0,0,0), "mm")) +
  xlab("") + ylab("") +
  coord_sf(ylim = c(38.5, 39.2),
           xlim = c(-105.1, -104.3)) +
  north(x.min = -105.1, x.max = -104.3,
        y.min =  38.5, y.max = 39.2,
        symbol = 12, location = "bottomright", scale = 0.075,
        anchor = c(x = -104.3, y = 38.55)) +
  scalebar(x.min = -105.1, x.max = -104.3,
           y.min =  38.5, y.max = 39.2,
           dist = 10, dd2km=T, model="WGS84", st.bottom = F, st.size = 3,
           height = 0.02, anchor = c(x = -104.3, y = 38.5), st.dist = 0.025)


ses_plots <- ggarrange(
  annotate_figure(ggarrange(springs_nhw, springs_income, 
                            labels = c("A: Non-Hispanic White", "B: Median income"), ncol = 2, nrow = 1),
                  left = text_grob("Colorado Springs", rot = 90, face = "bold")),
  annotate_figure(ggarrange(pueblo_nhw, pueblo_income, 
                            labels = c("C: Non-Hispanic White", "D: Median income"), 
                            ncol = 2, nrow = 1),
                  left = text_grob("Pueblo", rot = 90, face = "bold")),
  ncol = 1, nrow = 2)
ses_plots
ggsave(ses_plots,
       filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/SES_plots.jpeg", 
       device = "jpeg", dpi=500, units = "in", height = 8, width = 8)

sup_ses_plots <- ggarrange(
  annotate_figure(ggarrange(springs_low_ed, springs_limited_eng, springs_unemp, 
                            labels = c("A: Low education", "B: Limited English",
                                       "C: Unemployment"), 
                            ncol = 3, nrow = 1),
                  left = text_grob("Colorado Springs", rot = 90, face = "bold")),
  annotate_figure(ggarrange(pueblo_low_ed, pueblo_limited_eng, pueblo_unemp, 
                            labels = c("D: Low education", "E: Limited English",
                                       "F: Unemployment"), 
                            ncol = 3, nrow = 1),
                  left = text_grob("Pueblo", rot = 90, face = "bold")),
  ncol = 1, nrow = 2)
sup_ses_plots
ggsave(sup_ses_plots,
       filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Supplement_SES_plots.jpeg", 
       device = "jpeg", dpi=500, units = "in", height = 8, width = 12)

mort_plots <- ggarrange(
  annotate_figure(ggarrange(springs_ac_mort, springs_na_mort, 
                            labels = c("A: All-cause", "B: Non-accidental"), ncol = 2, nrow = 1),
                  left = text_grob("Colorado Springs", rot = 90, face = "bold")),
  annotate_figure(ggarrange(pueblo_ac_mort, pueblo_na_mort, 
                            labels = c("C: All-cause", "D: Non-accidental"), 
                            ncol = 2, nrow = 1),
                  left = text_grob("Pueblo", rot = 90, face = "bold")),
  ncol = 1, nrow = 2)
mort_plots
ggsave(mort_plots,
       filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Mortality_plots.jpeg", 
       device = "jpeg", dpi=500, units = "in", height = 8, width = 8)


sup_hosp_plots <- ggarrange(
  annotate_figure(ggarrange(springs_res, springs_cvd, 
                            labels = c("A: Respiratory", "B: Cardiovascular"),
                            ncol = 2, nrow = 1),
                  left = text_grob("Colorado Springs", rot = 90, face = "bold")),
  annotate_figure(ggarrange(pueblo_res, pueblo_cvd, 
                            labels = c("C: Respiratory", "D: Cardiovascular"), 
                            ncol = 2, nrow = 1),
                  left = text_grob("Pueblo", rot = 90, face = "bold")),

  ncol = 1, nrow = 2)
sup_hosp_plots
ggsave(sup_hosp_plots,
       filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Hospitalization_plots.jpeg", 
       device = "jpeg", dpi=500, units = "in", height = 8, width = 8)

#' -----------------------------------------------------------------------------
#' Concentration index plots
#' -----------------------------------------------------------------------------

library(IC2)

rate_pop <- 10000

#' Health scenario 1 (s == 7 & 8)
s <- 7

load(paste("./HIA Outputs/", pre[s], "zcta_impacts.RData", sep=""))
out_df_1 <- out_df

load(paste("./HIA Outputs/", pre[s+1], "zcta_impacts.RData", sep=""))
out_df_2 <- out_df

rm(out_df)

ses_file <- "./HIA Inputs/ses indicators.txt"
ses <- read.table(ses_file, header=T, stringsAsFactors = F) %>%
  mutate(GEOID = gsub("86000US", "", GEOID)) %>%
  dplyr::rename(zcta = GEOID)

combined_df <- bind_rows(out_df_1, out_df_2) %>%
  group_by(zcta, pol, outcome) %>%
  summarise(median = sum(median, na.rm=T),
            p2.5 = sum(p2.5, na.rm=T),
            p97.5 = sum(p97.5, na.rm=T),
            median_scaled = sum(median_scaled, na.rm=T),
            p2.5_scaled = sum(p2.5_scaled, na.rm=T),
            p97.5_scaled = sum(p97.5_scaled, na.rm=T),
            median_value = sum(median_value, na.rm=T),
            p2.5_value = sum(p2.5_value, na.rm=T),
            p97.5_value = sum(p97.5_value, na.rm=T))

#' Merge median impacts with indicators of inequality
impacts2 <- ungroup(combined_df) %>%
  select(zcta, pol, outcome, median_scaled) %>%
  mutate(pol = as.character(pol)) %>%
  left_join(ses, by="zcta") %>% 
  gather(ses_indic, ses_value, pct_under5:med_income) %>%
  mutate(rate = (median_scaled / total_pop) * rate_pop) %>% 
  mutate(rate = ifelse(rate < 0, NA, rate))

#' Springs ZCTAs and Pueblo ZCTAs
springs_zips <- c("80938", "80939", "80951", "80918", "80919", "80920",
                  "80921", "80922", "80923", "80924", "80925", "80926",
                  "80927", "80929", "80903", "80904", "80905", "80906", 
                  "80907", "80908", "80909", "80910", "80911", "80914",
                  "80915", "80916", "80917", "80809", "80829", "80831",
                  "80840", "80913", "80817", "80925", "80929")

springs_impacts <- filter(impacts2, zcta %in% springs_zips)

pueblo_zips <- c("81001", "81003", "81004", "81005", "81006",
                 "81007", "81008", "81022", "81025")

pueblo_impacts <- filter(impacts2, zcta %in% pueblo_zips)

concentration <- impacts2  %>%
  select(pol, outcome, ses_indic, ses_value, rate) %>%
  mutate(rate = ifelse(rate < 0, NA, rate)) %>%
  group_by(pol, outcome, ses_indic) %>%
  summarise(CI_rate = calcSConc(rate, ses_value)[[1]][[1]]) 

#' ALL ZCTAS
#' All-cause mortality plots
mort_nhw <- filter(impacts2, ses_indic == "pct_nhw") %>% 
  filter(outcome == "mort_ac")
mort_hs_grad <- filter(impacts2, ses_indic == "pct_hs_grad") %>% 
  filter(outcome == "mort_ac")
mort_employed <- filter(impacts2, ses_indic == "pct_employed") %>% 
  filter(outcome == "mort_ac")
mort_eng <- filter(impacts2, ses_indic == "pct_hh_not_limited_eng") %>% 
  filter(outcome == "mort_ac")
mort_income <- filter(impacts2, ses_indic == "med_income") %>% 
  filter(outcome == "mort_ac")

jpeg(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/AC_Mort_Conc_Curves.jpeg",
     width = 5, height = 5, units = "in", res = 500)
curveConcent(mort_nhw$rate, mort_nhw$ses_value, col="red", lty = 1,
             xlab = paste("Social advantage rank"),
             ylab = paste("Cumulative benefits"))
curveConcent(mort_hs_grad$rate, mort_hs_grad$ses_value, col="black", lty = 2, add = T)
curveConcent(mort_employed$rate, mort_employed$ses_value, col="blue", lty = 3, add = T)
curveConcent(mort_eng$rate, mort_eng$ses_value, col="darkgreen", lty = 4, add = T)
curveConcent(mort_income$rate, mort_income$ses_value, col="orange", lty = 5, add = T)
legend(x = 0.48, y = 0.38, cex = 0.6,
       title = "Indicator of ZCTA-level social advantage",
       col = c("red", "black", "blue", "darkgreen", "orange"),
       lty = c(1, 2, 3, 4, 5),
       legend = c("Non-Hispanic white population",
                  "High school graduates",
                  "Employment",
                  "Proficiency in English",
                  "Median income (2014$)"))
text(x = 0.05, y = 0.9, labels = c("A: All-cause mortality"), pos = 4)
dev.off()

#' non-accidental mortality plots
na_mort_nhw <- filter(impacts2, ses_indic == "pct_nhw") %>% 
  filter(outcome == "st_mort_na")
na_mort_hs_grad <- filter(impacts2, ses_indic == "pct_hs_grad") %>% 
  filter(outcome == "st_mort_na")
na_mort_employed <- filter(impacts2, ses_indic == "pct_employed") %>% 
  filter(outcome == "st_mort_na")
na_mort_eng <- filter(impacts2, ses_indic == "pct_hh_not_limited_eng") %>% 
  filter(outcome == "st_mort_na")
na_mort_income <- filter(impacts2, ses_indic == "med_income") %>% 
  filter(outcome == "st_mort_na")

jpeg(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/NA_Mort_Conc_Curves.jpeg",
     width = 5, height = 5, units = "in", res = 500)
curveConcent(na_mort_nhw$rate, na_mort_nhw$ses_value, col="red", lty = 1,
             xlab = paste("Social advantage rank"),
             ylab = paste("Cumulative benefits"))
curveConcent(na_mort_hs_grad$rate, na_mort_hs_grad$ses_value, col="black", lty = 2, add = T)
curveConcent(na_mort_employed$rate, na_mort_employed$ses_value, col="blue", lty = 3, add = T)
curveConcent(na_mort_eng$rate, na_mort_eng$ses_value, col="darkgreen", lty = 4, add = T)
curveConcent(na_mort_income$rate, na_mort_income$ses_value, col="orange", lty = 5, add = T)
# legend(x = 0.48, y = 0.38, cex = 0.6,
#        title = "Indicator of ZCTA-level social advantage",
#        col = c("red", "black", "blue", "darkgreen", "orange"),
#        lty = c(1, 2, 3, 4, 5),
#        legend = c("Non-Hispanic white population",
#                   "High school graduates",
#                   "Employment",
#                   "Proficiency in English",
#                   "Median income (2014$)"))
text(x = 0.05, y = 0.9, labels = c("B: Non-accidental mortality"), pos = 4)
dev.off()

#' respiratory hosp plots
res_nhw <- filter(impacts2, ses_indic == "pct_nhw") %>% 
  filter(outcome == "hosp_res")
res_hs_grad <- filter(impacts2, ses_indic == "pct_hs_grad") %>% 
  filter(outcome == "hosp_res")
res_employed <- filter(impacts2, ses_indic == "pct_employed") %>% 
  filter(outcome == "hosp_res")
res_eng <- filter(impacts2, ses_indic == "pct_hh_not_limited_eng") %>% 
  filter(outcome == "hosp_res")
res_income <- filter(impacts2, ses_indic == "med_income") %>% 
  filter(outcome == "hosp_res")

jpeg(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Res_Conc_Curves.jpeg",
     width = 5, height = 5, units = "in", res = 500)
curveConcent(res_nhw$rate, res_nhw$ses_value, col="red", lty = 1,
             xlab = paste("Social advantage rank"),
             ylab = paste("Cumulative benefits"))
curveConcent(res_hs_grad$rate, res_hs_grad$ses_value, col="black", lty = 2, add = T)
curveConcent(res_employed$rate, res_employed$ses_value, col="blue", lty = 3, add = T)
curveConcent(res_eng$rate, res_eng$ses_value, col="darkgreen", lty = 4, add = T)
curveConcent(res_income$rate, res_income$ses_value, col="orange", lty = 5, add = T)
legend(x = 0.48, y = 0.38, cex = 0.6,
       title = "Indicator of ZCTA-level social advantage",
       col = c("red", "black", "blue", "darkgreen", "orange"),
       lty = c(1, 2, 3, 4, 5),
       legend = c("Non-Hispanic white population",
                  "High school graduates",
                  "Employment",
                  "Proficiency in English",
                  "Median income (2014$)"))
text(x = 0.05, y = 0.9, labels = c("A: Respiratory hospitalizations"), pos = 4)
dev.off()

#' cvd hosp plots
cvd_nhw <- filter(impacts2, ses_indic == "pct_nhw") %>% 
  filter(outcome == "hosp_cvd")
cvd_hs_grad <- filter(impacts2, ses_indic == "pct_hs_grad") %>% 
  filter(outcome == "hosp_cvd")
cvd_employed <- filter(impacts2, ses_indic == "pct_employed") %>% 
  filter(outcome == "hosp_cvd")
cvd_eng <- filter(impacts2, ses_indic == "pct_hh_not_limited_eng") %>% 
  filter(outcome == "hosp_cvd")
cvd_income <- filter(impacts2, ses_indic == "med_income") %>% 
  filter(outcome == "hosp_cvd")

jpeg(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/CVD_Conc_Curves.jpeg",
     width = 5, height = 5, units = "in", res = 500)
curveConcent(cvd_nhw$rate, cvd_nhw$ses_value, col="red", lty = 1,
             xlab = paste("Social advantage rank"),
             ylab = paste("Cumulative benefits"))
curveConcent(cvd_hs_grad$rate, cvd_hs_grad$ses_value, col="black", lty = 2, add = T)
curveConcent(cvd_employed$rate, cvd_employed$ses_value, col="blue", lty = 3, add = T)
curveConcent(cvd_eng$rate, cvd_eng$ses_value, col="darkgreen", lty = 4, add = T)
curveConcent(cvd_income$rate, cvd_income$ses_value, col="orange", lty = 5, add = T)
# legend(x = 0.48, y = 0.38, cex = 0.6,
#        title = "Indicator of ZCTA-level social advantage",
#        col = c("red", "black", "blue", "darkgreen", "orange"),
#        lty = c(1, 2, 3, 4, 5),
#        legend = c("Non-Hispanic white population",
#                   "High school graduates",
#                   "Employment",
#                   "Proficiency in English",
#                   "Median income (2014$)"))
text(x = 0.05, y = 0.9, labels = c("B: Cardiovascular hospitalizations"), pos = 4)
dev.off()

#' SPRINGS ZCTAS
#' All-cause mortality plots
mort_nhw <- filter(springs_impacts, ses_indic == "pct_nhw") %>% 
  filter(outcome == "mort_ac")
mort_hs_grad <- filter(springs_impacts, ses_indic == "pct_hs_grad") %>% 
  filter(outcome == "mort_ac")
mort_employed <- filter(springs_impacts, ses_indic == "pct_employed") %>% 
  filter(outcome == "mort_ac")
mort_eng <- filter(springs_impacts, ses_indic == "pct_hh_not_limited_eng") %>% 
  filter(outcome == "mort_ac")
mort_income <- filter(springs_impacts, ses_indic == "med_income") %>% 
  filter(outcome == "mort_ac")

jpeg(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Springs_AC_Mort_Conc_Curves.jpeg",
     width = 5, height = 5, units = "in", res = 500)
curveConcent(mort_nhw$rate, mort_nhw$ses_value, col="red", lty = 1,
             xlab = paste("Social advantage rank"),
             ylab = paste("Cumulative benefits"))
curveConcent(mort_hs_grad$rate, mort_hs_grad$ses_value, col="black", lty = 2, add = T)
curveConcent(mort_employed$rate, mort_employed$ses_value, col="blue", lty = 3, add = T)
curveConcent(mort_eng$rate, mort_eng$ses_value, col="darkgreen", lty = 4, add = T)
curveConcent(mort_income$rate, mort_income$ses_value, col="orange", lty = 5, add = T)
legend(x = 0.48, y = 0.38, cex = 0.6,
       title = "Indicator of ZCTA-level social advantage",
       col = c("red", "black", "blue", "darkgreen", "orange"),
       lty = c(1, 2, 3, 4, 5),
       legend = c("Non-Hispanic white population",
                  "High school graduates",
                  "Employment",
                  "Proficiency in English",
                  "Median income (2014$)"))
text(x = 0.05, y = 0.9, labels = c("A: All-cause mortality"), pos = 4)
dev.off()

#' non-accidental mortality plots
na_mort_nhw <- filter(springs_impacts, ses_indic == "pct_nhw") %>% 
  filter(outcome == "st_mort_na")
na_mort_hs_grad <- filter(springs_impacts, ses_indic == "pct_hs_grad") %>% 
  filter(outcome == "st_mort_na")
na_mort_employed <- filter(springs_impacts, ses_indic == "pct_employed") %>% 
  filter(outcome == "st_mort_na")
na_mort_eng <- filter(springs_impacts, ses_indic == "pct_hh_not_limited_eng") %>% 
  filter(outcome == "st_mort_na")
na_mort_income <- filter(springs_impacts, ses_indic == "med_income") %>% 
  filter(outcome == "st_mort_na")

jpeg(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Springs_NA_Mort_Conc_Curves.jpeg",
     width = 5, height = 5, units = "in", res = 500)
curveConcent(na_mort_nhw$rate, na_mort_nhw$ses_value, col="red", lty = 1,
             xlab = paste("Social advantage rank"),
             ylab = paste("Cumulative benefits"))
curveConcent(na_mort_hs_grad$rate, na_mort_hs_grad$ses_value, col="black", lty = 2, add = T)
curveConcent(na_mort_employed$rate, na_mort_employed$ses_value, col="blue", lty = 3, add = T)
curveConcent(na_mort_eng$rate, na_mort_eng$ses_value, col="darkgreen", lty = 4, add = T)
curveConcent(na_mort_income$rate, na_mort_income$ses_value, col="orange", lty = 5, add = T)
# legend(x = 0.48, y = 0.38, cex = 0.6,
#        title = "Indicator of ZCTA-level social advantage",
#        col = c("red", "black", "blue", "darkgreen", "orange"),
#        lty = c(1, 2, 3, 4, 5),
#        legend = c("Non-Hispanic white population",
#                   "High school graduates",
#                   "Employment",
#                   "Proficiency in English",
#                   "Median income (2014$)"))
text(x = 0.05, y = 0.9, labels = c("B: Non-accidental mortality"), pos = 4)
dev.off()

#' respiratory hosp plots
res_nhw <- filter(springs_impacts, ses_indic == "pct_nhw") %>% 
  filter(outcome == "hosp_res")
res_hs_grad <- filter(springs_impacts, ses_indic == "pct_hs_grad") %>% 
  filter(outcome == "hosp_res")
res_employed <- filter(springs_impacts, ses_indic == "pct_employed") %>% 
  filter(outcome == "hosp_res")
res_eng <- filter(springs_impacts, ses_indic == "pct_hh_not_limited_eng") %>% 
  filter(outcome == "hosp_res")
res_income <- filter(springs_impacts, ses_indic == "med_income") %>% 
  filter(outcome == "hosp_res")

jpeg(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Springs_Res_Conc_Curves.jpeg",
     width = 5, height = 5, units = "in", res = 500)
curveConcent(res_nhw$rate, res_nhw$ses_value, col="red", lty = 1,
             xlab = paste("Social advantage rank"),
             ylab = paste("Cumulative benefits"))
curveConcent(res_hs_grad$rate, res_hs_grad$ses_value, col="black", lty = 2, add = T)
curveConcent(res_employed$rate, res_employed$ses_value, col="blue", lty = 3, add = T)
curveConcent(res_eng$rate, res_eng$ses_value, col="darkgreen", lty = 4, add = T)
curveConcent(res_income$rate, res_income$ses_value, col="orange", lty = 5, add = T)
legend(x = 0.48, y = 0.38, cex = 0.6,
       title = "Indicator of ZCTA-level social advantage",
       col = c("red", "black", "blue", "darkgreen", "orange"),
       lty = c(1, 2, 3, 4, 5),
       legend = c("Non-Hispanic white population",
                  "High school graduates",
                  "Employment",
                  "Proficiency in English",
                  "Median income (2014$)"))
text(x = 0.05, y = 0.9, labels = c("A: Respiratory hospitalizations"), pos = 4)
dev.off()

#' cvd hosp plots
cvd_nhw <- filter(springs_impacts, ses_indic == "pct_nhw") %>% 
  filter(outcome == "hosp_cvd")
cvd_hs_grad <- filter(springs_impacts, ses_indic == "pct_hs_grad") %>% 
  filter(outcome == "hosp_cvd")
cvd_employed <- filter(springs_impacts, ses_indic == "pct_employed") %>% 
  filter(outcome == "hosp_cvd")
cvd_eng <- filter(springs_impacts, ses_indic == "pct_hh_not_limited_eng") %>% 
  filter(outcome == "hosp_cvd")
cvd_income <- filter(springs_impacts, ses_indic == "med_income") %>% 
  filter(outcome == "hosp_cvd")

jpeg(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Springs_CVD_Conc_Curves.jpeg",
     width = 5, height = 5, units = "in", res = 500)
curveConcent(cvd_nhw$rate, cvd_nhw$ses_value, col="red", lty = 1,
             xlab = paste("Social advantage rank"),
             ylab = paste("Cumulative benefits"))
curveConcent(cvd_hs_grad$rate, cvd_hs_grad$ses_value, col="black", lty = 2, add = T)
curveConcent(cvd_employed$rate, cvd_employed$ses_value, col="blue", lty = 3, add = T)
curveConcent(cvd_eng$rate, cvd_eng$ses_value, col="darkgreen", lty = 4, add = T)
curveConcent(cvd_income$rate, cvd_income$ses_value, col="orange", lty = 5, add = T)
# legend(x = 0.48, y = 0.38, cex = 0.6,
#        title = "Indicator of ZCTA-level social advantage",
#        col = c("red", "black", "blue", "darkgreen", "orange"),
#        lty = c(1, 2, 3, 4, 5),
#        legend = c("Non-Hispanic white population",
#                   "High school graduates",
#                   "Employment",
#                   "Proficiency in English",
#                   "Median income (2014$)"))
text(x = 0.05, y = 0.9, labels = c("B: Cardiovascular hospitalizations"), pos = 4)
dev.off()

#' PUEBLO ZCTAS
#' All-cause mortality plots
mort_nhw <- filter(pueblo_impacts, ses_indic == "pct_nhw") %>% 
  filter(outcome == "mort_ac")
mort_hs_grad <- filter(pueblo_impacts, ses_indic == "pct_hs_grad") %>% 
  filter(outcome == "mort_ac")
mort_employed <- filter(pueblo_impacts, ses_indic == "pct_employed") %>% 
  filter(outcome == "mort_ac")
mort_eng <- filter(pueblo_impacts, ses_indic == "pct_hh_not_limited_eng") %>% 
  filter(outcome == "mort_ac")
mort_income <- filter(pueblo_impacts, ses_indic == "med_income") %>% 
  filter(outcome == "mort_ac")

jpeg(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Pueblo_AC_Mort_Conc_Curves.jpeg",
     width = 5, height = 5, units = "in", res = 500)
curveConcent(mort_nhw$rate, mort_nhw$ses_value, col="red", lty = 1,
             xlab = paste("Social advantage rank"),
             ylab = paste("Cumulative benefits"))
curveConcent(mort_hs_grad$rate, mort_hs_grad$ses_value, col="black", lty = 2, add = T)
curveConcent(mort_employed$rate, mort_employed$ses_value, col="blue", lty = 3, add = T)
curveConcent(mort_eng$rate, mort_eng$ses_value, col="darkgreen", lty = 4, add = T)
curveConcent(mort_income$rate, mort_income$ses_value, col="orange", lty = 5, add = T)
legend(x = 0.48, y = 0.38, cex = 0.6,
       title = "Indicator of ZCTA-level social advantage",
       col = c("red", "black", "blue", "darkgreen", "orange"),
       lty = c(1, 2, 3, 4, 5),
       legend = c("Non-Hispanic white population",
                  "High school graduates",
                  "Employment",
                  "Proficiency in English",
                  "Median income (2014$)"))
text(x = 0.05, y = 0.9, labels = c("A: All-cause mortality"), pos = 4)
dev.off()

#' non-accidental mortality plots
na_mort_nhw <- filter(pueblo_impacts, ses_indic == "pct_nhw") %>% 
  filter(outcome == "st_mort_na")
na_mort_hs_grad <- filter(pueblo_impacts, ses_indic == "pct_hs_grad") %>% 
  filter(outcome == "st_mort_na")
na_mort_employed <- filter(pueblo_impacts, ses_indic == "pct_employed") %>% 
  filter(outcome == "st_mort_na")
na_mort_eng <- filter(pueblo_impacts, ses_indic == "pct_hh_not_limited_eng") %>% 
  filter(outcome == "st_mort_na")
na_mort_income <- filter(pueblo_impacts, ses_indic == "med_income") %>% 
  filter(outcome == "st_mort_na")

jpeg(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Pueblo_NA_Mort_Conc_Curves.jpeg",
     width = 5, height = 5, units = "in", res = 500)
curveConcent(na_mort_nhw$rate, na_mort_nhw$ses_value, col="red", lty = 1,
             xlab = paste("Social advantage rank"),
             ylab = paste("Cumulative benefits"))
curveConcent(na_mort_hs_grad$rate, na_mort_hs_grad$ses_value, col="black", lty = 2, add = T)
curveConcent(na_mort_employed$rate, na_mort_employed$ses_value, col="blue", lty = 3, add = T)
curveConcent(na_mort_eng$rate, na_mort_eng$ses_value, col="darkgreen", lty = 4, add = T)
curveConcent(na_mort_income$rate, na_mort_income$ses_value, col="orange", lty = 5, add = T)
# legend(x = 0.48, y = 0.38, cex = 0.6,
#        title = "Indicator of ZCTA-level social advantage",
#        col = c("red", "black", "blue", "darkgreen", "orange"),
#        lty = c(1, 2, 3, 4, 5),
#        legend = c("Non-Hispanic white population",
#                   "High school graduates",
#                   "Employment",
#                   "Proficiency in English",
#                   "Median income (2014$)"))
text(x = 0.05, y = 0.9, labels = c("B: Non-accidental mortality"), pos = 4)
dev.off()

#' respiratory hosp plots
res_nhw <- filter(pueblo_impacts, ses_indic == "pct_nhw") %>% 
  filter(outcome == "hosp_res")
res_hs_grad <- filter(pueblo_impacts, ses_indic == "pct_hs_grad") %>% 
  filter(outcome == "hosp_res")
res_employed <- filter(pueblo_impacts, ses_indic == "pct_employed") %>% 
  filter(outcome == "hosp_res")
res_eng <- filter(pueblo_impacts, ses_indic == "pct_hh_not_limited_eng") %>% 
  filter(outcome == "hosp_res")
res_income <- filter(pueblo_impacts, ses_indic == "med_income") %>% 
  filter(outcome == "hosp_res")

jpeg(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Pueblo_Res_Conc_Curves.jpeg",
     width = 5, height = 5, units = "in", res = 500)
curveConcent(res_nhw$rate, res_nhw$ses_value, col="red", lty = 1,
             xlab = paste("Social advantage rank"),
             ylab = paste("Cumulative benefits"))
curveConcent(res_hs_grad$rate, res_hs_grad$ses_value, col="black", lty = 2, add = T)
curveConcent(res_employed$rate, res_employed$ses_value, col="blue", lty = 3, add = T)
curveConcent(res_eng$rate, res_eng$ses_value, col="darkgreen", lty = 4, add = T)
curveConcent(res_income$rate, res_income$ses_value, col="orange", lty = 5, add = T)
legend(x = 0.48, y = 0.38, cex = 0.6,
       title = "Indicator of ZCTA-level social advantage",
       col = c("red", "black", "blue", "darkgreen", "orange"),
       lty = c(1, 2, 3, 4, 5),
       legend = c("Non-Hispanic white population",
                  "High school graduates",
                  "Employment",
                  "Proficiency in English",
                  "Median income (2014$)"))
text(x = 0.05, y = 0.9, labels = c("A: Respiratory hospitalizations"), pos = 4)
dev.off()

#' cvd hosp plots
cvd_nhw <- filter(pueblo_impacts, ses_indic == "pct_nhw") %>% 
  filter(outcome == "hosp_cvd")
cvd_hs_grad <- filter(pueblo_impacts, ses_indic == "pct_hs_grad") %>% 
  filter(outcome == "hosp_cvd")
cvd_employed <- filter(pueblo_impacts, ses_indic == "pct_employed") %>% 
  filter(outcome == "hosp_cvd")
cvd_eng <- filter(pueblo_impacts, ses_indic == "pct_hh_not_limited_eng") %>% 
  filter(outcome == "hosp_cvd")
cvd_income <- filter(pueblo_impacts, ses_indic == "med_income") %>% 
  filter(outcome == "hosp_cvd")

jpeg(filename = "C:/Users/semarten/Dropbox/ALA_HIA/Manuscript/Figures/Springs_CVD_Conc_Curves.jpeg",
     width = 5, height = 5, units = "in", res = 500)
curveConcent(cvd_nhw$rate, cvd_nhw$ses_value, col="red", lty = 1,
             xlab = paste("Social advantage rank"),
             ylab = paste("Cumulative benefits"))
curveConcent(cvd_hs_grad$rate, cvd_hs_grad$ses_value, col="black", lty = 2, add = T)
curveConcent(cvd_employed$rate, cvd_employed$ses_value, col="blue", lty = 3, add = T)
curveConcent(cvd_eng$rate, cvd_eng$ses_value, col="darkgreen", lty = 4, add = T)
curveConcent(cvd_income$rate, cvd_income$ses_value, col="orange", lty = 5, add = T)
# legend(x = 0.48, y = 0.38, cex = 0.6,
#        title = "Indicator of ZCTA-level social advantage",
#        col = c("red", "black", "blue", "darkgreen", "orange"),
#        lty = c(1, 2, 3, 4, 5),
#        legend = c("Non-Hispanic white population",
#                   "High school graduates",
#                   "Employment",
#                   "Proficiency in English",
#                   "Median income (2014$)"))
text(x = 0.05, y = 0.9, labels = c("B: Cardiovascular hospitalizations"), pos = 4)
dev.off()

