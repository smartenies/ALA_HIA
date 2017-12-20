#' =============================================================================
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
#' 
#' This script creates maps of the ZIP code hospitalization rates and SES/demo
#' variables that will be used in the health impact assessment
#' =============================================================================

library(foreign)
library(sp)
library(Hmisc)
library(gstat)
library(rgdal)
library(ggplot2)
library(ggmap)
library(scales)
library(ggsn)
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

simple_theme2 <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 10000) #avoid scientific notation

#' =============================================================================
#' Load data
#' =============================================================================

#' base map for the area
base_map <- get_map(location="Pueblo West, CO", maptype="terrain",
                    zoom = 8, source="google")
ggmap(base_map)

scale_bar <- ggsn::scalebar(location = "bottomright", dd2km = T, model="WGS84",
                            dist=20, st.bottom=F, st.size = 3, height=0.0125, 
                            x.min = -106, x.max = -103.075, 
                            y.min = 37.05, y.max = 39.7) 

n_arrow <- geom_segment(arrow=arrow(length=unit(4, "mm")),
                        aes(x=-103.075, xend=-103.075, y=37.2, yend=37.35),
                        color="black", size=1)
n_label <- geom_text(aes(x=-103.075, y=37.4), label="N")
                     


map_crs <- "+proj=longlat +datum=WGS84"


#' ACS data
load("./Data/ACS_2010_2014/ACS.RData")
acs$GEOID10 <- gsub("86000US", "", acs$GEOID)

#' Hospitalization data
hosp <- read.csv("./Data/CHA Data/co_zip_rate_period.csv", header=T)
hosp <- rename(hosp, c("ZIP" = "GEOID10"))
zip_list <- unique(hosp$GEOID10)

#' Hospitalization data (65+ rates)
hosp65 <- read.csv("./Data/CHA Data/co_zip_65plus_rate_period.csv", header=T)
hosp65 <- rename(hosp65, c("ZIP" = "GEOID10"))
zip_list <- unique(hosp$GEOID10)

#' Colors for cloropleth maps
col_list <- c("#d7191c", "#fdae61", "#ffff66", "#abd9e9", "#2c7bb6")

#' Spatial data
load("./Data/Spatial Data/sfr_counties_utm_map.RData")
sfr_counties <- spTransform(sfr_counties_utm, CRS(map_crs))
sfr_counties@data$id <- rownames(sfr_counties@data)
sfr_counties_polygons <- fortify(sfr_counties, region="id")
sfr_counties_map <- merge(sfr_counties_polygons, 
                              sfr_counties@data, by="id")
rm(sfr_counties_utm, sfr_counties_utm_map)

load("./Data/Spatial Data/sfr_zcta_utm_map.RData")
sfr_zcta <- spTransform(sfr_zcta_utm, CRS(map_crs))
sfr_zcta@data$id <- rownames(sfr_zcta@data)
sfr_zcta_polygons <- fortify(sfr_zcta, region="id")
sfr_zcta_map <- merge(sfr_zcta_polygons, 
                          sfr_zcta@data, by="id")
rm(sfr_zcta_utm, sfr_zcta_utm_map)

load("./Data/Spatial Data/power_plants_utm.RData")
pp <- spTransform(pp_utm, CRS(map_crs))
pp_df <- as.data.frame(pp)
pp_df$id <- as.character(pp_df$id)
rm(pp_utm)

#' =============================================================================
#' Map the study area
#' =============================================================================
area_map <- ggmap(base_map) +
  ggtitle("HIA boundaries") +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="blue", pch=17, cex=3) +
  geom_text(data=pp_df, aes(x=long, y=lat),
            label=pp_df$id, nudge_x = 0.2, nudge_y = -0.07, color="blue") +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(area_map)
ggsave(area_map, filename = "./Maps/Study Area.jpeg", device = "jpeg", 
       dpi=400, width = 6, height = 6, units="in")

#' =============================================================================
#' Map the hospitalization rates
#' =============================================================================

sfr_zip <- sfr_zcta_map[which(sfr_zcta_map$GEOID10 %in% zip_list),]

sfr_hosp <- merge(sfr_zip, hosp, by="GEOID10")

sfr_hosp$cp_breaks <- cut(sfr_hosp$cardiopulm_per_100p5y, 
                          breaks=c(0,1,2,3,4,max(sfr_hosp$cardiopulm_per_100p5y)),
                          include.lowest=T)
sfr_hosp$cvd_breaks <- cut(sfr_hosp$cvd_per_100p5y, 
                           breaks=c(0,0.5,1,1.5,2,max(sfr_hosp$cvd_per_100p5y)),
                          include.lowest=T)
sfr_hosp$res_breaks <- cut(sfr_hosp$resp_per_100p5y, 
                           breaks=c(0,0.5,1,1.5,2,max(sfr_hosp$resp_per_100p5y)),
                          include.lowest=T)

#' Cardiopulmonary hosptializations per 100 persons
cp_map <- ggmap(base_map) +
  ggtitle("Cardiopulmonary hospitalizations") +
  geom_polygon(data=sfr_hosp, aes(x=long, y=lat, 
                                  group=group, fill=cp_breaks),
               color=NA, show.legend = T, alpha=0.5) +
  geom_polygon(data=sfr_zcta_map, aes(x=long, y=lat, group=group),
               color="grey70", fill="NA", size=0.5) +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_manual(name="Hospitalizations\nper 100 persons", 
                    values=rev(col_list), na.value="grey50") +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(cp_map)
ggsave(cp_map, filename = "./Maps/CP Hospitalization Rates.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")

#' CVD hosptializations per 100 persons
cvd_map <- ggmap(base_map) +
  ggtitle("Cardiovascular hospitalizations") +
  geom_polygon(data=sfr_hosp, aes(x=long, y=lat, 
                                  group=group, fill=cvd_breaks),
               color=NA, show.legend = T, alpha=0.5) +
  geom_polygon(data=sfr_zcta_map, aes(x=long, y=lat, group=group),
               color="grey70", fill="NA", size=0.5) +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_manual(name="Hospitalizations\nper 100 persons", 
                    values=rev(col_list), na.value="grey50") +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(cvd_map)
ggsave(cvd_map, filename = "./Maps/CVD Hospitalization Rates.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")

#' Respiratory hosptializations per 100 persons
res_map <- ggmap(base_map) +
  ggtitle("Respiratory hospitalizations") +
  geom_polygon(data=sfr_hosp, aes(x=long, y=lat, 
                                  group=group, fill=res_breaks),
               color=NA, show.legend = T, alpha=0.5) +
  geom_polygon(data=sfr_zcta_map, aes(x=long, y=lat, group=group),
               color="grey70", fill="NA", size=0.5) +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_manual(name="Hospitalizations\nper 100 persons", 
                    values=rev(col_list), na.value="grey50") +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(res_map)
ggsave(res_map, filename = "./Maps/Res Hospitalization Rates.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")

#' =============================================================================
#' Map the hospitalization rates (65+ only)
#' =============================================================================

sfr_zip <- sfr_zcta_map[which(sfr_zcta_map$GEOID10 %in% zip_list),]

sfr_hosp65 <- merge(sfr_zip, hosp65, by="GEOID10")

sfr_hosp65$cp_breaks <- cut(sfr_hosp65$cardiopulm_per_100_65p5y, 
                          breaks=c(0,5,10,15,20,
                                   max(sfr_hosp65$cardiopulm_per_100_65p5y, na.rm=T)),
                          include.lowest=T)
sfr_hosp65$cvd_breaks <- cut(sfr_hosp65$cvd_per_100_65p5y, 
                           breaks=c(0,2,4,6,8,
                                    max(sfr_hosp65$cvd_per_100_65p5y, na.rm=T)),
                           include.lowest=T)
sfr_hosp65$res_breaks <- cut(sfr_hosp65$resp_per_100_65p5y, 
                           breaks=c(0,1.5,3,5.5,7,
                                    max(sfr_hosp65$resp_per_100_65p5y, na.rm=T)),
                           include.lowest=T)

#' Cardiopulmonary hosptializations per 100 persons
cp65_map <- ggmap(base_map) +
  ggtitle("Cardiopulmonary hospitalizations (adults 65 years and older)") +
  geom_polygon(data=sfr_hosp65, aes(x=long, y=lat, 
                                  group=group, fill=cp_breaks),
               color=NA, show.legend = T, alpha=0.5) +
  geom_polygon(data=sfr_zcta_map, aes(x=long, y=lat, group=group),
               color="grey70", fill="NA", size=0.5) +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_manual(name="Hospitalizations\nper 100 persons 65+", 
                    values=rev(col_list), na.value="grey50") +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(cp65_map)
ggsave(cp65_map, filename = "./Maps/CP Hospitalization Rates 65+.jpeg", 
       device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")

#' CVD hosptializations per 100 persons
cvd65_map <- ggmap(base_map) +
  ggtitle("Cardiovascular hospitalizations (adults 65 years and older)") +
  geom_polygon(data=sfr_hosp65, aes(x=long, y=lat, 
                                  group=group, fill=cvd_breaks),
               color=NA, show.legend = T, alpha=0.5) +
  geom_polygon(data=sfr_zcta_map, aes(x=long, y=lat, group=group),
               color="grey70", fill="NA", size=0.5) +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_manual(name="Hospitalizations\nper 100 persons 65+", 
                    values=rev(col_list), na.value="grey50") +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(cvd65_map)
ggsave(cvd65_map, filename = "./Maps/CVD Hospitalization Rates 65+.jpeg",
       device="jpeg",
       dpi=400, width = 7, height = 6, units="in")

#' Respiratory hosptializations per 100 persons
res65_map <- ggmap(base_map) +
  ggtitle("Respiratory hospitalizations (adults 65 years and older)") +
  geom_polygon(data=sfr_hosp65, aes(x=long, y=lat, 
                                  group=group, fill=res_breaks),
               color=NA, show.legend = T, alpha=0.5) +
  geom_polygon(data=sfr_zcta_map, aes(x=long, y=lat, group=group),
               color="grey70", fill="NA", size=0.5) +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_manual(name="Hospitalizations\nper 100 persons 65+", 
                    values=rev(col_list), na.value="grey50") +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(res65_map)
ggsave(res65_map, filename = "./Maps/Res Hospitalization Rates 65+.jpeg", 
       device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")


#' =============================================================================
#' Map the ACS variables
#' =============================================================================

sfr_zip <- sfr_zcta_map[which(sfr_zcta_map$GEOID10 %in% zip_list),]

sfr_acs <- merge(sfr_zip, acs, by="GEOID10")

#' "pct_under5", "pct_over64", "pct_poc", "pct_less_hs", "pct_limited_eng", 
#' "pct_hh_pov", "med_income"

sfr_acs$u5_breaks <- cut(sfr_acs$pct_under5, 
                          breaks=c(0,2,4,5,8,
                                   max(sfr_acs$pct_under5,na.rm=T)),
                          include.lowest=T)
sfr_acs$o64_breaks <- cut(sfr_acs$pct_over64, 
                          breaks=c(0,10,20,30,40,
                                   max(sfr_acs$pct_over64, na.rm=T)),
                          include.lowest=T)
sfr_acs$poc_breaks <- cut(sfr_acs$pct_poc, 
                          breaks=c(0,10,20,30,40,
                                   max(sfr_acs$pct_poc, na.rm=T)),
                          include.lowest=T)
sfr_acs$hs_breaks <- cut(sfr_acs$pct_less_hs, 
                           breaks=c(0,5,10,15,20,
                                    max(sfr_acs$pct_less_hs, na.rm=T)),
                           include.lowest=T)
sfr_acs$eng_breaks <- cut(sfr_acs$pct_limited_eng, 
                          breaks=c(0,1,2,3,4,
                                   max(sfr_acs$pct_limited_eng, na.rm=T)),
                          include.lowest=T)
sfr_acs$pov_breaks <- cut(sfr_acs$pct_hh_pov, 
                           breaks=c(0,5,10,15,20,
                                    max(sfr_acs$pct_hh_pov,na.rm=T)),
                           include.lowest=T)
sfr_acs$in_breaks <- cut(sfr_acs$med_income, 
                           breaks=c(20000,40000,60000,80000,100000,
                                    max(sfr_acs$med_income,na.rm=T)),
                           include.lowest=T)

#' under5
u5_map <- ggmap(base_map) +
  ggtitle("Percentage of the population under 5 years of age") +
  geom_polygon(data=sfr_acs, aes(x=long, y=lat, 
                                  group=group, fill=u5_breaks),
               color=NA, show.legend = T, alpha=0.5) +
  geom_polygon(data=sfr_zcta_map, aes(x=long, y=lat, group=group),
               color="grey50", fill="NA", size=0.5) +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_manual(name="Percentage of\nZIP code population", 
                    values=rev(col_list), na.value="grey50") +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(u5_map)
ggsave(u5_map, filename = "./Maps/Under 5 Population.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")

#' over 64
o64_map <- ggmap(base_map) +
  ggtitle("Percentage of the population 65 years of age and older") +
  geom_polygon(data=sfr_acs, aes(x=long, y=lat, 
                                 group=group, fill=o64_breaks),
               color=NA, show.legend = T, alpha=0.5) +
  geom_polygon(data=sfr_zcta_map, aes(x=long, y=lat, group=group),
               color="grey50", fill="NA", size=0.5) +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_manual(name="Percentage of\nZIP code population", 
                    values=rev(col_list), na.value="grey50") +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(o64_map)
ggsave(o64_map, filename = "./Maps/Over 64 Population.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")

#' persons of color
poc_map <- ggmap(base_map) +
  ggtitle("Percentage of the population that is persons of color") +
  geom_polygon(data=sfr_acs, aes(x=long, y=lat, 
                                 group=group, fill=poc_breaks),
               color=NA, show.legend = T, alpha=0.5) +
  geom_polygon(data=sfr_zcta_map, aes(x=long, y=lat, group=group),
               color="grey50", fill="NA", size=0.5) +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_manual(name="Percentage of\nZIP code population", 
                    values=rev(col_list), na.value="grey50") +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(poc_map)
ggsave(poc_map, filename = "./Maps/POC Population.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")

#' persons of color
poc_map <- ggmap(base_map) +
  ggtitle("Percentage of the population that is persons of color") +
  geom_polygon(data=sfr_acs, aes(x=long, y=lat, 
                                 group=group, fill=poc_breaks),
               color=NA, show.legend = T, alpha=0.5) +
  geom_polygon(data=sfr_zcta_map, aes(x=long, y=lat, group=group),
               color="grey50", fill="NA", size=0.5) +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_manual(name="Percentage of\nZIP code population", 
                    values=rev(col_list), na.value="grey50") +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(poc_map)
ggsave(poc_map, filename = "./Maps/POC Population.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")

#' less than hs
hs_map <- ggmap(base_map) +
  ggtitle("Percentage of the population over 25 years of age\nwith less than a high school education") +
  geom_polygon(data=sfr_acs, aes(x=long, y=lat, 
                                 group=group, fill=hs_breaks),
               color=NA, show.legend = T, alpha=0.5) +
  geom_polygon(data=sfr_zcta_map, aes(x=long, y=lat, group=group),
               color="grey50", fill="NA", size=0.5) +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_manual(name="Percentage of\nZIP code population", 
                    values=rev(col_list), na.value="grey50") +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(hs_map)
ggsave(hs_map, filename = "./Maps/Less HS Population.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")

#' Limited English
eng_map <- ggmap(base_map) +
  ggtitle("Percentage of households with limited ability to speak English") +
  geom_polygon(data=sfr_acs, aes(x=long, y=lat, 
                                 group=group, fill=eng_breaks),
               color=NA, show.legend = T, alpha=0.5) +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_polygon(data=sfr_zcta_map, aes(x=long, y=lat, group=group),
               color="grey50", fill="NA", size=0.5) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_manual(name="Percentage of\nZIP code households", 
                    values=rev(col_list), na.value="grey50") +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(eng_map)
ggsave(eng_map, filename = "./Maps/Limited English Households.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")

#' Households in poverty
pov_map <- ggmap(base_map) +
  ggtitle("Percentage of households with\nlast year income below poverty level") +
  geom_polygon(data=sfr_acs, aes(x=long, y=lat, 
                                 group=group, fill=pov_breaks),
               color=NA, show.legend = T, alpha=0.5) +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_polygon(data=sfr_zcta_map, aes(x=long, y=lat, group=group),
               color="grey50", fill="NA", size=0.5) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_manual(name="Percentage of\nZIP code households", 
                    values=rev(col_list), na.value="grey50") +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(pov_map)
ggsave(pov_map, filename = "./Maps/Poverty Households.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")

#' Median Income
in_map <- ggmap(base_map) +
  ggtitle("Median income (2014$)") +
  geom_polygon(data=sfr_acs, aes(x=long, y=lat, 
                                 group=group, fill=in_breaks),
               color=NA, show.legend = T, alpha=0.5) +
  geom_polygon(data=sfr_counties_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_polygon(data=sfr_zcta_map, aes(x=long, y=lat, group=group),
               color="grey50", fill="NA", size=0.5) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_manual(name="Inflation-adjusted\nincome (2014$)", 
                    values=col_list, na.value="grey50",
                    labels=c("$20,000 - $40,000", "$40,000 - $60,000", 
                             "$60,000 - $80,000", "$80,000 - 100,000",
                             ">$100,000")) +
  xlab("") + ylab("") +
  scale_bar + n_arrow + n_label +
  theme(legend.position = "right") +
  simple_theme2
print(in_map)
ggsave(in_map, filename = "./Maps/Median Income.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")

#' =============================================================================
#' Fake grided data for exposure figures
#' =============================================================================

load("./Data/Spatial Data/sfr_counties_utm_map.RData")
load("./Data/Spatial Data/sfr_zcta_utm_map.RData")
load("./Data/Spatial Data/power_plants_utm.RData")

pp_df <- as.data.frame(pp_utm)
pp_df$id <- as.character(pp_df$id)

plot_crs <- proj4string(sfr_zcta_utm)

sfr_grid <- makegrid(sfr_zcta_utm, cellsize = 4000) #units are in m!
colnames(sfr_grid) <- c("x", "y")
sfr_grid$id <- rownames(sfr_grid)

#' Simulate some spatially-correlated concentrations
#' Remember that map units are m!

d_total <- gstat(formula=total~1, locations=~x+y, dummy=T, beta=70, 
                 model=vgm(psill=50, range=50000, model='Exp'), nmax=20)
sim_total <- predict(d_total, newdata=sfr_grid, nsim=1)

d_back <- gstat(formula=back~1, locations=~x+y, dummy=T, beta=30, 
                model=vgm(psill=50, range=50000, model='Exp'), nmax=20)
sim_back <- predict(d_back, newdata=sfr_grid, nsim=1)

sfr_grid$total <- sim_total$sim1
sfr_grid$back <- sim_back$sim1
sfr_grid$diff <- sfr_grid$total - sfr_grid$back

summary(sfr_grid)

total_map <- ggplot() +
  ggtitle("Baseline case O\u2083 concentration (ppb)") +
  geom_tile(data=sfr_grid, aes(x=x, y=y, fill=total),
            color=NA, show.legend = T) +
  geom_polygon(data=sfr_counties_utm_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_gradient2(name="O\u2083 (ppb)", midpoint=mean(sfr_grid$total),
                       low = "tan", mid = "cadetblue2", high="navy")+
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme2
print(total_map)
ggsave(total_map, filename = "./Maps/Simulated Total O3.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")


back_map <- ggplot() +
  ggtitle("Background O\u2083 concentration (ppb)") +
  geom_tile(data=sfr_grid, aes(x=x, y=y, fill=back),
            color=NA, show.legend = T) +
  geom_polygon(data=sfr_counties_utm_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_gradient2(name="O\u2083 (ppb)", midpoint=mean(sfr_grid$back),
                       low = "tan", mid = "cadetblue2", high="navy")+
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme2
print(back_map)
ggsave(back_map, filename = "./Maps/Simulated Background O3.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")


pp_map <- ggplot() +
  ggtitle("O\u2083 concentration (ppb) due to plant emissions") +
  geom_tile(data=sfr_grid, aes(x=x, y=y, fill=diff),
            color=NA, show.legend = T) +
  geom_polygon(data=sfr_counties_utm_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_gradient2(name="O\u2083 (ppb)", midpoint=mean(sfr_grid$diff),
                       low = "tan", mid = "cadetblue2", high="navy")+
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme2
print(pp_map)
ggsave(pp_map, filename = "./Maps/Simulated Attributable O3.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")

#' Area average of fake ozone data over ZCTA
sfr_pts <- SpatialPointsDataFrame(coords=sfr_grid[,c(1,2)],
                                  proj4string = CRS(plot_crs),
                                  data=sfr_grid)

sfr_zcta_utm@data$ozone <- sp::over(sfr_zcta_utm, sfr_pts["diff"], fn=mean)
sfr_zcta_utm_polygons <- fortify(sfr_zcta_utm, region="id")
sfr_zcta_utm_map <- merge(sfr_zcta_utm_polygons, sfr_zcta_utm@data, by="id")
rm(sfr_zcta_utm_polygons)

#' Median Income
ozone_map <- ggplot() +
  ggtitle("Simulated ZCTA-level ozone concentration (ppb)") +
  geom_polygon(data=sfr_zcta_utm_map, 
               aes(x=long, y=lat, group=group, fill=ozone),
               color=NA, show.legend = T) +
  geom_polygon(data=sfr_counties_utm_map, aes(x=long, y=lat, group=group),
               color="black", fill="NA", size=1) +
  geom_polygon(data=sfr_zcta_utm_map, aes(x=long, y=lat, group=group),
               color="grey50", fill="NA", size=0.5) +
  geom_point(data=pp_df, aes(x=long, y=lat),
             color="black", pch=17, cex=3) +
  scale_fill_gradient2(name="O\u2083 (ppb)", midpoint=mean(sfr_grid$diff),
                       low = "tan", mid = "cadetblue2", high="navy")+
  xlab("") + ylab("") +
  theme(legend.position = "right") +
  simple_theme
print(ozone_map)
ggsave(ozone_map, filename = "./Maps/Simulated ZIP ozone.jpeg", device = "jpeg", 
       dpi=400, width = 7, height = 6, units="in")