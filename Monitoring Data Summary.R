#' =============================================================================
#' Project: American Lung Association HIA
#' Date created: February 22, 2018
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
#' This script summarizes O3 and PM2.5 monitoring data in the SFR
#' =============================================================================

library(foreign)
library(sp)
library(gstat)
library(rgdal)
library(raster)
library(ggplot2)
library(ggmap)
library(rgeos)
library(maptools)
library(ggthemes)
library(ggrepel)
library(RColorBrewer)
library(gridExtra)
library(plyr)
library(stringr)

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
options(scipen = 9999) #avoid scientific notation

geo_data <- "T:/Rsch-MRS/ECHO/SEM Large Data/Spatial Data"
utm_13 <- "+init=epsg:26913"
lat_long <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
#' =============================================================================

#' -----------------------------------------------------------------------------
# Identifying air monitors
#' -----------------------------------------------------------------------------

pm_full <- read.csv("./Data/AQS Data/AQS Data 2009-2017.csv", header=T,
                     stringsAsFactors = F)


#' Monitor IDs 08041* are in El Paso county and 08101* are in Pueblo
#' Monitor IDs 08031* are in Denver 
sfr_ids <- c("080410013", "080410015", "080410016", "080410017", "081010015",
             )

aqs_sfr <- aqs_full[which(aqs_full$monitor_id %in% sfr_ids),] 

#' -----------------------------------------------------------------------------

load("./Data/Air Quality/monitors.RData")
load("./Data/Spatial Data/dm_tracts_utm_map.RData")

monitors_coord <- monitors[,c("Longitude", "Latitude")]
monitors_spdf <- SpatialPointsDataFrame(coords = monitors_coord,
                                        data = monitors,
                                        proj4string = CRS("+init=epsg:4326"))
monitors_utm <- spTransform(monitors_spdf, CRS(utm_13)) #UTM 13N
monitors_utm@data$id <- rownames(monitors_utm@data)
monitors_utm@data$long <- coordinates(monitors_utm)[,1]
monitors_utm@data$lat <- coordinates(monitors_utm)[,2]
monitors_utm_map <- as.data.frame(monitors_utm)

pm2.5_mon_utm <- monitors_utm[which(monitors_utm$Parameter.Code=="88101"),]
pm2.5_mon_utm_map <- monitors_utm_map[which(monitors_utm_map$Parameter.Code=="88101"),]

o3_mon_utm <- monitors_utm[which(monitors_utm$Parameter.Code=="44201"),]
o3_mon_utm_map <- monitors_utm_map[which(monitors_utm_map$Parameter.Code=="44201"),]

save(monitors_utm, monitors_utm_map,
     pm2.5_mon_utm, pm2.5_mon_utm_map,
     o3_mon_utm, o3_mon_utm_map,
     file="./Data/Spatial Data/monitors_utm_map.RData")

#' -----------------------------------------------------------------------------
#' Mapping the air monitors
#' -----------------------------------------------------------------------------

load("./Data/Spatial Data/monitors_utm_map.RData")

o3_mon <- as.data.frame(spTransform(o3_mon_utm, CRS(lat_long)))
pm2.5_mon <- as.data.frame(spTransform(pm2.5_mon_utm, CRS(lat_long)))

mon_df <- rbind(o3_mon, pm2.5_mon)

base_map <- get_map(location = "Aurora, CO", zoom=9)

scale_bar <- ggsn::scalebar(location = "bottomright", dd2km = T, model="WGS84",
                            dist=10, st.bottom=F, st.size = 3, height=0.0125, 
                            x.min = -105.5, x.max = -104.0, 
                            y.min = 39.08, y.max = 40.4) 

n_arrow <- geom_segment(arrow=arrow(length=unit(4, "mm")),
                        aes(x=-104.05, xend=-104.05, y=39.15, yend=39.25),
                        color="black", size=1)

n_label <- geom_text(aes(x=-104.05, y=39.28), label="N")

ggmap(base_map) +
  scale_bar + n_arrow + n_label +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

monitor_map <- ggmap(base_map) +
  ggtitle("PM\u2082.\u2085 and O\u2083 monitors in the Denver Area") +
  geom_point(data=mon_df, aes(x=Longitude, y=Latitude, color=as.factor(Parameter.Code)),
             pch=19, cex=3, position = position_jitter(w = 0.3, h = 0)) +
  scale_color_manual(name="Pollutant",
                     values=c("red", "blue"),
                     labels=c("O\u2083", "PM\u2082.\u2085")) +
  scale_bar + n_arrow + n_label +
  simple_theme2
print(monitor_map)
ggsave(monitor_map, filename = "./Maps/Area Monitors.jpeg", device = "jpeg", 
       dpi=600, width = 7, height = 7, units="in")

rm(monitor_map)

#' -----------------------------------------------------------------------------
#' Time series plots of pm2.5 and ozone at each monitor
#' -----------------------------------------------------------------------------

# load("./Data/Air Quality/AQS Data 2009-2017.RData")
# 
# p <- c("88101", "44201")
# p_name <- c("PM\u2082.\u2085 (\u03BCg/m\u00B3)", "O\u2083 (ppm)")
# p_lab <- c("PM2.5", "Ozone")
# 
# for (i in 1:length(p)) {
#   df <- output[which(output$Parameter.Code==as.integer(p[i]) & output$POC==1),]
#   ts_plot <- ggplot(data=df, aes(x=datetime, y=Sample.Measurement)) +
#     geom_line(aes(group=monitor_id, color=monitor_id)) + 
#     geom_smooth(color="black", size=1) +
#     scale_x_datetime(date_breaks = "6 months", date_labels =  "%b %d %H:%M") +
#     xlab("Date") + ylab(p_name[i]) +
#     theme(axis.text.x=element_text(angle=60, hjust=1)) +
#     simple_theme
#   print(ts_plot)
#   
#   ggsave(ts_plot, filename = paste("./Maps/Criteria Pollutants/TS_",
#                                    p_lab[i], "_2009-2017.jpeg", sep=""), 
#          device = "jpeg", dpi=400, width = 7, height = 6, units="in")
# }
