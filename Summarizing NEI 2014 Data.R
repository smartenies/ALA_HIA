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
#' 
#' This script summarized the NEI 2014 data in order to identify emissions at 
#' the two facilities. There was a request to update the estimates based on
#' impacts per ton of pollutant emitted. These NEI data are going to be used
#' to meet that request.
#' -----------------------------------------------------------------------------

library(ggplot2)
library(reshape2)
library(plyr)
library(XLConnect)

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
  #legend.position = c(0.75,0.3),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

#' -----------------------------------------------------------------------------

NEI <- read.csv("./Data/NEI 2014 Facility Data/2014v1facilities.csv", header=T)
CO_NEI <- NEI[which(NEI$st_usps_cd == "CO"),]

save(CO_NEI, file="./Data/NEI 2014 Facility Data/CO_2014_NEI.RData")
rm(NEI)

comanche <- CO_NEI[which(CO_NEI$facility_site_name == 
                           "PUBLIC SERVICE CO COMANCHE PLT"),]
drake <- CO_NEI[which(CO_NEI$facility_site_name == 
                        "COLORADO SPRINGS UTILITIES- MARTIN DRAKE"),]

front_range <- rbind(comanche, drake)

save(comanche, file="./Data/NEI 2014 Facility Data/Comanche_2014_NEI.RData")
save(drake, file="./Data/NEI 2014 Facility Data/Drake_2014_NEI.RData")
save(front_range, file="./Data/NEI 2014 Facility Data/ALA_2014_NEI.RData")

write.csv(front_range, file="./Data/NEI 2014 Facility Data/ALA_2014_NEI.csv",
          row.names = F)
