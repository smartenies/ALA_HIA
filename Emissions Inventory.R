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
#' This script summarized the NEI data in order to identify emissions at 
#' the two facilities. There was a request to update the estimates based on
#' impacts per ton of pollutant emitted. These NEI data are going to be used
#' to meet that request.
#' -----------------------------------------------------------------------------

library(ggplot2)
library(reshape2)
library(dplyr)
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
  #legend.position = c(0.75,0.3),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

#' -----------------------------------------------------------------------------
#' NEI files from 2008, 2011, and 2014 (2017 not yet available)
#' Subset to get CO facilities and just the ALA facilities
#' -----------------------------------------------------------------------------

NEI14 <- read.csv("./Data/NEI Data/2014neiv1_facility/2014v1facilities.csv", 
                  header=T, stringsAsFactors = F)
NEI14$year <- 2014
CO_NEI14 <- NEI14[which(NEI14$st_usps_cd == "CO"),]
rm(NEI14)

NEI11 <- read.csv("./Data/NEI Data/2011neiv2_facility/2011neiv2_facility.csv", 
                  header=T, stringsAsFactors = F)
NEI11$year <- 2011
CO_NEI11 <- NEI11[which(NEI11$st_usps_cd == "CO"),]
rm(NEI11)

NEI08 <- read.csv("./Data/NEI Data/2008neiv3_facility/2008neiv3_facility.csv", 
                  header=T, stringsAsFactors = F)
NEI08$year <- 2008
CO_NEI08 <- NEI08[which(NEI08$st_usps_cd == "CO"),]
rm(NEI08)

#' Get column types to match
CO_NEI14$latitude_msr <- as.numeric(CO_NEI14$latitude_msr)
CO_NEI14$longitude_msr <- as.numeric(CO_NEI14$longitude_msr)

CO_NEI <- bind_rows(CO_NEI14, CO_NEI11, CO_NEI08)
rm(CO_NEI14, CO_NEI11, CO_NEI08)

save(CO_NEI, file="./Data/NEI Data/CO_NEI.RData")

com <- 4367811
comanche <- CO_NEI[which(CO_NEI$eis_facility_site_id == com),]

md <- 4391711
drake <- CO_NEI[which(CO_NEI$eis_facility_site_id == md),]

ALA_NEI <- rbind(comanche, drake)

save(comanche, file="./Data/NEI Data/Comanche_2014_NEI.RData")
save(drake, file="./Data/NEI Data/Drake_2014_NEI.RData")
save(ALA_NEI, file="./Data/NEI Data/ALA_2014_NEI.RData")

write.csv(ALA_NEI, file="./Data/NEI Data/ALA_2014_NEI.csv",
          row.names = F)

#' -----------------------------------------------------------------------------
#' Plot changes in pollutant emissions over time for each facility
#' -----------------------------------------------------------------------------

load("./Data/NEI Data/ALA_2014_NEI.RData")

#' Particulate matter, ozone precurors, and Hg
#pols <- c("SO2", "NOX", "PM25-PRI", "VOC", "EC", "OC", "SO4", "NO3", "Mercury")
pols <- c("SO2", "NOX", "PM25-PRI", "VOC")
facilities <- unique(ALA_NEI$eis_facility_site_id)
  
ala <- ALA_NEI[which(ALA_NEI$pollutant_cd %in% pols),]
ala$facility_site_name <- gsub("UTILITIES-", "UTILITIES", ala$facility_site_name)

#' plotting changes over time
pol_plot <- ggplot(data=ala) +
  geom_point(aes(x=year, y=total_emissions, 
                 group=pollutant_cd, 
                 color=pollutant_cd)) +
  geom_line(aes(x=year, y=total_emissions, 
                group=pollutant_cd, 
                color=pollutant_cd)) +
  facet_grid(pollutant_cd ~ facility_site_name, scales="free") +
  xlab("Year") + ylab("Emissions (TPY)") +
  simple_theme
print(pol_plot)

ggsave(plot=pol_plot, file="./Data/NEI Data/Pollutants By Facility.jpeg",
       device="jpeg", units="in", height=8, width=8, dpi=300)

#' -----------------------------------------------------------------------------
#' Check for linear trends over the last three NEI years
#' -----------------------------------------------------------------------------

lm_results <- data.frame()
for (i in 1:length(facilities)) {
  df <- ala[which(ala$eis_facility_site_id == facilities[i]),]
  temp <- data.frame()
  for (j in 1:length(pols)) {
    df2 <- df[which(df$pollutant_cd == pols[j]),]
    m <- summary(lm(total_emissions ~ year, data=df2))
    temp[1+(j-1), 1] <- facilities[i]
    temp[1+(j-1), 2] <- pols[j]
    temp[1+(j-1), 3] <- m$coefficients[2,1]
    temp[1+(j-1), 4] <- m$coefficients[2,4]
  }
  lm_results <- rbind(lm_results, temp)
  rm(temp)
}
colnames(lm_results) <- c("Facility", "Pollutant", "Coeff", "P value")
write.csv(lm_results, file="./Data/NEI Data/LM Test for Trend.csv", 
          row.names = F)
  

  
  
  
  
