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
#' This script generates the pppd, CR and valuation databases used in the HIA
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

#' -----------------------------------------------------------------------------
#' get the list of ZCTAs in Colorado
#' -----------------------------------------------------------------------------
co_zcta <- load("./Data/Spatial Data/co_zcta_utm_map.RData")
zcta <- unique(as.character(co_zcta_utm$GEOID_Data))

rm(co_zcta_utm, co_zcta_utm_map, co_zcta)

#' -----------------------------------------------------------------------------
#' Creating the baseline rates database
#' -----------------------------------------------------------------------------

#' mortality
mort <- read.csv("./Data/VS Data/co_mortality_zip_30plus_rate_period.csv", 
                 header=T, stringsAsFactors = F)
mort$mort_ac <- mort$ac_per_1000_30plus5y / 1000 / 365
mort$mort_ac_se <- mort$ac_per_1000_30plus5y_se / 1000 / 365
mort$mort_na <- mort$na_per_1000_30plus5y / 1000 / 365
mort$mort_na_se <- mort$na_per_1000_30plus5y_se / 1000 / 365

#' just pppd columns
mort <- mort[,c(1, 10:13)]

#' hosptializations for the 65+ population (from Ryan)
hosp <- read.csv("./Data/CHA Data/co_zip_65plus_rate_period.csv", 
                 header=T, stringsAsFactors = F)

#' calculate rates per person per day
hosp$hosp_cp <- hosp$cp_per_100_65plus5y / 100 / 365
hosp$hosp_cp_se <- hosp$cp_per_100_65plus5y_se / 100 / 365
hosp$hosp_cvd <- hosp$cvd_per_100_65plus5y / 100 / 365
hosp$hosp_cvd_se <- hosp$cvd_per_100_65plus5y_se / 100 / 365
hosp$hosp_res <- hosp$res_per_100_65plus5y / 100 / 365
hosp$hosp_res_se <- hosp$res_per_100_65plus5y_se / 100 / 365

#' just pppd columns
hosp <- hosp[,c(1, 13:18)]

out_df <- merge(mort, hosp, by="ZIP", all=T)

#' other morbitidies (See Ozone RIA 2015 and BenMAP user manual)
#' minor_ast is mean of cough, sob, and wheeze
morb <- read.csv("./Data/Other Health Data/Other Morbidity Rates.csv",
                 header=T, stringsAsFactors = F)
outcomes <- c(unique(as.character(morb$outcome)),
              paste(unique(as.character(morb$outcome)), "_se", sep=""))

morb_pppd <- morb[,c("ZIP", "outcome", "pppd")]
morb_pppd <- reshape(morb_pppd, idvar = "ZIP",
                     timevar = c("outcome"),
                     direction = "wide")
colnames(morb_pppd) <- gsub("pppd.", "", colnames(morb_pppd))
morb_pppd$ZIP <- NULL

morb_se <- morb[,c("ZIP", "outcome", "se")]
morb_se <- reshape(morb_se, idvar = "ZIP",
                     timevar = c("outcome"),
                     direction = "wide")
colnames(morb_se) <- gsub("se.", "", colnames(morb_se))
colnames(morb_se)[-1] <- paste(colnames(morb_se)[-1], "_se", sep="")
morb_se$ZIP <- NULL

morb2 <- cbind(morb_pppd, morb_se)
morb2 <- morb2[,order(morb2)]

morb2 <- morb2[rep(row.names(morb2), nrow(out_df)),]

#' put them all together
pppd <- cbind(out_df, morb2)

rownames(pppd) <- seq(1:nrow(pppd))

pppd <- rename(pppd, c("ZIP" = "GEOID"))

write.table(pppd, "./HIA Inputs/rates.txt", row.names = F)

#' -----------------------------------------------------------------------------
#' Creating the outcome database-- CR (SE), age-group, and monetized value
#' -----------------------------------------------------------------------------

load("./Data/Pooled CRs.RData")
pooled_crs <- pooled_crs[order(pooled_crs$pol, pooled_crs$metric),]

values <- read.table("./Data/Ages and Values.txt", header=T,
                     stringsAsFactors = F)
values$age_group <- gsub("-", "_", values$age_group)
values$age_group <- paste("p", values$age_group, sep="")

cr <- merge(pooled_crs, values, by="outcome", all.x=T)

cr <- cr[,c("outcome", "pol", "metric", "age_group", "cr_beta", "cr_se", "value_2024")]

write.table(cr, "./HIA Inputs/CR.txt", row.names = F)
