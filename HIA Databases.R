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
#' Creating the baseline rates database
#' -----------------------------------------------------------------------------

#' mortality
mort <- read.csv("./Data/VS Data/co_mortality_zip_30plus_rate_period.csv", 
                 header=T, stringsAsFactors = F)
mort$mort_ac <- mort$all_cause_per_1000_30plus5y / 1000 / 365
mort$mort_na <- mort$all_cause_per_1000_30plus5y / 1000 / 365

#' hosptializations for the 65+ population (from Ryan)
hosp <- read.csv("./Data/CHA Data/co_zip_65plus_rate_period.csv", 
                 header=T, stringsAsFactors = F)

#' calculate rates per person per day
hosp$hosp_cp <- hosp$cardiopulm_per_100_65p5y / 100 / 365
hosp$hosp_cvd <- hosp$cvd_per_100_65p5y / 100 / 365
hosp$hosp_res <- hosp$resp_per_100_65p5y / 100 / 365

#' other morbitidies (See Ozone RIA 2015 and BenMAP user manual)
#' minor_ast is mean of cough, sob, and wheeze
morb <- data.frame(outcome = c("ed_ast", "hosp_mi", "minor_astc", "minor_asts",
                               "minor_astw", "minor_ast", "minor_mrad", "minor_sld", 
                               "minor_wld", "minor_lrs", "ac_bronchitis"),
                   pppd = c(.959/100/365, NA, 0.067, 0.037, 
                            0.076, 0.067, 0.02137, 9.9/180, 
                            0.0057, 0.0012, 0.043/365))
outcomes <- unique(as.character(morb$outcome))
morb2 <- as.data.frame(t(morb))
colnames(morb2) <- outcomes
morb2 <- morb2[-c(1),]
morb2 <- morb2[rep(row.names(morb2), nrow(hosp)),]

#' put them all together
pppd <- hosp[,c("ZIP", "hosp_cp", "hosp_cvd", "hosp_res")]
pppd <- merge(pppd, mort[,c("ZIP", "mort_ac", "mort_na")], 
              by="ZIP", all=T)
pppd <- cbind(pppd, morb2)

rownames(pppd) <- seq(1:nrow(pppd))

pppd <- rename(pppd, c("ZIP" = "GEOID"))

write.table(pppd, "./HIA Inputs/rates.txt", row.names = F)

#' -----------------------------------------------------------------------------
#' Creating the outcome database-- CR (SE), age-group, and monetized value
#' -----------------------------------------------------------------------------

load("./Data/Pooled CRs.RData")
cr <- merge(pooled_crs, values, by="outcome")

cr <- cr[,c("outcome", "pol", "age_group", "cr_beta", "cr_se", "value_2024")]

write.table(cr, "./HIA Inputs/CR.txt", row.names = F)
