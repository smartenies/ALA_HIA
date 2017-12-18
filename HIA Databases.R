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
#' This script generates the databases used in the HIA
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

#' get the list of ZCTAs in Colorado
co_zcta <- load("./Data/Spatial Data/co_zcta_utm_map.RData")
zcta <- unique(as.character(co_zcta_utm$GEOID_Data))

rm(co_zcta_utm, co_zcta_utm_map, co_zcta)

# load the outcome age groups and values
values <- read.table("./Data/Ages and Values.txt", header=T,
                     stringsAsFactors = F)
values$age_group <- gsub("-", "_", values$age_group)

age_groups <- unique(values$age_group)
age_groups <- paste("p", age_groups, sep="")
age_groups <- c("GEOID", "total", "p0_17", age_groups)

#' -----------------------------------------------------------------------------
#' Creating the population database
#' -----------------------------------------------------------------------------

pop <- read.table("./Data/ACS_2010_2014/X01_AGE_AND_SEX.txt", header=T, sep=",")
pop$GEOID <- as.character(pop$GEOID)
pop <- pop[which(pop$GEOID %in% zcta),]

#' Summarize population age groups
pop$total <- pop$B01001e1

#' Sum male and female counts in each age category
pop$p0_4 <- pop$B01001e3 + pop$B01001e27
pop$p5_9 <- pop$B01001e4 + pop$B01001e28
pop$p10_14 <- pop$B01001e5 + pop$B01001e29
pop$p15_17 <- pop$B01001e6 + pop$B01001e30
pop$p18_19 <- pop$B01001e7 + pop$B01001e31
pop$p20 <- pop$B01001e8 + pop$B01001e32
pop$p21 <- pop$B01001e9 + pop$B01001e33
pop$p22_24 <- pop$B01001e10 + pop$B01001e34
pop$p25_29 <- pop$B01001e11 + pop$B01001e35
pop$p30_34 <- pop$B01001e12 + pop$B01001e36
pop$p35_39 <- pop$B01001e13 + pop$B01001e37
pop$p40_44 <- pop$B01001e14 + pop$B01001e38
pop$p45_49 <- pop$B01001e15 + pop$B01001e39
pop$p50_54 <- pop$B01001e16 + pop$B01001e40
pop$p55_59 <- pop$B01001e17 + pop$B01001e41
pop$p60_61 <- pop$B01001e18 + pop$B01001e42
pop$p62_64 <- pop$B01001e19 + pop$B01001e43
pop$p65_66 <- pop$B01001e20 + pop$B01001e44
pop$p67_69 <- pop$B01001e21 + pop$B01001e45
pop$p70_74 <- pop$B01001e22 + pop$B01001e46
pop$p75_79 <- pop$B01001e23 + pop$B01001e47
pop$p80_84 <- pop$B01001e24 + pop$B01001e48
pop$p85_99 <- pop$B01001e25 + pop$B01001e49

#' Get smaller age groups by assuming equal distribution across all ages
pop$p0_1 <- pop$p0_4 / 5
pop$p0_17 <- pop$p0_4 + pop$p5_9 + pop$p10_14 + pop$p15_17
pop$p5_17 <- pop$p5_9 + pop$p10_14 + pop$p15_17
pop$p8_12 <- (pop$p5_9 * (2/5)) + (pop$p10_14 * (3/5))
pop$p6_14 <- (pop$p5_9 * (4/5)) + pop$p10_14
pop$p6_18 <- (pop$p5_9 * (4/5)) + pop$p10_14 + pop$p15_17 + (pop$p18_19 * (1/2))
pop$p18_64 <- (pop$p18_19 + pop$p20 + pop$p21 + pop$p22_24 + pop$p25_29 
                + pop$p30_34 + pop$p35_39 + pop$p40_44 + pop$p45_49 + pop$p50_54 + pop$p55_59
                + pop$p60_61 + pop$p62_64)
pop$p0_64 <- (pop$p0_17 + pop$p18_19 + pop$p20 + pop$p21 + pop$p22_24 + pop$p25_29 +
              pop$p30_34 + pop$p35_39 + pop$p40_44 + pop$p45_49 + pop$p50_54 +
              pop$p55_59 + pop$p60_61 + pop$p62_64)
pop$p60_64 <- pop$p60_61 + pop$p62_64
pop$p65_69 <- pop$p65_66 + pop$p67_69
pop$p65_99 <- pop$p65_69 + pop$p70_74 + pop$p75_79 + pop$p80_84 + pop$p85_99 
pop$p18_99 <- pop$p18_64 + pop$p65_99
pop$p30_99 <- pop$p30_34 + pop$p35_39 + pop$p40_44 + pop$p45_49 + pop$p50_54 + 
              pop$p55_59 + pop$p60_64 + pop$p65_99 
pop$p0_99 <- pop$total

co_pop <- pop[,age_groups] 
co_pop$GEOID <- gsub("86000US", "", co_pop$GEOID)

write.table(co_pop, "./HIA Inputs/population.txt", row.names=F)

#' -----------------------------------------------------------------------------
#' Creating the baseline rates database
#' -----------------------------------------------------------------------------

#' mortality


#' hosptializations (from Ryan)
hosp <- read.csv("./Data/CHA Data/co_zip_rate_period.csv", header=T)

#' calculate rates per person per day
hosp$hosp_cp <- hosp$cardiopulm_per_100p5y / 100 / 365
hosp$hosp_cvd <- hosp$cvd_per_100p5y / 100 / 365
hosp$hosp_res <- hosp$resp_per_100p5y / 100 / 365

#' other morbitidies (See Ozone RIA 2015 and BenMAP user manual)
morb <- data.frame(outcome = c("ed_ast", "hosp_mi", "minor_astc", "minor_asts",
                               "minor_astw", "minor_mrad", "minor_sld", 
                               "minor_wld", "minor_lrs", "ac_bronchitis"),
                   pppd = c(.959/100/365, NA, 0.145, 0.074, 0.173, 0.02137, 9.9/180, 0.0057,
                            0.0012, 0.043/365))
outcomes <- unique(as.character(morb$outcome))
morb2 <- as.data.frame(t(morb))
colnames(morb2) <- outcomes
morb2 <- morb2[-c(1),]
morb2 <- morb2[rep(row.names(morb2), nrow(hosp)),]

#' put them all together
pppd <- hosp[,c("ZIP", "hosp_cp", "hosp_cvd", "hosp_res")]
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
