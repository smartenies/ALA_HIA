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
#' This script generates the age-group specific populations and standard errors
#' used to calculate rates and attributable health impacts
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

# # load the outcome age groups and values
# values <- read.table("./Data/Ages and Values.txt", header=T,
#                      stringsAsFactors = F)
# values$age_group <- gsub("-", "_", values$age_group)
# 
# ages <- paste("p", unique(values$age_group), sep="")
# ages_se <- paste(age_groups, "_se", sep="")
# age_groups <- c("GEOID", "total", "total_se", age_groups, age_groups_se)
# age_groups <- age_groups[order(age_groups)]

#' -----------------------------------------------------------------------------
#' Creating the population database
#' 
#' See ACS Instructions for Applying Statistical Testing:
#' SE for each population age group is derived from the MOE
#'      SE = MOE / Z where Z is 1.645 (ACS uses a 90% confidence level)
#' When combining estimates, MOE is SQRT(MOE1^2 + MOE2^2 + ...)
#' -----------------------------------------------------------------------------

pop <- read.table("./Data/ACS_2010_2014/X01_AGE_AND_SEX.txt", header=T, sep=",")
pop$GEOID <- as.character(pop$GEOID)
pop <- pop[which(pop$GEOID %in% zcta),]

pop_cols <- colnames(pop)[-c(1:2)]

Z <- 1.645

#' Summarize population age groups
pop$total <- pop$B01001e1
pop$total_se <- pop$B01001m1 / Z
  
  #' Sum male and female counts in each age category
pop$p0_4 <- pop$B01001e3 + pop$B01001e27
pop$p0_4_se <- sqrt(pop$B01001m3**2 + pop$B01001m27**2) / Z

pop$p5_9 <- pop$B01001e4 + pop$B01001e28
pop$p5_9_se <- sqrt(pop$B01001m4**2 + pop$B01001m28**2) / Z

pop$p10_14 <- pop$B01001e5 + pop$B01001e29
pop$p10_14_se <- sqrt(pop$B01001m5**2 + pop$B01001m29**2) / Z

pop$p15_17 <- pop$B01001e6 + pop$B01001e30
pop$p15_17_se <- sqrt(pop$B01001m6**2 + pop$B01001m30**2) / Z

pop$p18_19 <- pop$B01001e7 + pop$B01001e31
pop$p18_19_se <- sqrt(pop$B01001m7**2 + pop$B01001m31**2) / Z

pop$p20 <- pop$B01001e8 + pop$B01001e32
pop$p20_se <- sqrt(pop$B01001m8**2 + pop$B01001m32**2) / Z

pop$p21 <- pop$B01001e9 + pop$B01001e33
pop$p21_se <- sqrt(pop$B01001m9**2 + pop$B01001m33**2) / Z

pop$p22_24 <- pop$B01001e10 + pop$B01001e34
pop$p22_24_se <- sqrt(pop$B01001m10**2 + pop$B01001m34**2) / Z

pop$p25_29 <- pop$B01001e11 + pop$B01001e35
pop$p25_29_se <- sqrt(pop$B01001m11**2 + pop$B01001m35**2) / Z

pop$p30_34 <- pop$B01001e12 + pop$B01001e36
pop$p30_34_se <- sqrt(pop$B01001m12**2 + pop$B01001m36**2) / Z

pop$p35_39 <- pop$B01001e13 + pop$B01001e37
pop$p35_39_se <- sqrt(pop$B01001m13**2 + pop$B01001m37**2) / Z

pop$p40_44 <- pop$B01001e14 + pop$B01001e38
pop$p40_44_se <- sqrt(pop$B01001m14**2 + pop$B01001m38**2) / Z

pop$p45_49 <- pop$B01001e15 + pop$B01001e39
pop$p45_49_se <- sqrt(pop$B01001m15**2 + pop$B01001m39**2) / Z

pop$p50_54 <- pop$B01001e16 + pop$B01001e40
pop$p50_54_se <- sqrt(pop$B01001m16**2 + pop$B01001m40**2) / Z

pop$p55_59 <- pop$B01001e17 + pop$B01001e41
pop$p55_59_se <- sqrt(pop$B01001m17**2 + pop$B01001m41**2) / Z

pop$p60_61 <- pop$B01001e18 + pop$B01001e42
pop$p60_61_se <- sqrt(pop$B01001m18**2 + pop$B01001m42**2) / Z

pop$p62_64 <- pop$B01001e19 + pop$B01001e43
pop$p62_64_se <- sqrt(pop$B01001m19**2 + pop$B01001m43**2) / Z

pop$p65_66 <- pop$B01001e20 + pop$B01001e44
pop$p65_66_se <- sqrt(pop$B01001m20**2 + pop$B01001m44**2) / Z

pop$p67_69 <- pop$B01001e21 + pop$B01001e45
pop$p67_69_se <- sqrt(pop$B01001m21**2 + pop$B01001m45**2) / Z

pop$p70_74 <- pop$B01001e22 + pop$B01001e46
pop$p70_74_se <- sqrt(pop$B01001m22**2 + pop$B01001m46**2) / Z

pop$p75_79 <- pop$B01001e23 + pop$B01001e47
pop$p75_79_se <- sqrt(pop$B01001m23**2 + pop$B01001m47**2) / Z

pop$p80_84 <- pop$B01001e24 + pop$B01001e48
pop$p80_84_se <- sqrt(pop$B01001m24**2 + pop$B01001m48**2) / Z

pop$p85_99 <- pop$B01001e25 + pop$B01001e49
pop$p85_99_se <- sqrt(pop$B01001m25**2 + pop$B01001m49**2) / Z

#' Get combined age groups by assuming equal distribution across all ages
pop$p0_1 <- pop$p0_4 / 5
pop$p0_1_se <- (pop$p0_4_se) / 5

pop$p0_17 <- pop$p0_4 + pop$p5_9 + pop$p10_14 + pop$p15_17
pop$p0_17_se <- sqrt(pop$p0_4_se**2 + pop$p5_9_se**2 + 
                       pop$p10_14_se**2 + pop$p15_17_se**2)

pop$p5_17 <- pop$p5_9 + pop$p10_14 + pop$p15_17
pop$p5_17_se <- sqrt(pop$p5_9_se**2 + pop$p10_14_se**2 + pop$p15_17_se**2)

pop$p8_12 <- (pop$p5_9 * (2/5)) + (pop$p10_14 * (3/5))
pop$p8_12_se <- sqrt((pop$p5_9_se * (2/5))**2 + (pop$p10_14_se * (3/5))**2)

pop$p6_14 <- (pop$p5_9 * (4/5)) + pop$p10_14
pop$p6_14_se <- sqrt((pop$p5_9_se * (4/5))**2 + pop$p10_14_se**2)

pop$p6_18 <- (pop$p5_9 * (4/5)) + pop$p10_14 + pop$p15_17 + (pop$p18_19 * (1/2))
pop$p6_18_se <- sqrt((pop$p5_9_se * (4/5))**2 + pop$p10_14_se**2 
                      + pop$p15_17_se**2 + (pop$p18_19_se * (1/2))**2)

pop$p18_64 <- (pop$p18_19 + pop$p20 + pop$p21 + pop$p22_24 + pop$p25_29 
               + pop$p30_34 + pop$p35_39 + pop$p40_44 + pop$p45_49 + pop$p50_54 
               + pop$p55_59 + pop$p60_61 + pop$p62_64)
pop$p18_64_se <- sqrt(pop$p18_19_se**2 + pop$p20_se**2 + pop$p21_se**2 + pop$p22_24_se**2
                      + pop$p25_29_se**2 + pop$p30_34_se**2 + pop$p35_39_se**2
                      + pop$p40_44_se**2 + pop$p45_49_se**2 + pop$p50_54_se**2 
                      + pop$p55_59_se**2 + pop$p60_61_se**2 + pop$p62_64_se**2)

pop$p0_64 <- (pop$p0_17 + pop$p18_19 + pop$p20 + pop$p21 + pop$p22_24
              + pop$p25_29 + pop$p30_34 + pop$p35_39 + pop$p40_44 
              + pop$p45_49 + pop$p50_54 + pop$p55_59 + pop$p60_61 + pop$p62_64)
pop$p0_64_se <- sqrt(pop$p0_17_se**2 + pop$p18_19_se**2 + pop$p20_se**2 + pop$p21_se**2
                     + pop$p22_24_se**2 + pop$p25_29_se**2 + pop$p30_34_se**2 
                     + pop$p35_39_se**2 + pop$p40_44_se**2 + pop$p45_49_se**2 
                     + pop$p50_54_se**2 + pop$p55_59_se**2 + pop$p60_61_se**2
                     + pop$p62_64_se**2)

pop$p60_64 <- pop$p60_61 + pop$p62_64
pop$p60_64_se <- sqrt(pop$p60_61_se**2 + pop$p62_64_se**2)

pop$p65_69 <- pop$p65_66 + pop$p67_69
pop$p65_69_se <- sqrt(pop$p65_66_se**2 + pop$p67_69_se**2)

pop$p65_99 <- pop$p65_69 + pop$p70_74 + pop$p75_79 + pop$p80_84 + pop$p85_99
pop$p65_99_se <- sqrt(pop$p65_69_se**2 + pop$p70_74_se**2 + pop$p75_79_se**2
                      + pop$p80_84_se**2 + pop$p85_99_se**2)

pop$p18_99 <- pop$p18_64 + pop$p65_99
pop$p18_99_se <- sqrt(pop$p18_64_se**2 + pop$p65_99_se**2)

pop$p30_99 <- (pop$p30_34 + pop$p35_39 + pop$p40_44 + pop$p45_49 + pop$p50_54 
               + pop$p55_59 + pop$p60_64 + pop$p65_99) 
pop$p30_99_se <- sqrt(pop$p30_34_se**2 + pop$p35_39_se**2 + pop$p40_44_se**2
                      + pop$p45_49_se**2 + pop$p50_54_se**2 + pop$p55_59_se**2
                      + pop$p60_64_se**2 + pop$p65_99_se**2) 

pop$p0_99 <- pop$total
pop$p0_99_se <- pop$total_se

co_pop <- pop[,which(!(colnames(pop) %in% pop_cols))] 
co_pop$GEOID <- gsub("86000US", "", co_pop$GEOID)

write.table(co_pop, "./HIA Inputs/population.txt", row.names=F)
write.csv(co_pop, "./Data/ACS_2010_2014/co_populations.csv", row.names=F)
