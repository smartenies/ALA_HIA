# ------------------------------------------------------------------------------
# Title: Estimating Colorado 2010-2014 period ED rates in ZIP Codes
# Author: Ryan Gan
# Date Created: 2017-12-6
# 
# Modified 12-9-17 by SEM to subset data by age (>= 65 years)
# ------------------------------------------------------------------------------

# library ----
library(tidyverse)


# read file and create some variables -----
co_hosp <- read_csv("./Data/CHA Data/co_hosp_w_outcome_df.csv") %>% 
  # noticed broomfield county_geo 014 was sometimes assigned county_final 159
  # there is no county 159. All other spatial indicators suggest broomfield 014
  filter(AGEYRS >= 65) %>%
  mutate(county_final = ifelse(county_geo == "014" & county_final == "159", 
                               "014", county_final),
         # make fips code
         FIPS = paste0("08", county_final),
         # adding in a season variable for estimation by season 
         season = as.factor(ifelse(ADMMM >= 3 & ADMMM <= 5, "spring",
                                   ifelse(ADMMM >= 6 & ADMMM <= 8, "summer",
                                          ifelse(ADMMM >= 9 & ADMMM <= 11, 
                                                 "fall", "winter")))),
         # adding in a year indicator for season to check for trends
         season_yr = as.factor(paste0(season, ADMYY)),
         # set wrfgrid values of 0 to missing
         WRFGRID_ID = ifelse(WRFGRID_ID == 0, NA, WRFGRID_ID),
         WRFGRID_ID = as.character(WRFGRID_ID)) %>% 
  # remove those missing a WRFGRID ID
  filter(!is.na(WRFGRID_ID))


# read in populations ----
# zip_pop <- read_csv(paste0("../colorado_wildfire/data/shapefiles/",
#   "2014_ZCTA_Population/Populations_CO_ZCTA_2014.txt")) %>% 
zip_pop <- read_csv("./Data/5-year Populations/Populations_CO_ZCTA_2014.txt") %>% 
    #'  B01001e20    Male population 65-66
    #'  B01001e21    Male population 67-69
    #'  B01001e22    Male population 70-74
    #'  B01001e23    Male population 75-79
    #'  B01001e24    Male population 80-84
    #'  B01001e25    Male populiation 85+
    #'  B01001e44    Female population 65-66
    #'  B01001e45    Female population 67-69
    #'  B01001e46    Female population 70-74
    #'  B01001e47    Female population 75-79
    #'  B01001e48    Female population 80-84
    #'  B01001e49    Female populiation 85+
  #select(ZCTA5CE10, B01001e1) %>%  
  select(ZCTA5CE10, B01001e1, B01001e20, B01001e21, B01001e23, B01001e24,
         B01001e25, B01001e44, B01001e45, B01001e46, B01001e47, B01001e48) %>%
  mutate(age_65plus = (B01001e20 + B01001e21 + B01001e23 + B01001e24 +
                         B01001e25 + B01001e44 + B01001e45 + B01001e46 + 
                         B01001e47 + B01001e48)) %>%
  # need to only extract the last part of the GEOID to join 
  select(ZCTA5CE10, B01001e1, age_65plus) %>%
  rename(ZIP = ZCTA5CE10,
         total_pop = B01001e1) %>% 
  mutate(ZIP = as.character(ZIP))

# calculate rate by zipcode -----
zip_rate_period <- co_hosp %>% 
  filter(ADMYY != 15) %>% 
  group_by(ZIP) %>% 
  summarise(cvd_n = sum(cvd_dx), resp_n = sum(resp_dx)) %>% 
  # filter to zip codes in colorado
  filter(as.numeric(ZIP) >= 80001 & as.numeric(ZIP) <= 81658) %>% 
  # join with census denom
  right_join(zip_pop, by = "ZIP") %>% 
  # removing zip codes with 0 population
  filter(total_pop != 0) %>% 
  # calculate rate 
  mutate(cvd_n = ifelse(is.na(cvd_n), 0, cvd_n),
         resp_n = ifelse(is.na(resp_n), 0, resp_n),
         # set NA cvd resp n to 0
         cardiopulm_n = cvd_n + resp_n,
         total_pop5y = total_pop*5,
         total_65pop5y = age_65plus*5,
         # cardiopulm_per_100p5y = (cardiopulm_n/total_pop5y)*100,
         # cvd_per_100p5y = (cvd_n/total_pop5y)*100,
         # resp_per_100p5y = (resp_n/total_pop5y)*100,
         cardiopulm_per_100_65p5y = ifelse(total_65pop5y==0, NA,
                                           (cardiopulm_n/total_65pop5y)*100),
         cvd_per_100_65p5y = ifelse(total_65pop5y==0, NA,
                                    (cvd_n/total_65pop5y)*100),
         resp_per_100_65p5y = ifelse(total_65pop5y==0, NA,
                                     (resp_n/total_65pop5y)*100))

# save zip estimate file
write_path <- "./Data/CHA Data/co_zip_65plus_rate_period.csv"
write_csv(zip_rate_period, write_path)
