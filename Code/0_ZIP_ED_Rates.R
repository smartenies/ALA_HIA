# ------------------------------------------------------------------------------
# Title: Estimating Colorado 2010-2014 period ED rates in ZIP Codes
# Author: Ryan Gan
# Date Created: 2017-12-6
# 
# Edited 2018-01-22 by SEM to include the SE for the rate
# SE is calculated as count / sqrt(pop) (as in Woodward 2005, page 152)
# Assumes rate follows the Poisson distribution
#
# Edited 2018-01-30 to use Poisson regression to caluclate rates and standard 
# errors. Method above gave HUGE errors on the rates... still need to see if
# this is the right way to do things
# ------------------------------------------------------------------------------

# library ----
library(tidyverse)

# setup -----
# read in the dataframe with outcomes 
# path to my colorado repo folder so I don't have to copy the file
#hosp_path <- "../colorado_wildfire/data/health/co_hosp_w_outcome_df.csv"
hosp_path <- "./Data/CHA Data/co_hosp_w_outcome_df.csv"


# read file and create some variables -----
co_hosp <- read_csv(hosp_path) %>% 
  # noticed broomfield county_geo 014 was sometimes assigned county_final 159
  # there is no county 159. All other spatial indicators suggest broomfield 014
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
zip_pop <- read_csv("./Data/ACS_2010_2014/co_populations.csv") %>%
  select(GEOID, total) %>%
  # need to only extract the last part of the GEOID to join
  rename(ZIP = GEOID,
         total_pop = total) %>%
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
         total_pop5y = total_pop*5)#,
         # cardiopulm_per_100p5y = (cardiopulm_n/total_pop5y)*100,
         # cardiopulm_per_100p5y = (cardiopulm_n/sqrt(total_pop5y))*100,
         # cvd_per_100p5y = (cvd_n/total_pop5y)*100,
         # cvd_per_100p5y_se = (cvd_n/sqrt(total_pop5y))*100,
         # resp_per_100p5y = (resp_n/total_pop5y)*100,
         # resp_per_100p5y_se = (resp_n/sqrt(total_pop5y))*100)

#' use Poisson regression to calculate rates and standard errors
for (i in 1:nrow(zip_rate_period)) {
  df <- zip_rate_period[i,]
  cp_mod <- glm(cardiopulm_n ~ 1, offset=log(total_pop5y/1000), 
                family=poisson, data=df)
  zip_rate_period[i,"cp_per_100_5y"] <- exp(coef(summary(cp_mod))[1,1])
  zip_rate_period[i,"cp_per_100_5y_se"] <- exp(coef(summary(cp_mod))[1,2])
  
  cv_mod <- glm(cvd_n ~ 1, offset=log(total_pop5y/1000), 
                family=poisson, data=df)
  zip_rate_period[i,"cvd_per_100_5y"] <- exp(coef(summary(cv_mod))[1,1])
  zip_rate_period[i,"cvd_per_100_5y_se"] <- exp(coef(summary(cv_mod))[1,2])
  
  res_mod <- glm(resp_n ~ 1, offset=log(total_pop5y/1000), 
                family=poisson, data=df)
  zip_rate_period[i,"res_per_100_5y"] <- exp(coef(summary(res_mod))[1,1])
  zip_rate_period[i,"res_per_100_5y_se"] <- exp(coef(summary(res_mod))[1,2])
}

# save zip estimate file
write_path <- "./Data/co_zip_rate_period.csv"
write_csv(zip_rate_period, write_path)
