# ------------------------------------------------------------------------------
# Title: Estimating Colorado 2010-2014 period mortality rates by ZIP Codes
# Author: Ryan Gan
# Date Created: 2017-12-20
#
# Modified 01-12-18 by SEM to subset data by age (>= 30 years)
# ------------------------------------------------------------------------------

# library ----
library(tidyverse)

# setup -----
# read in mortality data -----

# ICD 10 A00-R00 for detecting non-accidental cause of death
non_accidental_list <- LETTERS[1:18]

co_mortality <- read_csv("./Data/VS Data/co_death_1016.csv") %>% 
    # need adults > 29 years of age
  filter(ageid == 1 & age >=30 & age < 999) %>%
    # create date variable based on date of death
  mutate(date = as.Date(dod, format="%m%d%Y"),
    # non-accidental cause of death
    # find non-accidental deaths
    non_accidental = ifelse(stringr::str_detect(ucod, 
      paste(non_accidental_list, collapse = '|')),1,0), 
    ZIP = as.character(repzip))
  
# read in populations ----
zip_pop <- read_csv("./Data/5-year Populations/Populations_CO_ZCTA_2014.txt") %>% 
  #'  B01001e12    Male population 30-34
  #'  B01001e13    Male population 35-39
  #'  B01001e14    Male population 40-44
  #'  B01001e15    Male population 45-49
  #'  B01001e16    Male population 50-54
  #'  B01001e17    Male population 55-59
  #'  B01001e18    Male population 60-61
  #'  B01001e19    Male population 62-64 
  #'  B01001e20    Male population 65-66
  #'  B01001e21    Male population 67-69
  #'  B01001e22    Male population 70-74
  #'  B01001e23    Male population 75-79
  #'  B01001e24    Male population 80-84
  #'  B01001e25    Male populiation 85+
  #'  B01001e36    Female population 30-34
  #'  B01001e37    Female population 35-39
  #'  B01001e38    Female population 40-44
  #'  B01001e39    Female population 45-49
  #'  B01001e40    Female population 50-54
  #'  B01001e41    Female population 55-59
  #'  B01001e42    Female population 60-61
  #'  B01001e43    Female population 62-64 
  #'  B01001e44    Female population 65-66
  #'  B01001e45    Female population 67-69
  #'  B01001e46    Female population 70-74
  #'  B01001e47    Female population 75-79
  #'  B01001e48    Female population 80-84
  #'  B01001e49    Female populiation 85+
  select(ZCTA5CE10, B01001e1, 
         B01001e12, B01001e13, B01001e14, B01001e15, B01001e16, B01001e17, 
         B01001e18, B01001e19, B01001e20, B01001e21, B01001e22, B01001e23, 
         B01001e24, B01001e25, 
         B01001e36, B01001e37, B01001e38, B01001e39, B01001e40, B01001e41,
         B01001e42, B01001e43, B01001e44, B01001e45, B01001e46, B01001e47, 
         B01001e48, B01001e49) %>%
  mutate(age_30plus = (B01001e12 + B01001e13 + B01001e14 + B01001e15 + 
                       B01001e16 + B01001e17 + B01001e18 + B01001e19 + 
                       B01001e20 + B01001e21 + B01001e22 + B01001e23 + 
                       B01001e24 + B01001e25 + 
                       
                       B01001e36 + B01001e37 + B01001e38 + B01001e39 + 
                       B01001e40 + B01001e41 + B01001e42 + B01001e43 + 
                       B01001e44 + B01001e45 + B01001e46 + B01001e47 + 
                       B01001e48 + B01001e49)) %>%
  # need to only extract the last part of the GEOID to join 
  select(ZCTA5CE10, B01001e1, age_30plus) %>%
  rename(ZIP = ZCTA5CE10,
         total_pop = B01001e1) %>% 
  mutate(ZIP = as.character(ZIP))

# calculate mortality rate by zipcode -----
co_mortality_zip <- co_mortality %>% 
  filter(yr >= 2010 & yr <= 2014) %>% 
  group_by(ZIP) %>% 
  summarise(all_cause_n = n(), non_accidental_n = sum(non_accidental)) %>% 
  # filter to zip codes in colorado
  filter(as.numeric(ZIP) >= 80001 & as.numeric(ZIP) <= 81658) %>% 
  # join with census denom
  right_join(zip_pop, by = "ZIP") %>% 
  # removing zip codes with 0 population
  filter(age_30plus != 0) %>% 
  # calculate rate 
  mutate(all_cause_n = ifelse(is.na(all_cause_n),0,all_cause_n),
         non_accidental_n = ifelse(is.na(non_accidental_n),0, non_accidental_n),
         # set NA cvd resp n to 0
         total_30plus5y = age_30plus*5,
         all_cause_per_1000_30plus5y = (all_cause_n/total_30plus5y)*1000,
         non_accidental_1000_30plus5y = (non_accidental_n/total_30plus5y)*1000)

# save zip estimate file
write_path <- "./Data/VS Data/co_mortality_zip_30plus_rate_period.csv"
write_csv(co_mortality_zip, write_path)


