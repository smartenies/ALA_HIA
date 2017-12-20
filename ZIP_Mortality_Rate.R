# ------------------------------------------------------------------------------
# Title: Estimating Colorado 2010-2014 period mortality rates by ZIP Codes
# Author: Ryan Gan
# Date Created: 2017-12-20
# ------------------------------------------------------------------------------

# library ----
library(tidyverse)

# setup -----
# read in mortality data -----
# path to my colorado repo folder so I don't have to copy the file
m_path <- "../colorado_wildfire/data/health/co_death_1016.csv"

# ICD 10 A00-R00 for detecting non-accidental cause of death
non_accidental_list <- LETTERS[1:18]

co_mortality <- read_csv(m_path) %>% 
  # create date variable based on date of death
  mutate(date = as.Date(dod, format="%m%d%Y"),
    # non-accidental cause of death
    # find non-accidental deaths
    non_accidental = ifelse(stringr::str_detect(ucod, 
      paste(non_accidental_list, collapse = '|')),1,0), 
    ZIP = as.character(repzip))
  
# read in populations ----
zip_pop <- read_csv(paste0("../colorado_wildfire/data/shapefiles/",
    "2014_ZCTA_Population/Populations_CO_ZCTA_2014.txt")) %>% 
  select(ZCTA5CE10, B01001e1) %>% 
  # need to only extract the last part of the GEOID to join 
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
  filter(total_pop != 0) %>% 
  # calculate rate 
  mutate(all_cause_n = ifelse(is.na(all_cause_n),0,all_cause_n),
         non_accidental_n = ifelse(is.na(non_accidental_n),0,non_accidental_n),
         # set NA cvd resp n to 0
         total_pop5y = total_pop*5,
         all_cause_per_10005y = (all_cause_n/total_pop5y)*1000,
         non_accidental_per_1000p5y = (non_accidental_n/total_pop5y)*1000)

# save zip estimate file
write_path <- "./Data/co_mortality_zip_rate_period.csv"
write_csv(co_mortality_zip, write_path)


