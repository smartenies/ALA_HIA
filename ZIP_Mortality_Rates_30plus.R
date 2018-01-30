# ------------------------------------------------------------------------------
# Title: Estimating Colorado 2010-2014 period mortality rates by ZIP Codes
# Author: Ryan Gan
# Date Created: 2017-12-20
#
# Modified 01-12-18 by SEM to subset data by age (>= 30 years)
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
zip_pop <- read_csv("./Data/ACS_2010_2014/co_populations.csv") %>%
  select(GEOID, p30_99) %>%
  # need to only extract the last part of the GEOID to join
  rename(ZIP = GEOID) %>%
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
  filter(p30_99 != 0) %>% 
  # calculate rate 
  mutate(all_cause_n = ifelse(is.na(all_cause_n),0,all_cause_n),
         non_accidental_n = ifelse(is.na(non_accidental_n),0, non_accidental_n),
         # set NA cvd resp n to 0
         p30plus5yr = p30_99*5)#,
         # all_cause_per_1000_30plus5y = (all_cause_n/p30plus5yr)*1000,
         # all_cause_per_1000_30plus5y_se = (all_cause_n/sqrt(p30plus5yr))*1000,
         # non_accidental_1000_30plus5y = (non_accidental_n/p30plus5yr)*1000,
         # non_accidental_1000_30plus5y_se = (non_accidental_n/sqrt(p30plus5yr))*1000)

#' use Poisson regression to calculate rates and standard errors
for (i in 1:nrow(co_mortality_zip)) {
  df <- co_mortality_zip[i,]
  ac_mod <- glm(all_cause_n ~ 1, offset=log(p30plus5yr/1000), 
                family=poisson, data=df)
  co_mortality_zip[i,"ac_per_1000_30plus5y"] <- exp(coef(summary(ac_mod))[1,1])
  co_mortality_zip[i,"ac_per_1000_30plus5y_se"] <- exp(coef(summary(ac_mod))[1,2])
  
  na_mod <- glm(non_accidental_n ~ 1, offset=log(p30plus5yr/1000), 
                family=poisson, data=df)
  co_mortality_zip[i,"na_per_1000_30plus5y"] <- exp(coef(summary(na_mod))[1,1])
  co_mortality_zip[i,"na_per_1000_30plus5y_se"] <- exp(coef(summary(na_mod))[1,2])
}

# save zip estimate file
write_path <- "./Data/VS Data/co_mortality_zip_30plus_rate_period.csv"
write_csv(co_mortality_zip, write_path)


