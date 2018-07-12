#' =============================================================================
#' Project: American Lung Association HIA
#' Date created: March 29, 2018
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
#' This script examines the concentrations and health benefits, and SES 
#' characteristics of two ZIP codes in Springs 
#' 
#' The final results are saved as an Excel spreadsheet
#' =============================================================================

s <- 3

#' Load exposures
load(paste("./HIA Inputs/", pre[s], "pm_zcta_metrics.RData", sep=""))
pm_df_1 <- zcta_list[[1]]

load(paste("./HIA Inputs/", pre[s+1], "pm_zcta_metrics.RData", sep=""))
pm_df_2 <- zcta_list[[1]]

pm_df <- bind_rows(pm_df_1, pm_df_2) %>% 
  rename(zcta = GEOID10) %>% 
  group_by(zcta) %>% 
  summarise(mean_pm_diff = mean(wt_conc))

rm(pm_df_1, pm_df_2, zcta_list)

#' Load impacts
load(paste("./HIA Outputs/", pre[s], "zcta_impacts.RData", sep=""))
out_df_1 <- out_df
  
load(paste("./HIA Outputs/", pre[s+1], "zcta_impacts.RData", sep=""))
out_df_2 <- out_df
  
impact_df <- bind_rows(out_df_1, out_df_2) %>%
  filter(outcome == "mort_ac") %>% 
  group_by(zcta, pol, outcome) %>%
  summarise(median = sum(median, na.rm=T),
            p2.5 = sum(p2.5, na.rm=T),
            p97.5 = sum(p97.5, na.rm=T),
            median_scaled = sum(median_scaled, na.rm=T),
            p2.5_scaled = sum(p2.5_scaled, na.rm=T),
            p97.5_scaled = sum(p97.5_scaled, na.rm=T),
            median_value = sum(median_value, na.rm=T),
            p2.5_value = sum(p2.5_value, na.rm=T),
            p97.5_value = sum(p97.5_value, na.rm=T))
  
rm(out_df_1, out_df_2, out_df)

#' Load the inequality indicators
ses <- read.table(ses_file, header=T, stringsAsFactors = F) %>%
  mutate(GEOID = gsub("86000US", "", GEOID)) %>%
  dplyr::rename(zcta = GEOID) %>%
  dplyr::select(zcta, total_pop, pct_poc, pct_less_hs, 
                pct_limited_eng, pct_hh_pov, med_income)

#' Clean up median impacts and calculate rate 
inequality_df <- ungroup(impact_df) %>%
  left_join(ses, by="zcta") %>% 
  left_join(pm_df, by="zcta") %>% 
  mutate(rate = median_scaled / total_pop * rate_pop)

#' Compare the two ZCTAs
zcta_ids <- c("80906", "80924")

compare_df <- filter(inequality_df, zcta %in% zcta_ids)
write_xlsx(compare_df,
           path=paste("./HIA Outputs/Inequality Example.xlsx",sep=""))  


#' Map the ZCTAs
load("./Data/Spatial Data/power_plants.RData")
pp <- st_transform(pp, crs=ll_wgs84)

load("./HIA Inputs/HIA_Winter_zcta.RData")
ses_zctas <- st_as_sf(zcta) %>% 
  st_transform(crs=st_crs(pp))
low_ses <- filter(ses_zctas, GEOID10 == "80904")
high_ses <- filter(ses_zctas, GEOID10 == "80924")

ggplot(ses_zctas) +
  geom_sf(fill=NA, color="grey80") +
  geom_sf(data=low_ses, fill=NA, color="red") +
  geom_sf(data=high_ses, fill=NA, color="blue") +
  geom_sf(data=pp, fill="black", size = 2) +
  geom_text(aes(x = -104.9, y = 38.91, label = "80904"), col="red") +
  geom_text(aes(x = -104.7, y = 38.92, label = "80924"), col="blue") +
  geom_text(aes(x = -104.82, y = 38.8, label = "Martin Drake Power Plant"), 
            col="black") +
  coord_sf(xlim = c(-105.1, -104.5), ylim = c(38.6, 39.1)) +
  xlab("") + ylab("") +
  simple_theme
ggsave(filename = "./HIA Outputs/Maps/ZCTAs for comparion.jpeg",
       device = "jpeg", dpi = 600)
