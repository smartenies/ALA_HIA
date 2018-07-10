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
#' This script summarizes exposures and health benefits at the ZCTA level,
#' calculates the rate of attributable impacts for each ZCTA, maps
#' these rates, and then generates the AI and CI estimates for each pollutant-
#' outcome combination. Concentration curves are also plotted and saved
#' 
#' The final results are saved as an Excel spreadsheet
#' =============================================================================

#' load the ZCTA file and the power plants
load(paste("./HIA Inputs/", pre[s], "zcta.RData", sep=""))
load("./Data/Spatial Data/power_plants.RData")

#' load the impacts file
#' Combined the winter and summer impacts
load(paste("./HIA Outputs/", pre[s], "zcta_impacts.RData",sep=""))
out_df$zcta <- as.character(out_df$zcta)

out_df2 <- out_df2

load(paste("./HIA Outputs/", pre[s+1], "zcta_impacts.RData",sep=""))
out_df$zcta <- as.character(out_df$zcta)

out_df2 <- bind_rows(out_df2, out_df)
rm(out_df)



#' Summarize impacts
total_df <- out_df %>%
  group_by(pol, outcome) %>%
  summarise(median = round(sum(median, na.rm=T),2),
            p2.5 = round(sum(p2.5, na.rm=T),2),
            p97.5 = round(sum(p97.5, na.rm=T),2),
            median_scaled = round(sum(median_scaled, na.rm=T),0),
            p2.5_scaled = round(sum(p2.5_scaled, na.rm=T),0),
            p97.5_scaled = round(sum(p97.5_scaled, na.rm=T),0),
            median_value = round(sum(median_value, na.rm=T),0),
            p2.5_value = round(sum(p2.5_value, na.rm=T),0),
            p97.5_value = round(sum(p97.5_value, na.rm=T),0))


#' Load the inequality indicators
ses <- read.table(ses_file, header=T, stringsAsFactors = F) %>%
  mutate(GEOID = gsub("86000US", "", GEOID)) %>%
  dplyr::rename(zcta = GEOID) %>%
  dplyr::select(zcta, total_pop, pct_poc, pct_less_hs, 
                pct_limited_eng, pct_hh_pov, med_income)

#' Clean up median impacts and calculate rate
impacts <- out_df2 %>%
  left_join(ses, by="zcta") %>% 
  select(zcta, pol, outcome, median_scaled, total_pop) %>% 
  mutate(pol = as.character(pol)) %>%
  mutate(rate = (median_scaled / total_pop) * rate_pop) 

#' Map outcome rates across ZCTA's
load("./Data/Spatial Data/co_zcta") 

zcta_sf <- filter(co_zcta, GEOID10 %in% zcta$GEOID10) %>%
  dplyr::rename(zcta = GEOID10) %>%
  select(zcta) %>%
  mutate(zcta = as.character(zcta)) %>%
  right_join(impacts, by="zcta")

if(exists(paste("./HIA Outputs/Maps/", pre[s], sep=""))==F) {
  dir.create(paste("./HIA Outputs/Maps/", pre[s], sep=""))
} 

for (i in 1:length(unique(zcta_sf$pol))) {
  zcta_pol <- zcta_sf %>%
    filter(pol == unique(zcta_sf$pol)[i])
  
  for (j in 1:length(unique(zcta_pol$outcome))) {
    zcta_map <- zcta_pol %>%
      filter(outcome == unique(zcta_pol$outcome)[j]) %>%
      mutate(rate = ifelse(rate < 0, 0, rate))
    
    ggplot(data = zcta_map) +
      geom_sf(aes(fill = rate)) +
      geom_sf(data = pp, color = "red") +
      # stat_sf(data = pp, geom = "text")
      scale_fill_viridis(name = paste(unique(zcta_sf$pol)[i], "-attributable\n",
                                      unique(zcta_pol$outcome)[j], 
                                      "\nrate per ", rate_pop, sep="")) +
      simple_theme
    ggsave(filename = paste("./HIA Outputs/Maps/", pre[s], "/", 
                            unique(zcta_sf$pol)[i],
                            "_", unique(zcta_pol$outcome)[j], ".jpeg", sep=""),
           device = "jpeg", dpi = 600)
  }
}
  
#' Calculate AI and CI values for each outcome and SES indicator
detach(package:plyr)

atkinson <- impacts  %>%
  select(pol, outcome, rate) %>%
  mutate(rate = ifelse(rate < 0, NA, rate)) %>%
  group_by(pol, outcome) %>%
  summarise(mean_rate = mean(rate, na.rm=T),
            AI = Atkinson(rate, parameter = 0.75, na.rm=T)) 

#' Merge median impacts with indicators of inequality
impacts2 <- out_df %>%
  select(zcta, pol, outcome, median_scaled) %>%
  mutate(pol = as.character(pol)) %>%
  left_join(ses, by="zcta") %>% 
  gather(ses_indic, ses_value, pct_poc:med_income) %>%
  mutate(rate = (median_scaled / total_pop) * rate_pop)

concentration <- impacts2  %>%
  select(pol, outcome, ses_indic, ses_value, rate) %>%
  mutate(rate = ifelse(rate < 0, NA, rate)) %>%
  group_by(pol, outcome, ses_indic) %>%
  summarise(conc_index = calcSConc(rate, ses_value)[[1]][[1]]) 

#' Generate concentration curves
if(exists(paste("./HIA Outputs/CI Plots/", pre[s], sep=""))==F) {
  dir.create(paste("./HIA Outputs/CI Plots/", pre[s], sep=""))
} 

for (i in 1:length(unique(impacts2$pol))) {
  zcta_pol <- impacts2 %>%
    filter(pol == unique(zcta_sf$pol)[i])
  
  for (j in 1:length(unique(zcta_pol$outcome))) {
    zcta_map <- zcta_pol %>%
      filter(outcome == unique(zcta_pol$outcome)[j]) %>%
      mutate(rate = ifelse(rate < 0, 0, rate))
    
    for (k in 1:length(unique(zcta_map$ses_indic))) {
      zcta_ses <- filter(zcta_map, ses_indic == unique(zcta_map$ses_indic)[k])
        
      jpeg(filename = paste("./HIA Outputs/CI Plots/", pre, "/", unique(zcta_sf$pol)[i],
                          "_", unique(zcta_pol$outcome)[j], "_",
                          unique(zcta_map$ses_indic)[k], ".jpeg", sep=""))
      curveConcent(zcta_ses$rate, zcta_ses$ses_value, col="red",
                   xlab = paste("Ranking by", unique(zcta_map$ses_indic)[k]),
                   ylab = paste("Cumulative rate of", unique(zcta_sf$pol)[i],
                                unique(zcta_pol$outcome)[j]))
      dev.off()
    }
  }
}


inequality <- atkinson %>%
  right_join(concentration, by=c("pol", "outcome")) %>%
  spread(ses_indic, conc_index)

colnames(inequality)[5:9] <- paste("CI_", colnames(inequality)[5:9], sep="")


save(inequality,
     file=paste("./HIA Outputs/", pre[s], "zcta_inequality.RData",sep=""))
write_xlsx(inequality,
           path=paste("./HIA Outputs/", pre[s], "zcta_inequality.xlsx",sep=""))  












