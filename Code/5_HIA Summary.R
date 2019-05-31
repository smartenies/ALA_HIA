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

pueblo_zips <- c("81001", "81003", "81004", "81005", "81006",
                 "81007", "81008", "81022", "81025")

springs_zips <- c("80938", "80939", "80951", "80918", "80919", "80920",
                  "80921", "80922", "80923", "80924", "80925", "80926",
                  "80927", "80929", "80903", "80904", "80905", "80906", 
                  "80907", "80908", "80909", "80910", "80911", "80914",
                  "80915", "80916", "80917", "80809", "80829", "80831",
                  "80840", "80913", "80817", "80925", "80929")

sfr_zips <-  c("80938", "80939", "80951", "81001", "81003", "81004", "81005",
               "81006", "81007", "81008", "81019", "81020", "81022", "81023",
               "81025", "81039", "81040", "80918", "80919", "80920", "80921",
               "80922", "80923", "80924", "80925", "80926", "80927", "80928",
               "80929", "80930", "80863", "80864", "80902", "80903", "80904",
               "80905", "80906", "80907", "80908", "80909", "80910", "80911",
               "80913", "80914", "80915", "80916", "80917", "81059", "81062", 
               "81067", "81069", "81089", "81131", "81143", "80106", "80118",
               "80808", "80809", "80813", "80814", "80816", "80817", "80819",
               "80132", "80133", "80135", "80449", "81212", "81221", "81222",
               "81223", "81226", "81232", "81233", "81240", "81244", "81252",
               "81253", "81155", "81201", "80820", "80827", "80829", "80830", 
               "80831", "80832", "80833", "80835", "80840", "80860")

#' For the report!!
#' What is the area of the receptor grid?
load(paste("./HIA Inputs/", pre[s], "cmaq_spatial.RData", sep=""))
cmaq_bound <- gConvexHull(cmaq_p)
cmaq_bound <- st_as_sf(cmaq_bound) %>% 
  st_transform(albers)
plot(st_geometry(cmaq_bound))
as.numeric(st_area(cmaq_bound)) / (1000**2) # area in km^2

load(paste("./HIA Inputs/", pre[s], "zcta.RData", sep=""))
zcta_sf <- st_as_sf(zcta) %>% 
  st_transform(albers) %>%
  st_union()
plot(st_geometry(zcta_sf))
as.numeric(st_area(zcta_sf)) / (1000**2) # area in km^2

#' How many people live in the study area?
zcta_ids <- unique(zcta@data$GEOID10)
pop <- read.table(pop_file, header=T, stringsAsFactors = F) 
sum(pop$total) #' all zip codes

zcta_pop <- pop %>%
  filter(GEOID %in% zcta_ids) %>%
  select(GEOID, total) %>%
  summarize(total_pop = sum(total))
zcta_pop #' ZIP codes in the study area

# For posterity, summarize CMAQ receptor concentrations----
rec_summary <- data.frame()
pol_names <- c("pm", "o3")

for (i in 1:length(pol_names)) {
  load(paste("./HIA Inputs/", pre[s], 
             pol_names[i], "_receptor_metrics.RData", sep=""))
  
  metrics <- names(exp_list)
  
  for (j in 1:length(metrics)) {
    exp_df <- exp_list[[j]]
    
    temp <- data.frame(pol = pol_names[i],
                       season = pre[s],
                       metric = metrics[j],
                       mean = mean(exp_df[,metrics[j]], na.rm=T),
                       sd = sd(exp_df[,metrics[j]], na.rm=T),
                       min = min(exp_df[,metrics[j]], na.rm=T),
                       median = median(exp_df[,metrics[j]], na.rm=T),
                       max = max(exp_df[,metrics[j]], na.rm=T))
    rec_summary <- bind_rows(rec_summary, temp)
    rm(temp)
  }
  
  load(paste("./HIA Inputs/", pre[s+1], 
             pol_names[i], "_receptor_metrics.RData", sep=""))
  
  metrics <- names(exp_list)
  
  for (j in 1:length(metrics)) {
    exp_df <- exp_list[[j]]
    
    temp <- data.frame(pol = pol_names[i],
                       season = pre[s+1],
                       metric = metrics[j],
                       mean = mean(exp_df[,metrics[j]], na.rm=T),
                       sd = sd(exp_df[,metrics[j]], na.rm=T),
                       min = min(exp_df[,metrics[j]], na.rm=T),
                       median = median(exp_df[,metrics[j]], na.rm=T),
                       max = max(exp_df[,metrics[j]], na.rm=T))
    rec_summary <- bind_rows(rec_summary, temp)
    rm(temp)
  }
}

write_xlsx(rec_summary,
           path=paste("./HIA Outputs/", pre[s], pre[s+1],
                      "receptor_conc_summary.xlsx",sep=""))

# First, summarize ZCTA Exposures----
exp_summary <- data.frame()
pol_names <- c("pm", "o3")

for (i in 1:length(pol_names)) {
  load(paste("./HIA Inputs/", pre[s], 
             pol_names[i], "_zcta_metrics.RData", sep=""))
  
  metrics <- names(zcta_list)
  
  for (j in 1:length(metrics)) {
    exp_df <- zcta_list[[j]]
    
    temp <- data.frame(pol = pol_names[i],
                       zctas = "all",
                       season = pre[s],
                       metric = metrics[j],
                       mean = mean(exp_df$wt_conc, na.rm=T),
                       sd = sd(exp_df$wt_conc, na.rm=T),
                       min = min(exp_df$wt_conc, na.rm=T),
                       median = median(exp_df$wt_conc, na.rm=T),
                       max = max(exp_df$wt_conc, na.rm=T))
    exp_summary <- bind_rows(exp_summary, temp)
    rm(temp)
    
    exp_df2 <- zcta_list[[j]] %>% 
      filter(GEOID10 %in% springs_zips)
    
    temp <- data.frame(pol = pol_names[i],
                       zctas = "springs",
                       season = pre[s],
                       metric = metrics[j],
                       mean = mean(exp_df2$wt_conc, na.rm=T),
                       sd = sd(exp_df2$wt_conc, na.rm=T),
                       min = min(exp_df2$wt_conc, na.rm=T),
                       median = median(exp_df2$wt_conc, na.rm=T),
                       max = max(exp_df2$wt_conc, na.rm=T))
    exp_summary <- bind_rows(exp_summary, temp)
    rm(temp)
    
    exp_df3 <- zcta_list[[j]] %>% 
      filter(GEOID10 %in% pueblo_zips)
    
    temp <- data.frame(pol = pol_names[i],
                       zctas = "pueblo",
                       season = pre[s],
                       metric = metrics[j],
                       mean = mean(exp_df3$wt_conc, na.rm=T),
                       sd = sd(exp_df3$wt_conc, na.rm=T),
                       min = min(exp_df3$wt_conc, na.rm=T),
                       median = median(exp_df3$wt_conc, na.rm=T),
                       max = max(exp_df3$wt_conc, na.rm=T))
    exp_summary <- bind_rows(exp_summary, temp)
    rm(temp)
    
  }
  
  load(paste("./HIA Inputs/", pre[s+1], 
             pol_names[i], "_zcta_metrics.RData", sep=""))
  
  metrics <- names(zcta_list)
  
  for (j in 1:length(metrics)) {
    exp_df <- zcta_list[[j]]
    
    temp <- data.frame(pol = pol_names[i],
                       zctas = "all",
                       season = pre[s+1],
                       metric = metrics[j],
                       mean = mean(exp_df$wt_conc, na.rm=T),
                       sd = sd(exp_df$wt_conc, na.rm=T),
                       min = min(exp_df$wt_conc, na.rm=T),
                       median = median(exp_df$wt_conc, na.rm=T),
                       max = max(exp_df$wt_conc, na.rm=T))
    exp_summary <- bind_rows(exp_summary, temp)
    rm(temp)
    
    exp_df2 <- zcta_list[[j]] %>% 
      filter(GEOID10 %in% springs_zips)
    
    temp <- data.frame(pol = pol_names[i],
                       zctas = "springs",
                       season = pre[s+1],
                       metric = metrics[j],
                       mean = mean(exp_df2$wt_conc, na.rm=T),
                       sd = sd(exp_df2$wt_conc, na.rm=T),
                       min = min(exp_df2$wt_conc, na.rm=T),
                       median = median(exp_df2$wt_conc, na.rm=T),
                       max = max(exp_df2$wt_conc, na.rm=T))
    exp_summary <- bind_rows(exp_summary, temp)
    rm(temp)
    
    exp_df3 <- zcta_list[[j]] %>% 
      filter(GEOID10 %in% pueblo_zips)
    
    temp <- data.frame(pol = pol_names[i],
                       zctas = "pueblo",
                       season = pre[s+1],
                       metric = metrics[j],
                       mean = mean(exp_df3$wt_conc, na.rm=T),
                       sd = sd(exp_df3$wt_conc, na.rm=T),
                       min = min(exp_df3$wt_conc, na.rm=T),
                       median = median(exp_df3$wt_conc, na.rm=T),
                       max = max(exp_df3$wt_conc, na.rm=T))
    exp_summary <- bind_rows(exp_summary, temp)
    rm(temp)
  }
}

write_xlsx(exp_summary,
           path=paste("./HIA Outputs/", pre[s], pre[s+1],
           "zcta_exposures.xlsx",sep=""))

# Second, map change in exposure ----
#' Winter and summer ozone and pm ("annual" averages)
pol_names <- c("pm", "o3")
pol_map <- c("PM\u2082.\u2085", "O\u2083")
unit_map <- c("(\u03BCg/m\u00B3)", "(ppb)")
season_map <- c("winter", "summer")

load("./Data/Spatial Data/power_plants.RData")
pp <- st_transform(pp, crs = ll_wgs84)

for (i in 1:length(pol_names)) {
  
  #' Get ZCTA IDs for plot-- season 1
  load(paste("./HIA Inputs/", pre[s], "zcta.RData", sep=""))
  zcta_ids <- unique(zcta$GEOID10)
  zcta_within_ids <- unique(zcta_within$GEOID10)
  
  rm(zcta, zcta_within)
  
  #' Read in change in "annual" concentration
  load(paste("./HIA Inputs/", pre[s], 
             pol_names[i], "_zcta_metrics.RData", sep=""))
  
  mean_df <- zcta_list[["ann_mean"]]
  
  #' Plot change in exposures
  load("./Data/Spatial Data/co_zcta.RData")
  zcta <- filter(co_zcta, GEOID10 %in% zcta_ids) %>%
    select(GEOID10) %>%
    left_join(mean_df, by="GEOID10")
  
  plot(st_geometry(zcta))
  
  title_name <- ifelse(is.na(cmaq_scenario[s]), "", "Change in")

  ggplot(data=zcta) +
    ggtitle(paste(title_name, season_map[1], pol_map[i], unit_map[i])) +
    geom_sf(aes(fill=wt_conc)) +
    geom_sf(data = pp, color = "red") +
    scale_fill_viridis(name = paste(pol_map[i], unit_map[i])) + 
    simple_theme
  ggsave(filename = paste("./HIA Outputs/Maps/", title_name, pre[s], pre[s+1],
                          season_map[1], "_", pol_map[i], ".jpeg", sep=""),
         device = "jpeg", dpi = 600)
  
  #' Get ZCTA IDs for plot-- season 2
  load(paste("./HIA Inputs/", pre[s+1], "zcta.RData", sep=""))
  zcta_ids <- unique(zcta$GEOID10)
  zcta_within_ids <- unique(zcta_within$GEOID10)
  
  rm(zcta, zcta_within)
  
  #' Read in change in "annual" concentration
  load(paste("./HIA Inputs/", pre[s+1], 
             pol_names[i], "_zcta_metrics.RData", sep=""))
  
  mean_df <- zcta_list[["ann_mean"]]
  
  #' Plot change in exposures
  load("./Data/Spatial Data/co_zcta.RData")
  zcta <- filter(co_zcta, GEOID10 %in% zcta_ids) %>%
    select(GEOID10) %>%
    left_join(mean_df, by="GEOID10")
  
  plot(st_geometry(zcta))
  
  ggplot(data=zcta) +
    ggtitle(paste(title_name, season_map[2], pol_map[i], unit_map[i])) +
    geom_sf(aes(fill=wt_conc)) +
    geom_sf(data = pp, color = "red") +
    scale_fill_viridis(name = paste(pol_map[i], unit_map[i])) + 
    simple_theme
  ggsave(filename = paste("./HIA Outputs/Maps/", title_name, pre[s], pre[s+1],
                          season_map[2], "_", pol_map[i], ".jpeg", sep=""),
         device = "jpeg", dpi = 600)
}

# Third, summarize health benefits ----

load(paste("./HIA Outputs/", pre[s], "zcta_impacts.RData", sep=""))
out_df_1 <- out_df
  
load(paste("./HIA Outputs/", pre[s+1], "zcta_impacts.RData", sep=""))
out_df_2 <- out_df
  
rm(out_df)
  
combined_df <- bind_rows(out_df_1, out_df_2) %>%
  mutate(zcta = as.character(zcta)) %>% 
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
  
#' Summarize impacts
total_df <- combined_df %>%
  group_by(pol, outcome) %>%
  summarise(median = round(sum(median, na.rm=T),2),
            zctas = "all",
            p2.5 = round(sum(p2.5, na.rm=T),2),
            p97.5 = round(sum(p97.5, na.rm=T),2),
            median_scaled = round(sum(median_scaled, na.rm=T),0),
            p2.5_scaled = round(sum(p2.5_scaled, na.rm=T),0),
            p97.5_scaled = round(sum(p97.5_scaled, na.rm=T),0),
            median_value = round(sum(median_value, na.rm=T),0),
            p2.5_value = round(sum(p2.5_value, na.rm=T),0),
            p97.5_value = round(sum(p97.5_value, na.rm=T),0))

total_df2 <- combined_df %>%
  filter(zcta %in% springs_zips) %>% 
  group_by(pol, outcome) %>%
  summarise(median = round(sum(median, na.rm=T),2),
            zctas = "springs",
            p2.5 = round(sum(p2.5, na.rm=T),2),
            p97.5 = round(sum(p97.5, na.rm=T),2),
            median_scaled = round(sum(median_scaled, na.rm=T),0),
            p2.5_scaled = round(sum(p2.5_scaled, na.rm=T),0),
            p97.5_scaled = round(sum(p97.5_scaled, na.rm=T),0),
            median_value = round(sum(median_value, na.rm=T),0),
            p2.5_value = round(sum(p2.5_value, na.rm=T),0),
            p97.5_value = round(sum(p97.5_value, na.rm=T),0))

total_df3 <- combined_df %>%
  filter(zcta %in% pueblo_zips) %>% 
  group_by(pol, outcome) %>%
  summarise(median = round(sum(median, na.rm=T),2),
            zctas = "pueblo",
            p2.5 = round(sum(p2.5, na.rm=T),2),
            p97.5 = round(sum(p97.5, na.rm=T),2),
            median_scaled = round(sum(median_scaled, na.rm=T),0),
            p2.5_scaled = round(sum(p2.5_scaled, na.rm=T),0),
            p97.5_scaled = round(sum(p97.5_scaled, na.rm=T),0),
            median_value = round(sum(median_value, na.rm=T),0),
            p2.5_value = round(sum(p2.5_value, na.rm=T),0),
            p97.5_value = round(sum(p97.5_value, na.rm=T),0))

total_df <- bind_rows(total_df, total_df2, total_df3)

save(out_df_1, out_df_2, combined_df, total_df,
     file = paste("./HIA Outputs/", pre[s], pre[s+1], "zcta_combined.RData",
                  sep=""))
write_xlsx(total_df,
           path=paste("./HIA Outputs/", pre[s], pre[s+1],
                      "zcta_impact_summary.xlsx",sep=""))

#' Map outcomes
#' Load the inequality indicators
ses <- read.table(ses_file, header=T, stringsAsFactors = F) %>%
  mutate(GEOID = gsub("86000US", "", GEOID)) %>%
  dplyr::rename(zcta = GEOID) %>%
  dplyr::select(zcta, total_pop, pct_poc, pct_nhw, med_income, 
                pct_hs_grad, pct_employed, pct_hh_not_limited_eng)

#' Clean up median impacts and calculate benefit rate
impacts <- ungroup(combined_df) %>%
  left_join(ses, by="zcta") %>% 
  select(zcta, pol, outcome, median_scaled, total_pop) %>% 
  mutate(pol = as.character(pol)) %>%
  mutate(rate = (median_scaled / total_pop) * rate_pop)

zcta_ids <- unique(impacts$zcta)

#' Map outcome rates across ZCTA's
load("./Data/Spatial Data/co_zcta.RData")

zcta_sf <- filter(co_zcta, GEOID10 %in% zcta_ids) %>%
  dplyr::rename(zcta = GEOID10) %>%
  select(zcta) %>%
  mutate(zcta = as.character(zcta)) %>%
  right_join(impacts, by="zcta")

if(exists(paste("./HIA Outputs/Maps/", pre[s], pre[s+1], sep=""))==F) {
  dir.create(paste("./HIA Outputs/Maps/", pre[s], pre[s+1], sep=""))
} 

for (i in 1:length(unique(zcta_sf$pol))) {
  zcta_pol <- zcta_sf %>%
    filter(pol == unique(zcta_sf$pol)[i])
  
  outcomes <- unique(zcta_pol$outcome)
  
  for (j in 1:length(outcomes)) {
    scale_name <- ifelse(is.na(cmaq_scenario[s]), "Impacts\nper",
                         "Avoided impacts\nper")
    
    zcta_map <- zcta_pol %>%
      filter(outcome == outcomes[j]) %>%
      mutate(rate = ifelse(rate < 0, 0, rate))
    
    ggplot(data = zcta_map) +
      ggtitle(paste("Attributable Outcome Rates:", out_dict[[outcomes[j]]])) +
      geom_sf(aes(fill = rate)) +
      geom_sf(data = pp, color = "red") +
      # stat_sf(data = pp, geom = "text") +
      scale_fill_viridis(name = paste(scale_name, rate_pop)) +
      simple_theme
    ggsave(filename = paste("./HIA Outputs/Maps/", pre[s], pre[s+1],
                            "/", unique(zcta_sf$pol)[i],
                            "_", outcomes[j], ".jpeg", sep=""),
           device = "jpeg", dpi = 600)
  }
}

# Inequality metrics for health rates ----

#' Calculate AI using rates (AI was designed for income, i.e. a positive, so 
#' we want to stick with benefits-- higher benefit rates are better)

atkinson <- impacts  %>%
  select(pol, outcome, rate) %>%
  mutate(rate = ifelse(rate < 0, NA, rate)) %>%
  group_by(pol, outcome) %>%
  summarise(mean_rate = mean(rate, na.rm=T),
            AI_rate = Atkinson(rate, parameter = 0.75, na.rm=T)) 

#' Calculate CI using rates 

#' Merge median impacts with indicators of inequality
impacts2 <- ungroup(combined_df) %>%
  select(zcta, pol, outcome, median_scaled) %>%
  mutate(pol = as.character(pol)) %>%
  left_join(ses, by="zcta") %>% 
  gather(ses_indic, ses_value, pct_poc:pct_hh_not_limited_eng) %>%
  mutate(rate = (median_scaled / total_pop) * rate_pop)

concentration <- impacts2  %>%
  select(pol, outcome, ses_indic, ses_value, rate) %>%
  mutate(rate = ifelse(rate < 0, NA, rate)) %>%
  group_by(pol, outcome, ses_indic) %>%
  summarise(CI_rate = calcSConc(rate, ses_value)[[1]][[1]]) 

#' Generate concentration curves
if(exists(paste("./HIA Outputs/CI Plots/", pre[s], pre[s+1], sep=""))==F) {
  dir.create(paste("./HIA Outputs/CI Plots/", pre[s], pre[s+1], sep=""))
} 

for (i in 1:length(unique(impacts2$pol))) {
  zcta_pol <- impacts2 %>%
    filter(pol == unique(zcta_sf$pol)[i])
  
  outcomes <- unique(zcta_pol$outcome)
  for (j in 1:length(outcomes)) {
    zcta_map <- zcta_pol %>%
      filter(outcome == outcomes[j]) %>%
      mutate(rate = ifelse(rate < 0, 0, rate))
    
    ses_indicators <- unique(zcta_map$ses_indic)
    
    for (k in 1:length(ses_indicators)) {
      y_lab <- ifelse(is.na(cmaq_scenario[s]), "Health impact rate:",
                      "Health benefit rate:")
      
      zcta_ses <- filter(zcta_map, ses_indic == ses_indicators[k])
      
      jpeg(filename = paste("./HIA Outputs/CI Plots/", pre[s], pre[s+1], "/", 
                            unique(zcta_sf$pol)[i], "_", outcomes[j], "_",
                            ses_indicators[k], ".jpeg", sep=""))
      curveConcent(zcta_ses$rate, zcta_ses$ses_value, col="red",
                   xlab = paste("Ranking by", ses_dict[[ses_indicators[k]]]),
                   ylab = paste(y_lab, out_dict[[outcomes[j]]]))
      dev.off()
    }
  }
}


inequality <- atkinson %>%
  right_join(concentration, by=c("pol", "outcome")) %>%
  spread(ses_indic, CI_rate)

colnames(inequality)[5:ncol(inequality)] <- paste("CI_", 
                                                  colnames(inequality)[5:ncol(inequality)], 
                                                  sep="")

save(inequality,
     file=paste("./HIA Outputs/", pre[s], pre[s+1], "zcta_inequality_benefits.RData",sep=""))
write_xlsx(inequality,
           path=paste("./HIA Outputs/", pre[s], pre[s+1], "zcta_inequality_benefits.xlsx",sep=""))  

#' Map the median income and percentage of the population that is POC 

#' Get ZCTA IDs for plot-- season 1
load(paste("./HIA Inputs/", pre[s], "zcta.RData", sep=""))
zcta_ids <- unique(zcta$GEOID10)
zcta_within_ids <- unique(zcta_within$GEOID10)

rm(zcta, zcta_within)

