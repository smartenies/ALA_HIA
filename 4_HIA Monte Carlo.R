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
#' This script calculates the attributable health impacts for each pollutant,
#' exposure metric, outcome, and ZIP code using a Monte Carlo analysis (based
#' on Ryan Gan's ozone HIA)
#' =============================================================================

print("HIA")

#' load the HIA databases
cr <- read.table(cr_file, header=T, stringsAsFactors = F)
values <- cr[,c("outcome", "value_2024")]
pop <- read.table(pop_file, header = T, stringsAsFactors = F) 
rate <- read.table(rate_file, header=T, stringsAsFactors = F)

#' subset CR dataframe to only include outcomes for which we have rates
r_out <- unique(gsub("_se", "", colnames(rate)))
cr <- cr[which(cr$outcome %in% r_out),]
cr <- cr[which(cr$outcome != "hosp_mi"),]

#' load the ZCTA file
load(paste("./HIA Inputs/", pre[s], "zcta.RData", sep=""))

#' Loop through pollutants, metrics, and days to estimate attibutable health
#' impacts, scaled to a full year
pol_names <- c("pm", "o3")

#' Empty data frame for results
out_df <- data.frame()

#' set the see for generating all the distributions
set.seed(sim_seed)

for (i in 1:length(pol_names)) {

  #' load ZCTA exposures
  load(paste("./HIA Inputs/", pre[s], pol_names[i], "_zcta_metrics.RData",sep=""))
  
  metrics <- names(zcta_list)
  
  #' Loop through metrics
  for (j in 1:length(metrics)) {
    #' Get ZCTA exposures
    zcta_exp <- zcta_list[[j]]
    
    #' Get subset of concentration-response coefficients
    m_cr <- cr[which(cr$pol == pol_names[i] & cr$metric == metrics[j]),]
    if(nrow(m_cr)==0) next
    
    #' Get list of outcomes
    outcomes <- unique(m_cr$outcome)

    #' Loop through outcomes
    for (k in 1:length(outcomes)) {
      
      print(paste("Pollutant ", i, " of ", length(pol_names), 
                  "; Metric ", j, " of ", length(metrics), 
                  "; Outcome ", k, " of ", length(outcomes), sep=""))
      
      #' CR coefficient (same distribution for all ZCTAs)
      beta <- m_cr[which(m_cr$outcome==outcomes[k]), "cr_beta"]
      beta_se <- m_cr[which(m_cr$outcome==outcomes[k]), "cr_se"]
      beta_dist <- rnorm(n = mc_n, mean = beta, sd = beta_se)
      # ggplot(data = as.data.frame(beta_dist), aes(x=beta_dist)) +
      #   geom_density() +
      #   simple_theme

      #' subset rates by outcome
      rate_df <- rate[,c(1, grep(outcomes[k], colnames(rate)))]
      colnames(rate_df) <- c("GEOID", "rate", "se")
      rate_df$rate <- ifelse(rate_df$rate == 9999 | is.infinite(rate_df$rate), 
                             NA, rate_df$rate)
      rate_df$se <- ifelse(rate_df$se == 9999 | is.infinite(rate_df$se), 
                           NA, rate_df$se)
      
      #' Loop through zctas
      zctas <- unique(as.character(zcta$GEOID10))
      for (l in 1:length(zctas)) {
        
        #' empty vector to hold the daily estimates for each ZCTA
        hia <- vector()
        
        #' Baseline rate (y0)
        #' Only use a distribution if a standard error is available
        #' skip this ZCTA if the baseline rate is NA
        y0 <- rate_df[which(rate_df$GEOID==zctas[l]),"rate"]
        y0_se <- rate_df[which(rate_df$GEOID==zctas[l]),"se"]
        
        if (length(y0)==0) next
        
        if (is.na(y0_se)) {
          y0_dist <- y0
        } else {
          y0_dist <- rnorm(n = mc_n, mean = y0, sd = y0_se)
        }
        
        #' Population at risk
        age <- m_cr[which(m_cr$outcome == outcomes[k]), "age_group"]
        risk <- pop[which(pop$GEOID == zctas[l]), age]
        risk_se <- pop[which(pop$GEOID == zctas[l]), paste(age, "se", sep="_")]
        risk_dist <- rnorm(n = mc_n, mean = risk, sd = risk_se)
        
        #' Loop through days
        days <- unique(zcta_exp$day) 
        
        #' Monte Carlo for HIA
        for (m in 1:length(days)) {
          exp_df <- zcta_exp[which(zcta_exp$day == days[m]),]

          #' Exposure
          exp <- as.numeric(exp_df[which(exp_df$GEOID10==zctas[l]), "wt_conc"])
          exp_sd <- as.numeric(exp_df[which(exp_df$GEOID10==zctas[l]), "wt_conc_sd"])
          
          exp_dist <- rnorm(n = mc_n, mean = exp, sd = exp_sd)
          
          #' loop through n iterations of the MC
          for (p in 1:mc_n) {
            n_beta <- sample(beta_dist, 1, replace = T)
            n_y0 <- sample(y0_dist, 1, replace = T)
            n_exp <- sample(exp_dist, 1, replace = T)
            n_risk <- sample(risk_dist, 1, replace = T)
            
            #' apply the HIF
            hia[[length(hia)+1]] <- (n_y0*(1-exp((-n_beta)*(n_exp)))*n_risk)
          }
        }
        
        #' find median, 2.5th and 97.5th percentiles for the daily estimates
        #' all rates are daily, so need to scale by the number of days modeled
        scale <- d_per_y / length(days)
                        
        temp <- data.frame(zcta = zctas[l],
                           n_days = length(days),
                           scale = scale,
                           outcome = as.character(outcomes[k]),
                           metric = as.character(metrics[j]),
                           pol = pol_names[i],
                           median = median(hia, na.rm = T),
                           p2.5 = quantile(hia, probs = 0.025, na.rm = T),
                           p97.5 = quantile(hia, probs = 0.975, na.rm = T))
        temp$median_scaled <- temp$median * temp$scale
        temp$p2.5_scaled <- temp$p2.5 * temp$scale
        temp$p97.5_scaled <- temp$p97.5 * temp$scale
        
        #' Append results
        out_df <- rbind(out_df, temp)
        rm(temp, hia)
      }
    }
  }
}

#' Valuate the impacts and summarize by pollutant and outcome
detach(package:plyr)
out_df$outcome <- as.character(out_df$outcome)
out_df <- left_join(out_df, values, by="outcome") %>%
  mutate(median_value = median_scaled * value_2024,
         p2.5_value = p2.5_scaled * value_2024,
         p97.5_value = p97.5_scaled * value_2024)

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

save(out_df, total_df,
     file=paste("./HIA Outputs/", pre[s], "zcta_impacts.RData",sep=""))
write_xlsx(total_df,
           path=paste("./HIA Outputs/", pre[s], "zcta_impacts_summary.xlsx",
                      sep=""))  
  
  