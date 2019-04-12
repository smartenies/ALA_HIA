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
#' This script pools the CR coefficients for the HIA
#'      PM2.5 coefficients are for the full year
#'      O3 coefficients are for warm season months only
#'      
#' Edit 01.08.18: Added a pooled estimate for PM2.5 and asthma exacerbations
#' using the description in the 2012 EPA RIA for the PM2.5 NAAQS (5-44)
#' =============================================================================

#' =============================================================================
#' Read in the datasets and subset by pollutant and metric
#' =============================================================================

#' -----------------------------------------------------------------------------
#' Read in the Excel sheet with the full table
#' -----------------------------------------------------------------------------

full_cr <- read_excel("./Data/Outcome CRs and Valuation.xlsx",
                      sheet=1, skip=13)
colnames(full_cr) <- tolower(colnames(full_cr))
colnames(full_cr) <- gsub(" ", "_", colnames(full_cr))

pm_cr <- full_cr[which(full_cr$pollutant=="pm"),]
o3_cr <- full_cr[which(full_cr$pollutant=="o3"),] 

save(pm_cr, o3_cr, full_cr, file="./Data/CR datasets.RData")

#' =============================================================================
#' Pool the CRs for each pollutant and outcome using the 'metafor' package
#' =============================================================================

load("./Data/CR datasets.RData")

pols <- c("pm", "o3")

pooled_crs <- data.frame(matrix(ncol=9, nrow=0))
colnames(pooled_crs) <- c("pol", "metric", "outcome", "n", 
                          "cr_beta", "cr_se", "I^2", "Q", "Q_p")

for (i in 1:length(pols)) {
  cr_df <- full_cr[which(full_cr$pollutant == pols[i]),]
  metrics <- unique(cr_df$metric)
  
  for (j in 1:length(metrics)) {
    met_df <- cr_df[which(cr_df$metric == metrics[j]),]
    outcomes <- unique(met_df$outcome)
    
    for (k in 1:length(outcomes)) {
      df <- met_df[which(met_df$outcome == outcomes[k]),] 
      
      #' #' Skip the meta analysis if there is only one study
      #' if (nrow(df)==1) {
      #'   pooled_crs[nrow(pooled_crs)+1,1] <- pols[i]
      #'   pooled_crs[nrow(pooled_crs),2] <- metrics[j]
      #'   pooled_crs[nrow(pooled_crs),3] <- outcomes[k]
      #'   pooled_crs[nrow(pooled_crs),4] <- nrow(df)
      #'   pooled_crs[nrow(pooled_crs),5] <- df$cr
      #'   pooled_crs[nrow(pooled_crs),6] <- df$se
      #'   next
      #' }
      
      #' Pool the coefficients using a random effects model
      rma_model <- rma(yi = cr, sei = se, data=df)
      pooled_crs[nrow(pooled_crs)+1,1] <- pols[i]
      pooled_crs[nrow(pooled_crs),2] <- metrics[j]
      pooled_crs[nrow(pooled_crs),3] <- outcomes[k]
      pooled_crs[nrow(pooled_crs),4] <- nrow(df)
      pooled_crs[nrow(pooled_crs),5] <- rma_model$beta[,1]
      pooled_crs[nrow(pooled_crs),6] <- rma_model$se
      pooled_crs[nrow(pooled_crs),7] <- rma_model$I2
      pooled_crs[nrow(pooled_crs),8] <- rma_model$QE
      pooled_crs[nrow(pooled_crs),9] <- rma_model$QEp
      
      #' Generate the forest plot and funnel plot
      # f_name <- paste("./Data/CR Plots/",pols[i], " ", metrics[j], 
      #                 " ", outcomes[k]," forest.jpeg", sep="")
      # fun_name <- paste("./Data/CR Plots/",pols[i], " ", metrics[j], 
      #                 " ", outcomes[k]," funnel.jpeg", sep="")
      # 
      # jpeg(f_name)
      # forest(rma_model, slab=df$source, digits=4,
      #        mlab=paste(pols[i], metrics[j], "\n", outcomes[k]))
      # dev.off()
      # 
      # jpeg(fun_name)
      # funnel(rma_model, digits=4,
      #        xlab=paste("Observed", pols[i], metrics[j], outcomes[k]))
      # dev.off()
    }
  }
}

#' =============================================================================
#' Additional estimate of asthma exacerbations for PM2.5
#' Based on 2012 RIA for the PM2.5 NAAQS:
#'      1) Pool estimates for shortness of breath and cough using random effects
#'      2) Pool SoB/cough with wheeze using random effects
#' =============================================================================

library(metafor)

ast_outcomes1 <- c("minor_astc", "minor_asts")
ast_outcomes2 <- c("minor_astw")

pm_ast_cr1 <- full_cr[which(full_cr$pollutant=="pm" & full_cr$outcome %in% ast_outcomes1),]
pm_ast_cr2 <- full_cr[which(full_cr$pollutant=="pm" & full_cr$outcome %in% ast_outcomes2),]

#' meta-analysis for shortness of breath and cough
rma_model1 <- rma(yi = cr, sei = se, data=pm_ast_cr1)

#' get the first pooled beta and SE into the second dataset 
pm_ast_cr2[nrow(pm_ast_cr2)+1,1] <- "pm"
pm_ast_cr2[nrow(pm_ast_cr2),3] <- "minor_ast"
pm_ast_cr2[nrow(pm_ast_cr2),16] <- rma_model1$beta[,1]
pm_ast_cr2[nrow(pm_ast_cr2),19] <- rma_model1$se

#' meta-analysis for the first pooled estimate and wheeze
rma_model2 <- rma(yi = cr, sei = se, data=pm_ast_cr2)

#' add final beta and se to the pooled CR dataset
pooled_crs[nrow(pooled_crs)+1,1] <- "pm"
pooled_crs[nrow(pooled_crs),2] <- "d24h_mean"
pooled_crs[nrow(pooled_crs),3] <- "minor_ast"
pooled_crs[nrow(pooled_crs),4] <- 2
pooled_crs[nrow(pooled_crs),5] <- rma_model2$beta[,1]
pooled_crs[nrow(pooled_crs),6] <- rma_model2$se
pooled_crs[nrow(pooled_crs),7] <- rma_model2$I2
pooled_crs[nrow(pooled_crs),8] <- rma_model2$QE
pooled_crs[nrow(pooled_crs),9] <- rma_model2$QEp

save(pooled_crs, file="./Data/Pooled CRs.RData")
write.csv(pooled_crs, file="./Data/Pooled CRs.csv",
          row.names = F)
  
#' Clean up environment
rm(ast_outcomes1, ast_outcomes2, i, j, k, metrics, outcomes, pols,
   rma_model, rma_model1, rma_model2, cr_df, df, full_cr, met_df,
   o3_cr, pm_ast_cr1, pm_ast_cr2, pm_cr, pooled_crs)  
  
  





