#' -----------------------------------------------------------------------------
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
#' This script scrapes the EPA and CDPHE websites for air monitor data
#' -----------------------------------------------------------------------------

library(ggplot2)
library(reshape2)
library(plyr)
library(XLConnect)

#' For ggplots
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=10),
  #legend.position = c(0.75,0.3),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation

#' -----------------------------------------------------------------------------
#' Scraping US EPA AQS PM2.5 and ozone data for the Southern Front Range area
#' Based on code originally written by C. Milando (UMich)
#' -----------------------------------------------------------------------------
library(Hmisc)
library(stringr)

years <- c(2014)
state <- "08" #Colorado
all_counties <- c("041", "043", "101") #"El Paso, Fremont, and Pueblo

#' Criteria pollutants and carbon parameters
#' https://aqs.epa.gov/aqsweb/documents/codetables/methods_all.html
params <- c("88101", "44201") #PM2.5-LOCAL CONDITIONS, OZONE

output <- data.frame()

#' might not work the first time. if not just try typing in the link first yourself
for(county in all_counties) {
  for(param in params) {
    for(year in years) {
      
      bdate <- paste0(year,sprintf("%02i",1),"01")
      edate <- paste0(year,sprintf("%02i",12),31)
      
      prefix <- "https://aqs.epa.gov/api/rawData?user=sheena.martenies@colostate.edu&pw=khakifrog54&format=DMCSV&param="
      aqs_link <- paste(prefix,param,
                        "&bdate=",bdate,"&edate=",edate,"&state=",state,
                        "&county=",county,collapse = "",sep="")
      error_catch <- F; warn_catch <- F
      tryCatch(read.csv(aqs_link),error = function(e) error_catch <- T, 
               warning = function(w) warn_catch <- T)
      if(!error_catch) {
        aqs_data <- read.csv(aqs_link)[-1,]
        if(length(which(aqs_data$Latitude == "END OF FILE")) > 0) {
          aqs_data <- aqs_data[-which(aqs_data$Latitude == "END OF FILE"),]
        }
        
        if(nrow(aqs_data) > 0) {
          if(nrow(output) > 0) {
            output <- rbind(output,aqs_data)
          }
          else {
            output <- aqs_data
          }
          rm(aqs_data)
        }
      }
      
      cat("param = ",param,year,"; error?",error_catch,
          "; warn?", warn_catch,"\n")
    }
  }
}


output$datetime <- as.POSIXct(paste(output$Date.Local, output$X24.Hour.Local), 
                              format="%Y-%m-%d %H",tz = "GMT")

save(output, file = "./Data/AQS Data/AQS Data 2014.RData")
write.csv(output, file = "./Data/AQS Data/AQS Data 2014.csv",
          row.names = F)

#' -----------------------------------------------------------------------------
#' Scraping PM2.5 and ozone data from the CDPHE website. These data are reported
#' as hourly concentrations and arebNOT corrected or validated. Compare to US 
#' EPA data that have been validated
#' -----------------------------------------------------------------------------

library(rvest)
library(XML)
library(htmltab)
library(tidyr)
library(dplyr)

start_date <- as.Date("2014-01-01")
# end_date <- Sys.Date()
end_date <- as.Date("2014-12-31")
dates <- seq(start_date, end_date, by="1 day")
dates <- format(dates, "%m%d%Y")

params <- c("88101", "44201")
outputs <- list()

for (j in 1:length(params)){
  for (i in 1:length(dates)) {
    m <- substr(dates[i],1,2)
    d <- substr(dates[i],3,4)
    y <- substr(dates[i],5,8)
    
    url <- paste("https://www.colorado.gov/airquality/param_summary.aspx?",
                 "parametercode=",params[j],"&seeddate=", m, "%2f", d, "%2f", y,
                 "&export=False", sep="")
    
    cdphe <- read_html(url)
    #nodes <- html_nodes(cdphe, xpath = "//table")
    
    #' orginal HTML includes breaks that are not preserved by html_table
    #' need to download the data, substitute the breaks, and then get the data
    #' See: https://stackoverflow.com/questions/30989543/r-scraping-an-html-
    #' table-with-rvest-when-there-are-missing-tr-tags
    
    download.file(url, destfile = "./Temp/temp.html")
    ap_html <- readChar("./Temp/temp.html", file.info("./Temp/temp.html")$size)
    ap_html <- gsub("<br />", "-", ap_html)
    ap_data <- read_html(ap_html)
    
    nodes <- html_nodes(ap_data, xpath = "//table")
    table <- html_table(nodes)[[3]]
    
    #' Clean up the table
    colnames(table) <- table[1,] #' column names are in first and last rows
    table <- table[-c(1, nrow(table)-1, nrow(table)),] #' drop unnecessary rows
    table$date <- dates[i]
    table <- table[,c(ncol(table),1:ncol(table)-1)]
    
    #' append to data frame
    if (exists("output_temp")==F) {
      output_temp <- table
    } else {
      output_temp <- bind_rows(output_temp, table)
    }

    print(dates[i])
    rm(table)
    if (file.exists("./Temp/temp.html")) file.remove("./Temp/temp.html")
  }
  outputs[[j]] <- output_temp
  rm(output_temp)
}

save(outputs, file="./Data/AQS Data/CDPHE Monitors.RData")

#' colnames(output) <- gsub("\\*\\*", "", colnames(output))
#' colnames(output)[2:3] <- c("hour", "key")
#' output$dt <- as.POSIXct(paste(output$date, output$hour), 
#'                         format="%m%d%Y %I:%M %p") 
#' output <- output[,c(ncol(output),1:ncol(output)-1)]
#' row.names(output) <- 1:nrow(output)
#' 
#' write.table(output, file=paste("./Data/AQS Data/CDPHE ", as.character(start_date),
#'                                "_", as.character(end_date), ".txt", sep=""),
#'             row.names = F, sep=",")
#' rm(output)
#' 
#' #' Get data for specific monitors
#' cdphe <- read.table(file=paste("./Data/AQS Data/CDPHE ", as.character(start_date),
#'                                "_", as.character(end_date), ".txt", sep=""),
#'                     header=T, sep=",")
#' 
#' dt_cols <- colnames(cdphe)[1:4]

# monitors <- colnames(cdphe)[-c(1:4)]
# monitors <- c("FTCF", "GREH") #CSU Facilties and Greely Hospital
# 
# for (i in 1:length(monitors)) {
#   df <- cdphe[,c(dt_cols, monitors[i])]
#   key <- unlist(strsplit(as.character(unique(df$key)), "-"))
#   split <- strsplit(as.character(df[,monitors[i]]), "-", 3)
#   
#   temp <- as.data.frame(do.call("rbind", split))
#   
#   df <- cbind(df, temp)
#   colnames(df)[c((ncol(df)-2):ncol(df))] <- key
#   rm(temp)
#   
#   write.table(df, file=paste("./Data/Air Quality/", monitors[i], "_",
#                              as.character(start_date), "_", as.character(today),
#                              ".txt", sep=""),
#               row.names = F, sep=",")
#   print(monitors[i])
# }
