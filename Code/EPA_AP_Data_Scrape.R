#' -----------------------------------------------------------------------------
#' Date created: February 13, 2020
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: Scrape air pollution and MET data from the EPA AQS website
#' https://aqs.epa.gov/api
#' Username: sheena.martenies@colostate.edu
#' Password: khakifrog54
#' 
#' NOTE: This can sometimes take a while, depending on the number of monitors,
#' pollutants, and years. I often just let this run after work and come back 
#' to it in the morning
#' 
#' NOTE: Due to some issues with the API, going to just download the summary
#' files from the EPA website and subset to the locations we need in the 
#' cleaning script
#' ----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(rvest)
library(Hmisc)

years <- c(2018:2019)
met_vars <- c("WIND", "PRESS", "TEMP", "RH_DP")

aqs_path <- "R:/RSTOR-Magzamen/Research/Secondary_Data/EPA_Air_Quality_System_Data"

for (i in 1:length(years)) {
  
  #' Daily PM2.5
  aqs_url <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_88101_", 
                    years[i], ".zip")
  
  download.file(aqs_url, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = aqs_path)
  
  #' Daily met variables
  for (j in 1:length(met_vars)) {
    met_url <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_", met_vars[j],  
                      "_", years[i], ".zip")
    
    #' Download zipfile from the EPA website and unzip
    download.file(met_url, destfile = here::here("Data/Temp", "temp.zip"))
    unzip(here::here("Data/Temp", "temp.zip"), exdir = aqs_path)
  }
  
  #' daily mean ozone data
  aqs_url2 <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_44201_", 
                     years[i], ".zip")
  
  download.file(aqs_url2, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = aqs_path)
  
  #' daily 8h max ozone data
  aqs_url3 <- paste0("https://aqs.epa.gov/aqsweb/airdata/8hour_44201_", 
                     years[i], ".zip")
  
  download.file(aqs_url3, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = aqs_path)
  
  #' PM2.5 speciation data
  aqs_url4 <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_SPEC_", 
                     years[i], ".zip")
  
  download.file(aqs_url4, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = aqs_path)
  
  #' CO Data
  aqs_url5 <- paste0("https://aqs.epa.gov/aqsweb/airdata/daily_42101_", 
                     years[i], ".zip")
  
  download.file(aqs_url5, destfile = here::here("Data/Temp", "temp.zip"))
  unzip(here::here("Data/Temp", "temp.zip"), exdir = aqs_path)
}

