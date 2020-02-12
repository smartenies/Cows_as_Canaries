#' -----------------------------------------------------------------------------
#' Date created: February 11, 2020
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: Summarize daily metrics for PM2.5, ozone, and CO
#' ----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(rvest)
library(Hmisc)

start_date <- as.Date("2019-01-01")
end_date <- Sys.Date()
dates <- seq(start_date, end_date, by="1 day")
dates <- format(dates, "%m%d%Y")

#' Parameter codes
pc_list <- c(88101, 44201, 42101)
pc_names <- c("88101", "44201", "42101") #pm2.5, ozone, CO

#' -----------------------------------------------------------------------------
#' Daily mean and daily 1-hr max PM2.5
#' -----------------------------------------------------------------------------

pol <- "88101"

cdphe_data <- read_csv(here::here("Data/CDPHE_AQS_Data", "88101_CDPHE.csv")) %>% 
  gather(key = "monitor_loc", value = "metrics", 
         -c(Parameter_Code, Date_Local, hour_MST, metric_key)) %>% 
  mutate(Date_Local = str_pad(Date_Local, width = 8, pad = "0", side = "left")) %>% 
  mutate(Date_Local = as.Date(Date_Local, format = "%m%d%Y")) %>% 
  rowwise() %>% 
  mutate(metrics = as.list(str_split(metrics, "_"))) %>% 
  mutate(one_hr_conc = as.numeric(unlist(metrics)[1])) %>% 
  mutate(one_hr_conc = ifelse(is.nan(one_hr_conc), NA, one_hr_conc))

#' summarize to daily means
#' Assign a POC code of 1
cdphe_means <- cdphe_data %>% 
  select(-c(hour_MST, metrics, metric_key)) %>% 
  group_by(monitor_loc, Parameter_Code, Date_Local) %>%
  dplyr::summarize(Arithmetic_Mean = mean(one_hr_conc, na.rm = T),
                   Max_Value = max(one_hr_conc, na.rm = T)) %>% 
  mutate(Units_of_Measure = "Micrograms/cubic meter (LC)",
         Arithmetic_Mean = ifelse(is.nan(Arithmetic_Mean), NA, Arithmetic_Mean),
         Max_Value = ifelse(is.infinite(Max_Value), NA, Max_Value),
         Sample_Duration = "24 HOUR",
         POC = 1) %>% 
  ungroup()

#' add monitor IDs and coordinates
monitors <- read_csv(here::here("Data/CDPHE_AQS_Data", "CDPHE_Monitors.csv")) %>% 
  rename(Longitude = lon, Latitude = lat)

cdphe_means <- left_join(cdphe_means, monitors, by = "monitor_loc")

glimpse(cdphe_means)

write_csv(cdphe_means, here::here("Data/CDPHE_AQS_Data", "88101_Daily_CDPHE.csv"))

#' -----------------------------------------------------------------------------
#' Daily mean and daily 1-hr max CO
#' -----------------------------------------------------------------------------

pol <- "42101"

cdphe_data <- read_csv(here::here("Data/CDPHE_AQS_Data", "42101_CDPHE.csv")) %>%
  gather(key = "monitor_loc", value = "metrics",
         -c(Parameter_Code, Date_Local, hour_MST, metric_key)) %>%
  mutate(Date_Local = str_pad(Date_Local, width = 8, pad = "0", side = "left")) %>%
  mutate(Date_Local = as.Date(Date_Local, format = "%m%d%Y")) %>%
  rowwise() %>%
  mutate(metrics = as.list(str_split(metrics, "_"))) %>%
  mutate(one_hr_conc = as.numeric(unlist(metrics)[1])) %>%
  mutate(one_hr_conc = ifelse(is.nan(one_hr_conc), NA, one_hr_conc))

#' summarize to daily means
#' Assign a POC code of 1
cdphe_means <- cdphe_data %>%
  select(-c(hour_MST, metrics, metric_key)) %>%
  group_by(monitor_loc, Parameter_Code, Date_Local) %>%
  dplyr::summarize(Arithmetic_Mean = mean(one_hr_conc, na.rm = T),
                   Max_Value = max(one_hr_conc, na.rm = T)) %>%
  mutate(Units_of_Measure = "Parts per million",
         Arithmetic_Mean = ifelse(is.nan(Arithmetic_Mean), NA, Arithmetic_Mean),
         Max_Value = ifelse(is.infinite(Max_Value), NA, Max_Value),
         Sample_Duration = "24 HOUR",
         POC = 1) %>%
  ungroup()

#' add monitor IDs and coordinates
monitors <- read_csv(here::here("Data/CDPHE_AQS_Data", "CDPHE_Monitors.csv")) %>%
  rename(Longitude = lon, Latitude = lat)

cdphe_means <- left_join(cdphe_means, monitors, by = "monitor_loc")

glimpse(cdphe_means)

write_csv(cdphe_means, here::here("Data/CDPHE_AQS_Data", "42101_Daily_CDPHE.csv"))

#' -----------------------------------------------------------------------------
#' Daily 8-hr max and daily 1-hr max ozone
#' -----------------------------------------------------------------------------

pol <- "44201"

cdphe_data <- read_csv(here::here("Data/CDPHE_AQS_Data", "44201_CDPHE.csv")) %>% 
  gather(key = "monitor_loc", value = "metrics", 
         -c(Parameter_Code, Date_Local, hour_MST, metric_key)) %>% 
  mutate(Date_Local = str_pad(Date_Local, width = 8, pad = "0", side = "left")) %>% 
  mutate(Date_Local = as.Date(Date_Local, format = "%m%d%Y")) %>% 
  rowwise() %>% 
  mutate(metrics = str_split(metrics, "_")) %>% 
  mutate(one_hr_conc = as.numeric(unlist(metrics)[1])) %>% 
  mutate(one_hr_conc = ifelse(is.nan(one_hr_conc), NA, one_hr_conc)) %>% 
  mutate(eight_hr_conc = as.numeric(unlist(metrics)[2])) %>% 
  mutate(eight_hr_conc = ifelse(is.nan(eight_hr_conc), NA, eight_hr_conc))

#' summarize to daily mean, max hourly, and mda8
#' If there aren't 18 8-hour averages, set to NA (based on NAAQS)
#' Assign a POC code of 1
cdphe_means <- cdphe_data %>% 
  select(-c(hour_MST, metrics, metric_key)) %>% 
  group_by(monitor_loc, Parameter_Code, Date_Local) %>%
  dplyr::summarize(Arithmetic_Mean = mean(one_hr_conc, na.rm = T),
                   Max_Hourly = max(one_hr_conc, na.rm = T),
                   Max_8Hour_Mean = ifelse(sum(!is.na(eight_hr_conc)) > 18,
                                           max(eight_hr_conc, na.rm = T),
                                           NA)) %>% 
  mutate(Units_of_Measure = "Parts per billion",
         Arithmetic_Mean = ifelse(is.nan(Arithmetic_Mean), NA, Arithmetic_Mean),
         Max_Hourly = ifelse(is.infinite(Max_Hourly), NA, Max_Hourly),
         Max_8Hour_Mean = ifelse(is.infinite(Max_8Hour_Mean), NA, Max_8Hour_Mean),
         Sample_Duration = "1 HOUR",
         POC = 1) %>% 
  ungroup()

#' add monitor IDs and coordinates
monitors <- read_csv(here::here("Data/CDPHE_AQS_Data", "CDPHE_Monitors.csv")) %>% 
  rename(Longitude = lon, Latitude = lat)

cdphe_means <- left_join(cdphe_means, monitors, by = "monitor_loc")

glimpse(cdphe_means)

write_csv(cdphe_means, here::here("Data/CDPHE_AQS_Data", "44201_Daily_CDPHE.csv"))
