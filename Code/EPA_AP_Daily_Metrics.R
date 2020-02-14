#' -----------------------------------------------------------------------------
#' Date created: February 13, 2020
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: Clean up EPA data to have daily metrics for Heather's analysis
#' -----------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(sf)

#' Coordinate reference systems 
ll_wgs84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
albers <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

aqs_path <- "R:/RSTOR-Magzamen/Research/Secondary_Data/EPA_Air_Quality_System_Data/"

#' -----------------------------------------------------------------------------
#' First, read in the daily PM2.5 and simplify the datasets
#' -----------------------------------------------------------------------------

pol <- "88101"
years <- c("2018", "2019")

file_list <- paste0("daily_", pol, "_", years, ".csv")

aqs_data <- data.frame()

for (i in 1:length(file_list)) {
  temp <- read_csv(paste0(aqs_path, file_list[i]))
  colnames(temp) <- gsub(" ", "_", colnames(temp))
  
  temp <- filter(temp, State_Code == "08") %>% 
    mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
           Site_Num = str_pad(Site_Num, width = 4, side = "left", pad = "0")) %>% 
    mutate(monitor_id = paste0(State_Code, County_Code, Site_Num)) %>% 
    rename(Max_Value = "1st_Max_Value")
  
  aqs_data <- bind_rows(aqs_data, temp)
  rm(temp)
}

head(aqs_data)
write_csv(aqs_data, here::here("Data/EPA_AQS_Data", paste0(pol, "_Daily_EPA.csv")))

#' -----------------------------------------------------------------------------
#' Next, read in the daily carbon monoxide data and simplify the datasets
#' -----------------------------------------------------------------------------

pol <- "42101"
years <- c("2018", "2019")

file_list <- paste0("daily_", pol, "_", years, ".csv")

aqs_data <- data.frame()

for (i in 1:length(file_list)) {
  temp <- read_csv(paste0(aqs_path, file_list[i]))
  colnames(temp) <- gsub(" ", "_", colnames(temp))
  
  temp <- filter(temp, State_Code == "08") %>% 
    mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
           Site_Num = str_pad(Site_Num, width = 4, side = "left", pad = "0")) %>% 
    mutate(monitor_id = paste0(State_Code, County_Code, Site_Num)) %>% 
    rename(Max_Value = "1st_Max_Value")
  
  aqs_data <- bind_rows(aqs_data, temp)
  rm(temp)
}

head(aqs_data)
write_csv(aqs_data, here::here("Data/EPA_AQS_Data", paste0(pol, "_Daily_EPA.csv")))

#' -----------------------------------------------------------------------------
#' Daily ozone (mean, max hourly)
#' -----------------------------------------------------------------------------

pol <- "44201"
years <- c("2018", "2019")

file_list <- paste0("daily_", pol, "_", years, ".csv")

aqs_data <- data.frame()

for (i in 1:length(file_list)) {
  temp <- read_csv(paste0(aqs_path, file_list[i]))
  colnames(temp) <- gsub(" ", "_", colnames(temp))
  
  temp <- filter(temp, State_Code == "08") %>% 
    mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
           Site_Num = str_pad(Site_Num, width = 4, side = "left", pad = "0")) %>% 
    mutate(monitor_id = paste0(State_Code, County_Code, Site_Num)) %>% 
    rename(Max_Value = "1st_Max_Value")
  
  aqs_data <- bind_rows(aqs_data, temp)
  rm(temp)
}

head(aqs_data)
write_csv(aqs_data, here::here("Data/EPA_AQS_Data", paste0(pol, "_Daily_EPA.csv")))

#' -----------------------------------------------------------------------------
#' Daily 8h max ozone 
#' -----------------------------------------------------------------------------

pol <- "44201"
years <- c("2018", "2019")

file_list <- paste0("8hour_", pol, "_", years, ".csv")

aqs_data <- data.frame()

for (i in 1:length(file_list)) {
  temp <- read_csv(paste0(aqs_path, file_list[i]))
  colnames(temp) <- gsub(" ", "_", colnames(temp))
  
  temp <- filter(temp, State_Code == "08") %>% 
    mutate(County_Code = str_pad(County_Code, width = 3, side = "left", pad = "0"),
           Site_Num = str_pad(Site_Num, width = 4, side = "left", pad = "0")) %>% 
    mutate(monitor_id = paste0(State_Code, County_Code, Site_Num))
  
  aqs_data <- bind_rows(aqs_data, temp)
  rm(temp)
}

#' ID daily max 8-hour mean for the analysis
vars <- select(aqs_data, -c(Time_Local, Date_GMT, Time_GMT, Date_of_Last_Change,
                            Observation_Count:Mean_Excluding_Concurred_Flags)) %>% 
  distinct()

daily_max <- select(aqs_data, Date_Local, monitor_id, POC,
                    Mean_Including_All_Data:Mean_Excluding_Concurred_Flags) %>% 
  group_by(monitor_id, Date_Local, POC) %>% 
  summarize(Mean_Including_All_Data = max(Mean_Including_All_Data, na.rm = T),
            Mean_Excluding_All_Flagged_Data = max(Mean_Excluding_All_Flagged_Data, na.rm = T),
            Mean_Excluding_Concurred_Flags = max(Mean_Excluding_Concurred_Flags, na.rm = T)) %>% 
  mutate(Max_8Hour_Mean = Mean_Excluding_All_Flagged_Data) %>% 
  ungroup()

daily_max2 <- left_join(daily_max, vars, by = c("monitor_id", "Date_Local"))

head(daily_max2)
write_csv(daily_max2, here::here("Data/EPA_AQS_Data", paste0(pol, "_MDA8_EPA.csv")))
