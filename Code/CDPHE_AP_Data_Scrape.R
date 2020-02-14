#' -----------------------------------------------------------------------------
#' Date created: February 10, 2020
#' Author: Sheena Martenies
#' Contact: Sheena.Martenies@colostate.edu
#' 
#' Description: Scrape air pollution and MET data from the CDPHE website
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

#' Scrape data from CDPHE website
if(!dir.exists(here::here("Data/Temp"))) dir.create(here::here("Data/Temp"))

for (pol in 1:length(pc_list)) {
  output <- data.frame()
  
  for (i in 1:length(dates)) {
    m <- substr(dates[i],1,2)
    d <- substr(dates[i],3,4)
    y <- substr(dates[i],5,8)
    
    url <- paste("https://www.colorado.gov/airquality/param_summary.aspx?",
                 "parametercode=", pc_list[pol], "&seeddate=", m, "%2f", d, "%2f", y,
                 "&export=False", sep="")
    
    #' orginal HTML includes breaks that are not preserved by html_table
    #' need to download the data, substitute the breaks, and then get the data
    #' See: https://stackoverflow.com/questions/30989543/r-scraping-an-html-
    #' table-with-rvest-when-there-are-missing-tr-tags
    
    download.file(url, destfile = here::here("Data/Temp", "cdphe_temp.html"))
    ap_html <- readChar(here::here("Data/Temp", "cdphe_temp.html"), 
                        file.info(here::here("Data/Temp", "cdphe_temp.html"))$size)
    ap_html <- gsub("<br />", "_", ap_html)
    ap_data <- read_html(ap_html)
    
    nodes <- html_nodes(ap_data, xpath = "//table")
    table <- html_table(nodes)[[3]]
    
    #' Clean up the table
    colnames(table) <- table[1,] #' column names are in first and last rows
    
    if(pc_names[i] %in% c("88101", "44201", "42101")) {
      colnames(table)[2] <- "metric_key"
    }
    table <- table[-c(1, nrow(table)-1, nrow(table)),] #' drop unnecessary rows
    
    # colnames(table) <- gsub("[^[:alnum:] ]", "", colnames(table))
    colnames(table) <- gsub("\\*", "", colnames(table))
    colnames(table) <- gsub("\\**", "", colnames(table))
    
    table$Date_Local <- dates[i]
    table$Parameter_Code <- pc_list[pol]
    
    #' append to data frame
    output <- bind_rows(output, table)
    
    print(dates[i])
    rm(table)
  }
  colnames(output) <- gsub("\\*\\*", "", colnames(output))
  colnames(output)[1] <- c("hour_MST")
  
  output_name <- paste0(pc_names[pol], "_CDPHE.csv")
  write_csv(output, here::here("Data/CDPHE_AQS_Data", output_name))
}

#' -----------------------------------------------------------------------------
#' Monitor data from the CDPHE website
#' -----------------------------------------------------------------------------

monitor_url <- "https://www.colorado.gov/airquality/site_description.aspx"

monitors <- read_html(monitor_url)
monitor_data_html <- html_nodes(monitors, "p")
monitor_data_text <- as.data.frame(html_text(monitor_data_html))

monitor_data_text[,1] <- as.character(monitor_data_text[,1])
colnames(monitor_data_text) <- "monitor_info"

#' Extract monitor_id, lon, and lat

monitor_info <- monitor_data_text %>% 
  rowwise %>% 
  mutate(monitor_loc = str_extract_all(monitor_info, "\\([^()]+\\)")[[1]][1]) %>% 
  mutate(monitor_loc = gsub("\\(", "", monitor_loc)) %>% 
  mutate(monitor_loc = gsub("\\)", "", monitor_loc)) %>% 
  mutate(monitor_id = str_sub(str_split_fixed(monitor_info, "AQS ID: ", 2)[2],
                              start = 1, end = 9)) %>% 
  mutate(lon = as.numeric(str_sub(str_split_fixed(monitor_info, "Longitude: ", 2)[2],
                                  start = 1, end = 11))) %>% 
  mutate(lat = as.numeric(str_sub(str_split_fixed(monitor_info, "Latitude: ", 2)[2],
                                  start = 1, end = 9)))

monitor_file_name <- "CDPHE_Monitors.csv"
write_csv(monitor_info, here::here("Data/CDPHE_AQS_Data", monitor_file_name))

