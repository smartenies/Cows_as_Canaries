#' Daily RH data for Heather


library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)

df <- read_csv(here::here("Data", "NOAA Greeley Airport Data.csv"))

names(df)

df2 <- select(df, DATE, contains("RelativeHumidity"))
names(df2)
glimpse(df2)

df_sum <- df2 %>% 
  mutate(date = format(as.POSIXct(DATE,format='%Y-%m-%d %H:%M:%S'), format='%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarize(daily_mean = mean(HourlyRelativeHumidity, na.rm = T),
            daily_max = max(HourlyRelativeHumidity, na.rm = T))

test <- slice(df2, 1:32)

glimpse(df_sum)

write_csv(df_sum, here::here("Data", "RH_Data_Summarized.csv"))
