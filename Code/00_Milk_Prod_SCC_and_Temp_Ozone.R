#' Code for analyzing the associations between milk production or somatic cell
#' count and temperature among dairy cattle in Fort Collins


#' Load required libraries
library(foreign)
library(stats)
library(splines)
library(mgcv)
library(tidyverse)
library(readxl)
library(writexl)
library(ggplot2)
library(ggcorrplot)
library(ggthemes)
library(nlme)

#' For ggplots
simple_theme <- theme(
  #aspect.ratio = 1,
  text  = element_text(family="Calibri",size = 12, color = 'black'),
  panel.spacing.y = unit(0,"cm"),
  panel.spacing.x = unit(0.25, "lines"),
  panel.grid.minor = element_line(color = "transparent"),
  panel.grid.major = element_line(color = "transparent"),
  panel.border=element_rect(fill = NA),
  panel.background=element_blank(),
  axis.ticks = element_line(colour = "black"),
  axis.text = element_text(color = "black", size=10),
  # legend.position = c(0.1,0.1),
  plot.margin=grid::unit(c(0,0,0,0), "mm"),
  legend.key = element_blank()
)
windowsFonts(Calibri=windowsFont("TT Calibri"))
options(scipen = 9999) #avoid scientific notation


#' -----------------------------------------------------------------------------
#' Load production data, and merge with SCC data
#' -----------------------------------------------------------------------------
dairy_data <- read_csv(here::here("Data", "052019_to_062019_Production_FC_Data.csv"))
glimpse(dairy_data)

scc_data <- read_csv(here::here("Data", "05&062019_FCSCC.csv"))
glimpse(scc_data)

dairy_data <- left_join(dairy_data, scc_data, by = "DATE") %>% 
  mutate(DATE = as.Date(DATE, format = "%m/%d/%Y"))

write_csv(dairy_data, here::here("Data", "Dairy_Data_Full.csv"))

head(dairy_data)
summary(dairy_data)

#' -----------------------------------------------------------------------------
#' Examining each variable
#' Histograms and time series plots
#' Correlations between each variable
#' Check autocorrelation
#' -----------------------------------------------------------------------------

#' Average milk production
ggplot(dairy_data) +
  geom_histogram(aes(x = Avgmilk)) +
  simple_theme

ggplot(dairy_data) +
  geom_point(aes(x = DATE, y = Avgmilk)) +
  simple_theme

acf(dairy_data$Avgmilk, plot = T)
pacf(dairy_data$Avgmilk, plot = T)

#' Somatic cell counts
ggplot(dairy_data) +
  geom_histogram(aes(x = SCC_avg)) +
  simple_theme

ggplot(dairy_data) +
  geom_point(aes(x = DATE, y = SCC_avg)) +
  simple_theme

acf(dairy_data$SCC_avg, plot = T)
pacf(dairy_data$SCC_avg, plot = T)

#' Temperature
ggplot(dairy_data) +
  geom_histogram(aes(x = THI)) +
  simple_theme

ggplot(dairy_data) +
  geom_point(aes(x = DATE, y = THI)) +
  simple_theme

acf(dairy_data$THI, plot = T, na.action = na.pass)
pacf(dairy_data$THI, plot = T, na.action = na.pass)

#' Ozone
ggplot(dairy_data) +
  geom_histogram(aes(x = FOOTHILLS_DAILYMAXO3)) +
  simple_theme

ggplot(dairy_data) +
  geom_point(aes(x = DATE, y = FOOTHILLS_DAILYMAXO3)) +
  simple_theme

acf(dairy_data$FOOTHILLS_DAILYMAXO3, plot = T, na.action = na.pass)
pacf(dairy_data$FOOTHILLS_DAILYMAXO3, plot = T, na.action = na.pass)

#' 24 h mean PM
ggplot(dairy_data) +
  geom_histogram(aes(x = PM2.5_avg)) +
  simple_theme

ggplot(dairy_data) +
  geom_point(aes(x = DATE, y = PM2.5_avg)) +
  simple_theme

acf(dairy_data$PM2.5_avg, plot = T, na.action = na.pass)
pacf(dairy_data$PM2.5_avg, plot = T, na.action = na.pass)

#' 1 hour max PM2.5
ggplot(dairy_data) +
  geom_histogram(aes(x = PM2.5_max)) +
  simple_theme

ggplot(dairy_data) +
  geom_point(aes(x = DATE, y = PM2.5_max)) +
  simple_theme

acf(dairy_data$PM2.5_max, plot = T, na.action = na.pass)
pacf(dairy_data$PM2.5_max, plot = T, na.action = na.pass)

#' Correlation coefficients between the variables
#' PM2.5 mean and max are very highly correlated
var_cor <- select(dairy_data, -DATE) %>% 
  na.omit()
ggcorrplot(round(cor(var_cor), 1), outline.col = "white",
           lab = T, colors = c("#6D9EC1", "white", "#E46726"))
ggsave(here::here("Figs", "Correlation_Plot.jpeg"),
       heigh = 5, width = 5, dpi = 750, units = "in", device = "jpeg")

#' -----------------------------------------------------------------------------
#' First, linear regression models for milk production
#' -----------------------------------------------------------------------------

#' Just the date
prod_lm_0 <- lm(Avgmilk ~ DATE, data = dairy_data)
summary(prod_lm_0)
par(mfrow = c(2,2))
plot(prod_lm_0)
par(mfrow = c(1,1))

#' Add temperature
prod_lm_1 <- lm(Avgmilk ~ THI, data = dairy_data)
summary(prod_lm_1)
par(mfrow = c(2,2))
plot(prod_lm_1)
par(mfrow = c(1,1))

#' Add ozone
prod_lm_2 <- lm(Avgmilk ~ THI + FOOTHILLS_DAILYMAXO3, data = dairy_data)
summary(prod_lm_2)
par(mfrow = c(2,2))
plot(prod_lm_2)
par(mfrow = c(1,1))

#' Add daily PM2.5 average
prod_lm_3 <- lm(Avgmilk ~ THI + FOOTHILLS_DAILYMAXO3 + PM2.5_avg, 
              data = dairy_data)
summary(prod_lm_3)
par(mfrow = c(2,2))
plot(prod_lm_3)
par(mfrow = c(1,1))

#' Swap for daily PM max
prod_lm_4 <- lm(Avgmilk ~ THI + FOOTHILLS_DAILYMAXO3 + PM2.5_max, 
              data = dairy_data)
summary(prod_lm_4)
par(mfrow = c(2,2))
plot(prod_lm_4)
par(mfrow = c(1,1))

#' -----------------------------------------------------------------------------
#' Model average milk production using a generalized least squares (gls) model
#' with an autoregressive (AR1) correlation structure
#' -----------------------------------------------------------------------------

#' Need complete dataset
dairy_data2 <- na.omit(dairy_data)
nrow(dairy_data2)
summary(dairy_data2)

#' Just the time trend
prod_0 <- gls(Avgmilk ~ DATE, data = dairy_data2,
               correlation = corAR1())
summary(prod_0)
plot(prod_0)

#' Add temperature
prod_1 <- gls(Avgmilk ~ DATE + THI, data = dairy_data2,
              correlation = corAR1())
summary(prod_1)
plot(prod_1)

#' Add ozone
prod_2 <- gls(Avgmilk ~ DATE + THI + FOOTHILLS_DAILYMAXO3, data = dairy_data2,
              correlation = corAR1())
summary(prod_2)
plot(prod_2)

#' Add daily PM2.5 average
prod_3 <- gls(Avgmilk ~ DATE + THI + FOOTHILLS_DAILYMAXO3 + PM2.5_avg, 
              data = dairy_data2,
              correlation = corAR1())
summary(prod_3)
plot(prod_3)

#' Swap for daily PM max
prod_4 <- gls(Avgmilk ~ DATE + THI + FOOTHILLS_DAILYMAXO3 + PM2.5_max, 
              data = dairy_data2,
              correlation = corAR1())
summary(prod_4)
plot(prod_4)

#' -----------------------------------------------------------------------------
#' Second, linear regression models for somatic cell count
#' -----------------------------------------------------------------------------

#' Just the date
scc_lm_0 <- lm(SCC_avg ~ DATE, data = dairy_data)
summary(scc_lm_0)
par(mfrow = c(2,2))
plot(scc_lm_0)
par(mfrow = c(1,1))

#' Add temperature
scc_lm_1 <- lm(SCC_avg ~ THI, data = dairy_data)
summary(scc_lm_1)
par(mfrow = c(2,2))
plot(scc_lm_1)
par(mfrow = c(1,1))

#' Add ozone
scc_lm_2 <- lm(SCC_avg ~ THI + FOOTHILLS_DAILYMAXO3, data = dairy_data)
summary(scc_lm_2)
par(mfrow = c(2,2))
plot(scc_lm_2)
par(mfrow = c(1,1))

#' Add daily PM2.5 average
scc_lm_3 <- lm(SCC_avg ~ THI + FOOTHILLS_DAILYMAXO3 + PM2.5_avg, 
                data = dairy_data)
summary(scc_lm_3)
par(mfrow = c(2,2))
plot(scc_lm_3)
par(mfrow = c(1,1))

#' Swap for daily PM max
scc_lm_4 <- lm(SCC_avg ~ THI + FOOTHILLS_DAILYMAXO3 + PM2.5_max, 
                data = dairy_data)
summary(scc_lm_4)
par(mfrow = c(2,2))
plot(scc_lm_4)
par(mfrow = c(1,1))

#' -----------------------------------------------------------------------------
#' Model somatic cell counts using a generalized least squares (gls) model
#' with a autoregressive (AR) correlation structure
#' Note: histogram of SCC looks vaguely normal... do these need to be modeled
#' using a Poisson distribution (i.e., are the counts?). 
#' If so, I can rerun these models
#' -----------------------------------------------------------------------------

#' Just the time trend
scc_0 <- gls(SCC_avg ~ DATE, data = dairy_data2,
              correlation = corAR1())
summary(scc_0)
plot(scc_0)

#' Add temperature
scc_1 <- gls(SCC_avg ~ DATE + THI, data = dairy_data2,
              correlation = corAR1())
summary(scc_1)
plot(scc_1)

#' Add ozone
scc_2 <- gls(SCC_avg ~ DATE + THI + FOOTHILLS_DAILYMAXO3, data = dairy_data2,
              correlation = corAR1())
summary(scc_2)
plot(scc_2)

#' Add daily PM2.5 average
scc_3 <- gls(SCC_avg ~ DATE + THI + FOOTHILLS_DAILYMAXO3 + PM2.5_avg, 
             data = dairy_data2,
             correlation = corAR1())
summary(scc_3)
plot(scc_3)

#' Swap for daily PM max
scc_4 <- gls(SCC_avg ~ DATE + THI + FOOTHILLS_DAILYMAXO3 + PM2.5_max, 
             data = dairy_data2,
             correlation = corAR1())
summary(scc_4)
plot(scc_4)












