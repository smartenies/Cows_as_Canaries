library(foreign)
library(stats)
library(splines)
library(epicalc)
library(mgcv)

setwd("~/Google Drive/EHS 675/Labs/Lab 14")

## Load air pollution time-series data
## Daily cardiovascular deaths in people aged 75+ in Los Angeles, 1987-2000

load("CVDdaily.rda")
dim(CVDdaily)
names(CVDdaily)

## Data structure
CVDdaily[1:20,]
CVDdaily[60:80,]

### convert character to date
class(CVDdaily$date)
CVDdaily$date1<-as.Date(CVDdaily$date)
class(CVDdaily$date1)

CVDdaily[1:20,]
## date and date1 look the same but their characteristics differ

date2<-CVDdaily$date+365
date3<-CVDdaily$date1+365

date3[1:20]

### if there is no date variable but a sequence of days
### let's create a date variable
CVDdaily$seq[1:20]
date4<-as.Date("1986-12-31")
date4

CVDdaily$date2<-CVDdaily$seq+date4
CVDdaily[1:20,]
class(CVDdaily$date)
class(CVDdaily$date1)
class(CVDdaily$date2)

### Default date format is ISO (yyyy-mm-dd)
### The American format of 'month, day, year'
format(date4, "%b %d, %y")
format(date4, "%m/%d/%Y")
format(date4, "%m-%d-%y")

summ(CVDdaily)
dim(CVDdaily)

attach(CVDdaily)

## Plot daily CVD
plot(cvd~date,xlab="Date",ylab="Number of CVD deaths")	#error
plot(cvd~date1,xlab="Date",ylab="Number of CVD deaths", pch=".")
lines(lowess(cvd~date1, f=0.9), col=2, lwd=2)
lines(lowess(cvd~date1, f=0.5), col=3, lwd=2)
lines(lowess(cvd~date1, f=0.2), col=4, lwd=2)
lines(lowess(cvd~date1, f=0.1), col=5, lwd=2)
lines(lowess(cvd~date1, f=0.01), col=6, lwd=2)
    #' reducing the span helps clarify the pattern

plot(tmpd~date1, xlab="Date",ylab="Temperature (F)")
lines(lowess(tmpd~date1, f=0.01), col=2, lwd=2)

plot(o3mean~date1, xlab="Date",ylab="Daily mean ozone (ppb)")
lines(lowess(o3mean~date1, f=0.01), col=2, lwd=2)

##Check autocorrelated data
acf(cvd,plot=TRUE,lag.max=5000)
acf(tmpd,plot=TRUE,lag.max=5000)
acf(o3mean,plot=T,lag.max=5000)

#' partial autocorrelation elimiated correlation 
#' for the days between the start and the day of interest
#' e.g., removes days 1 and 2 when looking at day 0 and day 3
pacf(cvd, plot=T)	##default lag.max=33
  #' after about 10 days, there isn't much partial autocorrelation
  #' 0-10 days is the time window we're interested in
pacf(cvd, plot=T, lag.max=5000)

pacf(tmpd, plot=T)
pacf(tmpd, plot=T, lag.max=5000)

pacf(o3mean, plot=T)
pacf(o3mean, plot=T, lag.max=5000)

##Add lags and moving averages (by shifting one observation down) 
dim(CVDdaily)
CVDdaily$tmpd1<-c(NA, tmpd[1:5113])
CVDdaily$tmpd2<-c(NA, NA, tmpd[1:5112])
CVDdaily$tmpd01<-(CVDdaily$tmpd+CVDdaily$tmpd1)/2
CVDdaily$tmpd02<-(CVDdaily$tmpd+CVDdaily$tmpd1+CVDdaily$tmpd2)/3

CVDdaily$o3mean1<-c(NA, o3mean[1:5113])
CVDdaily$o3mean2<-c(NA, NA, o3mean[1:5112])
CVDdaily$o3mean01<-(CVDdaily$o3mean+CVDdaily$o3mean1)/2
CVDdaily$o3mean02<-(CVDdaily$o3mean+CVDdaily$o3mean1+CVDdaily$o3mean2)/3

CVDdaily[1:10, c("tmpd","tmpd1","tmpd2","tmpd01","tmpd02","o3mean","o3mean1","o3mean2","o3mean01","o3mean02")]

detach(CVDdaily)
attach(CVDdaily)

##Check if the mean is equal to the variance (Poisson)
hist(cvd)
summ(cvd)
mean(cvd)
var(cvd)
### MEAN IS NOT EQUAL TO THE VARIANCE
### USE QUASI-LIKELIHOOD POISSON RATHER THAN POISSON

### Trend and season
mod0<-glm(cvd~date1, family=quasipoisson, data=CVDdaily)
summary.glm(mod0)
## there is a long-term trend

## check pacf of residuals
#' Are the residuals independent, or are the correlated?
pacf(residuals(mod0,type="deviance"),plot=T)
  #' correlated up to about day 10
  #' Cannot assume observations are independent!
  #' need to filter out high partial correlation

## Add season indicators
mod0.season<-glm(cvd~date1+winter+spring+summer, family=quasipoisson, data=CVDdaily)
summary.glm(mod0.season)
### there are seasonal variations

pacf(residuals(mod0.season,type="deviance"),plot=T)
  #' still see high partial autocorrelation
  #' partial ACF is lower after adding seasonal indicators

# par(mfrow=c(2,2))
# par(mfrow=c(1,1))
pacf(residuals(mod0,type="deviance"),plot=T)
pacf(residuals(mod0.season,type="deviance"),plot=T)

### capture seasonal variation using spline (try 4df per yr*14 yrs = 56df)
names(CVDdaily)

mod1<-gam(cvd~s(date1, k=4*14), family=quasipoisson, data=CVDdaily)
## error: cannot use a class=date variable
#' need to use sequence instead!
mod1<-gam(cvd~s(seq, k=4*14), family=quasipoisson, data=CVDdaily)
summary(mod1)
plot(mod1)

pacf(residuals(mod1,type="deviance"),plot=T)
  #' still some autocorrelation
  #' highest PACF has come down, but still there

### Weather and day of week
par(mfrow=c(1,1))
boxplot(split(cvd,dow), notch=T)
plot(cvd~tmpd)
lines(lowess(cvd~tmpd), col=2, lwd=2)

mod2<-gam(cvd~s(seq, k=56)+dow, family=quasipoisson, data=CVDdaily)
summary(mod2)
plot(mod2)
pacf(residuals(mod2,type="deviance"),plot=T)
  #' doesn't change too much

anova(mod1, mod2, test="Chisq")
  #' adding DOW is better

mod3<-gam(cvd~s(seq, k=56)+dow+s(tmpd), family=quasipoisson, data=CVDdaily)
summary(mod3)
plot(mod3)
pacf(residuals(mod3,type="deviance"),plot=T)
  #' still haven't reduced partial autocorrelation

anova(mod2, mod3, test="Chisq")
  #' including temperature is better overall

## which lag/ma for temperature is best?
mod3.1<-gam(cvd~s(seq, k=56)+dow+s(tmpd1), family=quasipoisson, data=CVDdaily)
summary(mod3.1)
mod3.2<-gam(cvd~s(seq, k=56)+dow+s(tmpd2), family=quasipoisson, data=CVDdaily)
summary(mod3.2)
mod3.01<-gam(cvd~s(seq, k=56)+dow+s(tmpd01), family=quasipoisson, data=CVDdaily)
summary(mod3.01)
mod3.02<-gam(cvd~s(seq, k=56)+dow+s(tmpd02), family=quasipoisson, data=CVDdaily)
summary(mod3.02)

## MA 0-2 has the largest adj R2 and deviance explained
plot(mod3.02)
pacf(residuals(mod3.02,type="deviance"),plot=T)
  #' still have autocorrelation

### try 6df, 8df and 10df per yr and fixed knots (fx=T)
mod4<-gam(cvd~s(seq, k=14*6, fx=T)+dow+s(tmpd02), family=quasipoisson, data=CVDdaily)
summary(mod4)
pacf(residuals(mod4,type="deviance"),plot=T)

mod5<-gam(cvd~s(seq, k=14*8, fx=T)+dow+s(tmpd02), family=quasipoisson, data=CVDdaily)
summary(mod5)
pacf(residuals(mod5,type="deviance"),plot=T)
  #' gives us the best partial autcocorrelation plot!

mod6<-gam(cvd~s(seq, k=14*10, fx=T)+dow+s(tmpd02), family=quasipoisson, data=CVDdaily)
summary(mod6)
pacf(residuals(mod6,type="deviance"),plot=T)

par(mfrow=c(2,2))
pacf(residuals(mod3.02,type="deviance"),plot=T)
pacf(residuals(mod4,type="deviance"),plot=T)
pacf(residuals(mod5,type="deviance"),plot=T)
pacf(residuals(mod6,type="deviance"),plot=T)

sum(pacf(residuals(mod3.02,type="deviance"),plot=T)$acf)
sum(pacf(residuals(mod4,type="deviance"),plot=T)$acf)
sum(pacf(residuals(mod5,type="deviance"),plot=T)$acf)
sum(pacf(residuals(mod6,type="deviance"),plot=T)$acf)


### Add ozone
par(mfrow=c(1,1))
plot(cvd~o3mean)
lines(lowess(cvd~o3mean), col=2, lwd=2)

mod4.o3mean<-gam(cvd~o3mean+s(seq, k=84, fx=T)+dow+s(tmpd02), family=quasipoisson, data=CVDdaily)
summary(mod4.o3mean)

mod4.o3mean1<-gam(cvd~o3mean1+s(seq, k=84, fx=T)+dow+s(tmpd02), family=quasipoisson, data=CVDdaily)
summary(mod4.o3mean1)

mod4.o3mean2<-gam(cvd~o3mean2+s(seq, k=84, fx=T)+dow+s(tmpd02), family=quasipoisson, data=CVDdaily)
summary(mod4.o3mean2)

mod4.o3mean01<-gam(cvd~o3mean01+s(seq, k=84, fx=T)+dow+s(tmpd02), family=quasipoisson, data=CVDdaily)
summary(mod4.o3mean01)

mod4.o3mean02<-gam(cvd~o3mean02+s(seq, k=84, fx=T)+dow+s(tmpd02), family=quasipoisson, data=CVDdaily)
summary(mod4.o3mean02)

### o3mean2 (2-day lag) seems to have the largest effect
IQR(o3mean2, na.rm=T)
iqr.o3<-IQR(o3mean2, na.rm=T)
RR<-exp(iqr.o3*mod4.o3mean2$coef[2])
LL<-exp(iqr.o3*(mod4.o3mean2$coef[2]-1.96*summary(mod4.o3mean2)$se[2]))
UL<-exp(iqr.o3*(mod4.o3mean2$coef[2]+1.96*summary(mod4.o3mean2)$se[2]))
RR
LL
UL

### What happens if poisson is used?
mod4.o3mean2.poi<-gam(cvd~o3mean2+s(seq, k=84, fx=T)+dow+s(tmpd02), family=poisson, data=CVDdaily)
summary(mod4.o3mean2.poi)

### Check model
gam.check(mod4.o3mean2)

### Check linearity of the association
mod4.o3mean2.sp<-gam(cvd~s(o3mean2)+s(seq, k=84, fx=T)+dow+s(tmpd02), family=quasipoisson, data=CVDdaily)
summary(mod4.o3mean2.sp)
  #' p value for the spline is significant
  #' model suggests ozone can predict mortality
plot(mod4.o3mean2.sp, scale=0)
plot(mod4.o3mean2.sp, scale=0, select=1, xlab="Ozone (ppb)", ylab="Log Relative Risk")
  #' plot indicates the relationship is not linear
  #' still report RR for the linear model
  #' Does linear assumption make more biological sense?

#############################
########### CCO #############
#############################

library(season)
names(CVDdaily)

### Make sure that season package use the variable 'date' which should be in date format

CVDdaily$date<-CVDdaily$date1
CVDdaily[1:20,]
class(CVDdaily$date)
class(date)

detach(CVDdaily)
attach(CVDdaily)

# Effect of ozone on CVD death
cco1<-casecross(cvd~o3mean2+tmpd02+Mon+Tue+Wed+Thu+Fri+Sat,data=CVDdaily)
summary(cco1)

# match on day of the week
cco2<-casecross(cvd~o3mean2+tmpd02,matchdow=TRUE,data=CVDdaily)
summary(cco2)

# match on temperature to within a 5-degree F
cco3<-casecross(cvd~o3mean2+Mon+Tue+Wed+Thu+Fri+Sat,data=CVDdaily,
matchconf='tmpd02',confrange=5)
summary(cco3)


#############################################
########### Conditional Poisson #############
#############################################

library(gnm)
class(date)

# GENERATE strata of year, month and dow
CVDdaily$month  <- as.factor(months(CVDdaily$date))
CVDdaily$year   <- as.factor(format(CVDdaily$date, format="%Y") )
CVDdaily$dow    <- as.factor(weekdays(CVDdaily$date))
CVDdaily$stratum <- as.factor(CVDdaily$year:CVDdaily$month:CVDdaily$dow)
summary(CVDdaily$stratum)
dim(CVDdaily)
CVDdaily[1:20,30]

cpm1<-gnm(cvd~o3mean2+tmpd02, data=CVDdaily, family=quasipoisson, eliminate=factor(stratum))
summary(cpm1)

## using unconditional Poisson
cpm2<-glm(cvd~o3mean2+tmpd02+factor(stratum), data=CVDdaily, family=quasipoisson)
summary(cpm2)

##Meta-analysis

library(rmeta)

##Suppose you have the estimates from 16 cities of the effect of PM2.5 
##on Myocardial Infarction (MI) hospital admissions. 
##We want to obtain the combined effect across all the cities.
city<-1:16
coef<-c(0.00155,0.00445,-0.00597,0.00237,0.00031,-0.00035,0.00206,0.00127,
-0.00395,-0.00434,0.00241,0.00730,-0.00175,0.00117,0.00007,0.00350)

se<-c(0.013589379,0.003224905,0.002400626,0.001797566,0.001019834,0.002658267,
0.001529252,0.002388748,0.002583907,0.005885021,0.00122501,0.00597465,
0.002546777,0.002406304,0.000913846,0.00517299)

model<-meta.summaries(coef, se, names=city, method="random")
#method="random" estimates and adds a heterogeneity variance
summary(model)

##Plot the city-specific and summary results
par(mfrow=c(1,1))
plot(model)
metaplot(coef, se)

##Get combined estimate, its standard error and RR and 95% CI
model$summary 
model$summ
model$se.summary 
model$se

exp(model$summ*10)
exp((model$summ-1.96*model$se)*10)
exp((model$summ+1.96*model$se)*10)

