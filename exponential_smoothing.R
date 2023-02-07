rm(list=ls())

#loading the libraries
library(forecast)
library(ggplot2)
library(readxl)
install.packages("TSstudio")
library(TSstudio)
library(lmtest)
install.packages("Metrics")
library(Metrics)
install.packages("uroot")
library(uroot)
install.packages("urca")
library(urca)
library(dplyr)
install.packages("fpp2")
library(fpp2)
library(tsm)
library(seasonal)

#import data set
bitcoin_price <- read.csv("BTC-USD_monthly.csv")[ ,c('Close')]

#create ts object

a <- ts(bitcoin_price, start=c(2015,01),end = c(2022,04), frequency = 12)

# Spliting the data into training and test sets
a_training <- window(a,start=c(2015,01),end=c(2019,12))
a_test <- window(a, start=c(2020,01))


#Plot
autoplot(a) +
  ggtitle("Bitcoin price 2015-2022") +
  xlab("Year") +
  ylab("Price $")


# Time series decomposition

# Moving average smoothing
# Medii mobile
# Seria are trend
autoplot(a_training) + xlab("Year") + ylab("Price $") +
  ggtitle("Monthly Price for Bitcoin 2015-2022")

# Date lunare, netezsc cu diferenta sezoniera la lagu 12
# Netezire la 12 termeni
# Se poate aplica si Hodrick-Prescott
autoplot(a_training, series="Data") +
  autolayer(ma(a_training, 12), series="12-MA") +
  xlab("Year") + ylab("Bitcoin Price") +
  ggtitle("Monthly Price for Bitcoin") +
  scale_colour_manual(values=c("Data"="grey","12-MA"="red"),
                      breaks=c("Data","12-MA"))

#Classical decomposition
# Multiplicative decomposition
a_training %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
    of daily price of bitcoin")

# STL decomposition
a_decompose <- stl(a_training, s.window = 5) 
autoplot(a_decompose) +
  ggtitle("STL decomposition of daily bitcoin price")

#remove sesonality
a_season_adj <- seasadj(a_decompose)
plot(a_season_adj)


autoplot(a_training, series="Data") +
  autolayer(trendcycle(a_decompose), series="Trend") +
  autolayer(seasadj(a_decompose), series="Seasonally Adjusted") +
  xlab("Year") + ylab("%") +
  ggtitle("daily bitcoin price-%") +
  scale_colour_manual(values=c("black","dark grey","red"),
                      breaks=c("Data","Seasonally Adjusted","Trend"))  

Box.test(abs(a_season_adj),lag = 12, type='Ljung')

# Exponential smoothing

#1. Simple exponential smoothing
a_bitcoin <- window(a_season_adj,start=2015)
autoplot(a_bitcoin) +
  ylab("Bitcoin Price") + xlab("Year")

a_ses <- ses(a_bitcoin, h=36)
round(accuracy(a_ses),2)
plot(a_ses)

checkresiduals(a_ses)
Box.test(abs(a_ses$residuals),lag = 12, type='Ljung')

#forecast
autoplot(a_ses)

#accuracy testing
a_ses.acc <- a_ses %>%
  forecast(h=28) %>%
  accuracy(a)
a_ses.acc 

#2. Holt Winter additive and multiplicative
a_hw <- hw(a_training,seasonal="multiplicative")

autoplot(a_training) +
  autolayer(a_hw, series="HW multiplicative forecasts", PI=FALSE) +
  xlab("Year") +
  ylab("Price $") +
  ggtitle("Bitcoin Price") +
  guides(colour=guide_legend(title="Forecast"))

round(accuracy(a_hw),2)

checkresiduals(a_hw)
Box.test(abs(a_hw$residuals),lag = 12, type='Ljung')
#forecast
a_hw %>% forecast(h=20) %>%
  autoplot() +
  ylab("Bitcoin Price")



#accuracy testing
a_hw.acc <- a_hw %>%
  forecast(h=20) %>%
  accuracy(a)
a_hw.acc

#3. ETS model
a_ets <- ets(a_season_adj)
summary(a_ets)

a_ets %>% forecast(h=40) %>%
  autoplot() +
  ylab("Bitcoin Price")

checkresiduals(a_ets)
Box.test(abs(a_ets$residuals),lag = 12, type='Ljung')

#accuracy testing
a_ets.acc <- a_ets %>%
  forecast(h=28) %>%
  accuracy(a)
a_ets.acc





