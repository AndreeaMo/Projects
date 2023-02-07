rm(list=ls())
library(FinTs)
library(rugrach)
library(tseries)
library(dynlm)
library(nlwaldTest)
library(lmtest)
library(broom)
library(car)
library(vrtest)
library(fGarch)
library(tsm)

#RANDAMENT
#import data
bd <- read.csv(file.choose())
bd = bd[-c(1),]

                          
#create ts object
a <- ts(bd$Randament_BTC, start=c(2011,9), frequency = 365)

autoplot(a) +
  ggtitle("Bitcoin price 2015-2022") +
  xlab("Year") +
  ylab("Price $")

# Spliting the data into training and test sets
a_training <- window(a,start=c(2011,9),end=c(2020,07))
a_test <- window(a, start=c(2020,08))

# Time series decomposition
#STL Decomposition
a_decompose <- stl(a_training, s.window = "periodic")
autoplot(a_decompose)

# remove seonality
a_season_adj_ <-seasadj(a_decompose)
plot(a_season_adj_)

a_season_adj = data.frame(a_season_adj_)

# ACF and PACF
ggAcf(a_season_adj, lag =36)
ggPacf(a_season_adj, lag =36)
a_season_adj %>% ggtsdisplay()


#Determine AR and MA components

acf_a_train <- acf(a_season_adj, main = "ACF price of Bitcoin", lag.max = 50) #MA(6)
pacf_a_train <- pacf(a_season_adj, main = "PACF Price of Bitcoin", ylab = "PACF",
                     lag.max = 50)# AR(6)


#Estimating ARIMA models

arima106 <-Arima(a_season_adj, order =c(6,0,1),
                 include.constant = TRUE)
coeftest(arima610)
summary(arima610)
tsdiag(arima601)
checkresiduals(arima601)

fit2_acc<- arima601%>% forecast(h=671) 
summary(fit2_acc)
forecast::accuracy(fit2_acc, a_test)

arima601 %>% forecast(h=671) %>% autoplot()
checkresiduals(arima601)


# ARCH model
library(dynlm)
# Step 1: Estimate mean equation r = beta + error
byd.mean <- dynlm(a_season_adj ~1,data = a_season_adj)

# Step 2: Retrieve the residuals from the former model and square them
ehatsq <- ts(resid(byd.mean)^2)

# Step 3: regress squared residuals on one-lagged squared residuals
byd.arch <- dynlm(ehatsq ~ L(ehatsq), data = ehatsq)

summary(byd.arch)
library(FinTS)
byd.archTest <- ArchTest(a_season_adj, lags = 1, demean = TRUE)
byd.archTest
#Because the p-value is < 0.1, we reject the null hypothesis and conclude the presence of ARCH(1) effects

#Estimating ARCH model
library(fGarch)
arch.fit <- garchFit(~garch(1,0), data = a_season_adj, trace = F)
summary(arch.fit)

#Based on the output, the estimated mean of the series is 0.00199
# the estimate variance it's 0.00132+0.35155

#Plot variance
plot(arch.fit,which ="all")

#determine garch model
garch(a_season_adj,grad = "numerical", trace=FALSE) #GARCH(1,1)

#GARCH (1,1)
garch_m=ugarchspec(variance.model = list(garchOrder=c(1,1)),
                   mean.model = list(armaOrder=c(6,1)))
garch_m_fit=ugarchfit(garch_m, data=a_season_adj, out.sample = 100)
View(garch_m_fit)
plot(garch_m_fit)

#Forecast volatility - forecast pe test - grafic cu forecast si valori reale
garch_forcast =ugarchforecast(garch_m_fit,n.ahead = 100, n.roll =10)
print(garch_forcast)
plot(garch_forcast, which= "all")


#eGARCH

egarch_m=ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1)),
                    mean.model = list(armaOrder=c(6,1)))
egarch_m_fit = ugarchfit(egarch_m,data=a_season_adj,out.sample = 100)
egarch_m_fit
plot(egarch_m_fit, which="all")

#Forecast volatility - forecast pe test - grafic cu forecast si valori reale
egarch_forcast =ugarchforecast(egarch_m_fit,n.ahead = 20, n.roll =10)
print(egarch_forcast)
plot(egarch_forcast, which= "all")

