library(forecast)

gas <- ts(
  read.csv("https://robjhyndman.com/data/gasoline.csv", header=FALSE)[,1],
          freq=365.25/7, start=1991+31/365.25)

bestfit <- list(aicc=Inf)
for(i in 1:25)
{
  fit <- auto.arima(gas, xreg=fourier(gas, K=i), seasonal=FALSE)
  if(fit$aicc < bestfit$aicc)
    bestfit <- fit
  else break;
}
fc <- forecast(bestfit, xreg=fourier(gas, K=12, h=104))
plot(fc)


# Crossvalidation ---------------------------------------------------------

library(forecast)
library(tidyverse)
library(fpp2)


# Subset data
arimaFC <- function(x, h){
  forecast(auto.arima(x), h = h)
  }

e <- tsCV(y = goog200, forecastfunction = arimaFC, h = 3)

sqrt(mean(e^2, na.rm=TRUE))
