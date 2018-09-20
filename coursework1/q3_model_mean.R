library(evir)
library(forecast)
library(tseries)

# (b) Using time series modelling, or otherwise, find an appropriate model for the mean
# Central England Temperature for January 2001 to December 2016.

# Reading data
monthly <- read.csv("data/cetml1659on.csv", header = TRUE, colClasses=c("Date",NA))
monthly <- monthly[monthly$date>="2001-01-01"&monthly$date<="2016-12-01",]

plot(monthly, type="l", 
     main="Monthly temperature from 2001 to 2016",xlab="Date",ylab="Average temperature (degrees)")

# Time series
monthly.ts = ts(monthly$value, start=c(2001,01), frequency = 12)
# Autocorrelation plot
plot(acf(monthly.ts), main="Monthly average temperature (2001-2016)")
# Partial Autocorrelation plot
plot(pacf(monthly.ts), main="Monthly average temperature (2001-2016)")

# Another method for extracting a subset out of a time series
# window(tsales, start=c(2003, 5), end=c(2004, 6))

# Seasonal decomposition
plot(stl(monthly.ts, s.window="period"))

# (c) Use the model to find prediction and confidence intervals for the month of July
# 2017. Use these intervals to write a long range weather forecast of July’s tempera-
# ture as if written for the general public.

# AR model
ar2.model = arma(monthly.ts,order=c(2,0))
arTest.model = ar(monthly.ts) #????
summary(ar2.model)
plot(ar2.model)

# Moving Average (MA) 
ma2.model = arma(monthly.ts,order=c(0,3)) 
summary(ma2.model)
plot(ma2.model) # par(mar = rep(2, 4))

# ARMA 
arma.model = arma(monthly.ts,order=c(2,3)) 
summary(arma.model) # Lower sum of squares (323)
plot(arma.model)

# ARIMA
# Check if the ts is stationary
ndiffs(monthly.ts)
adf.test(monthly.ts) # It is stationary

arima.model1 <-arima(monthly.ts,order=c(2,0,0))
arima.model2 <-arima(monthly.ts,order=c(0,0,3))
arima.model3 <-arima(monthly.ts,order=c(2,0,3))
arima.model4 <-arima(monthly.ts,order=c(2,0,2))
arima.model5 <-arima(monthly.ts,order=c(2,0,1))
arima.model6 <-arima(monthly.ts,order=c(1,0,2))



# Visual comparison
plot(monthly.ts,
     main="Monthly temperature from 2001 to 2016",xlab="Date",ylab="Average temperature (degrees)", lwd=2)
lines(fitted(arima.model1),col="red", lt=2)
lines(fitted(arima.model2),col="blue", lt=2)
lines(fitted(arima.model3),col="green", lt=2)

# Error plots
plot(residuals(arima.model1),col="red", lt=1)
lines(residuals(arima.model2),col="blue", lt=1)
lines(residuals(arima.model3),col="green", lt=1)


# Accuracy assesment
accuracy(arima.model1)
accuracy(arima.model2)
accuracy(arima.model3)
accuracy(arima.model4)
accuracy(arima.model5)
accuracy(arima.model6)
AIC(arima.model1, arima.model2, arima.model3, arima.model4)


# Evaluating the model fit
qqnorm(arima.model3$residuals)
qqline(arima.model3$residuals)
Box.test(arima.model3$residuals, type="Ljung-Box")
plot(acf(arima.model3$residuals), main="ACF for the residuals of ARMA(2,3)")


# (c) Use the model to find prediction and confidence intervals for the month of July
# 2017. Use these intervals to write a long range weather forecast of July’s tempera-
# ture as if written for the general public.

arima.forecast <-forecast.Arima(arima.model3,h=7) # 7 months -> Jul
arima.forecast
plot(arima.forecast, level=,
     xlab="Years", ylab="Average temperature (degrees)")
