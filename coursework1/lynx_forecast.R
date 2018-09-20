library("forecast") # Load package forecast
bread.arima<-arima(lynx,order=c(2,0,0))
bread.forecast<-forecast.Arima(bread.arima,h=5)
bread.forecast
plot.forecast(bread.forecast)