#loading packages from Libraries
library(lmtest)
library("forecast")
library(FitAR)
library(readxl)
library(fUnitRoots)

#import data of goldprice
gold_data<-read_excel(file.choose())
attach(gold_data)
head(gold_data)

#convert to time series
goldprice_tsData<- ts(gold_data$Price,start=c(2011,1),frequency=365)
plot(goldprice_tsData,main="goldprice_tsdata")

#decompose timeseries into time series components
goldprice_timeseriescomponents <- decompose(goldprice_tsData)
plot(goldprice_timeseriescomponents)

#checking stationarity of data
adfTest(goldprice_tsData) 
acf(goldprice_tsData,main="acf gold_ts")
pacf(goldprice_tsData,main="pacf gold_ts")

#Differencing once
goldprice_tsdiff1<- diff(goldprice_tsData)
plot(goldprice_tsdiff1)
adfTest(goldprice_tsdiff1)
acf(goldprice_tsdiff1)
pacf(goldprice_tsdiff1)

#Natural log transformation and differenced
goldprice_ln=log(goldprice_tsData)
gold_diffln=diff(goldprice_ln)
plot(gold_diffln)
adfTest(gold_diffln)
acf(gold_diffln)
pacf(gold_diffln)

#remove seasonality components
goldprice_timeseriesseasonallyadjusted <- goldprice_tsData- goldprice_timeseriescomponents$seasonal
plot(goldprice_timeseriesseasonallyadjusted)
gold_tsseasonalstat <- diff(goldprice_timeseriesseasonallyadjusted, differences=1)
plot(gold_tsseasonalstat,main="gold_tsseasonalstat")
acf(gold_tsseasonalstat,main="acf gold_tsseasonalstat") 
pacf(gold_tsseasonalstat,main="acf gold_tsseasonalstat")

#Fiting of ARIMA model
ARIMAfit = auto.arima(goldprice_tsData, approximation=FALSE,trace=FALSE)
summary(ARIMAfit)

#Comparing model by mannually fitted model
fit <- arima(gold_data$Price, c(1, 1, 1))
summary(fit)
#significance of coefficients
coeftest(fit)

#Forecast using model
pred <- predict(fit, n.ahead = 5)

#create an ACF and PACF plot of the residuals of our best fit
acf(ts(fit$residuals),main='ACF Residual')
pacf(ts(fit$residuals),main='PACF Residual')
boxresult<-LjungBoxTest (fit$residuals,k=2,StartLag=1)
plot(boxresult,main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag",col="green")
qqnorm(fit$residuals,col="orange")
qqline(fit$residuals,col="black")

#forecasting the series
arima <- forecast(fit, h=5)
arima
accuracy(arima)

#Plotting the forecast
plot(arima)