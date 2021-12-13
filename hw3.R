#ADEC7460 Homework Time Series
library(fpp2)
#The data set was acquired from Kaggle: https://www.kaggle.com/sripaadsrinivasan/global-peanut-prices
#It contains monthly peanut(groundnut) prices measured in U.S. Dollars per metric ton(not seasonally adjusted)
#The data runs from Jan 1990 to October 2020
nuts <- read.csv('Global_Peanut_price.csv')
nutsts <- ts(nuts$PGNUTSUSDM, start=c(1990,1), end=c(2020,10), frequency=12)
autoplot(nutsts) + ylab('U.S. Dollars per metric ton')
summary(nuts)
#There are 370 observations. The range of values is 586.1 to 2600.2. The median is found to be 945.8 and the mean is 1242.3.
ggseasonplot(nutsts)
#There seems to be a weak seasonal component found within the data. A dip is found around July which then recovers the following month in August.

#80/20 split for train/test
train <- nutsts[1:296]
test <- nutsts[297:370]
traints <- ts(train, start=c(1990,1), end=c(2014,8), frequency=12)
testts <- ts(test, start=c(2014,9), end=c(2020,10), frequency=12)
autoplot(traints) + ylab('U.S. Dollars per metric ton') + ggtitle('Global Price of Peanuts(Groundnuts)')
autoplot(testts) + ylab('U.S. Dollars per metric ton') + ggtitle('Global Price of Peanuts(Groundnuts)')

#Time series decomposition
decompfit <- stl(traints, t.window=13, s.window="periodic", robust=TRUE)
autoplot(decompfit)
fcast <- forecast(decompfit, method='naive', h=74)
autoplot(fcast) + ylab('U.S. Dollars per metric ton') + ggtitle('Global Price of Peanuts(Groundnuts)')
traintsbc <- BoxCox(traints, lambda=BoxCox.lambda(traints))
decompfit2 <- stl(traintsbc, t.window=13, s.window="periodic", robust=TRUE)
autoplot(decompfit2)
fcast2 <- forecast(decompfit2, method='naive', h=74)
autoplot(fcast2) + ylab('U.S. Dollars per metric ton') + ggtitle('Global Price of Peanuts(Groundnuts)')
checkresiduals(fcast)
checkresiduals(fcast2)
accuracy(fcast, testts)
accuracy(fcast2, testts)
#Here, the model that performs better is the time series decomposition without a BoxCOX transformations. This is due to the accuracy against the test set is better for the model without a BoxCox transformation.

#Exponential smoothing
etsfit <- ets(traints)
summary(etsfit)
etsfit2 <- ets(traints, allow.multiplicative.trend = TRUE)
summary(etsfit2)
etsfit3 <- ets(traints, allow.multiplicative.trend = TRUE, lambda = BoxCox.lambda(traints))
summary(etsfit3)
fcast3 <- forecast(etsfit2,h=74)
autoplot(fcast3) + ylab('U.S. Dollars per metric ton') + ggtitle('Global Price of Peanuts(Groundnuts)')
fcast4 <- forecast(etsfit3,h=74)
autoplot(fcast4) + ylab('U.S. Dollars per metric ton') + ggtitle('Global Price of Peanuts(Groundnuts)')
checkresiduals(fcast3)
checkresiduals(fcast4)
accuracy(fcast3, testts)
accuracy(fcast4, testts)
#Here, the model that performs better is the ETS(M,Md,N) model as it has the lowest AIC, AICc and BIC values.

#Time series regression model
tsfit <- tslm(traints ~ trend + season)
summary(tsfit)
fcast5 <- forecast(tsfit, h=74)
autoplot(fcast5) + ylab('U.S. Dollars per metric ton') + ggtitle('Global Price of Peanuts(Groundnuts)')
checkresiduals(fcast5)
accuracy(fcast5, testts)
tsfit2 <- tslm(traints ~ trend)
summary(tsfit2)
fcast6 <- forecast(tsfit2, h=74)
autoplot(fcast6) + ylab('U.S. Dollars per metric ton') + ggtitle('Global Price of Peanuts(Groundnuts)')
checkresiduals(fcast6)
accuracy(fcast6, testts)
#Here, the best model found is the time series regression with both trend and seasonal components. This is because the accuracy against the test set is better for the model with both trend and seasonal components.

#ARIMA
arimafit <- auto.arima(traints)
arimafit
arimafit2 <- Arima(traints, order = c(3,1,1), seasonal = c(2,0,2), include.mean = TRUE)
arimafit2
arimafit3 <- Arima(traints, order = c(3,1,1), seasonal = c(2,0,2), include.drift = TRUE)
arimafit3
arimafit4 <- Arima(traints, order = c(4,1,1), seasonal = c(2,0,2))
arimafit4
arimafit5 <- Arima(traints, order = c(4,1,2), seasonal = c(2,0,2))
arimafit5
arimafit6 <- Arima(traints, order = c(5,1,2), seasonal = c(2,0,2))
arimafit6
arimafit7 <- Arima(traints, order = c(5,1,2), seasonal = c(2,1,2))
arimafit7
arimafit8 <- Arima(traints, order = c(5,1,2), seasonal = c(4,1,2))
arimafit8
checkresiduals(arimafit)
checkresiduals(arimafit2)
checkresiduals(arimafit3)
checkresiduals(arimafit4)
checkresiduals(arimafit5)
checkresiduals(arimafit6)
checkresiduals(arimafit7)
checkresiduals(arimafit8)
fcast7 <- forecast(arimafit6, h=74)
autoplot(fcast7) + ylab('U.S. Dollars per metric ton') + ggtitle('Global Price of Peanuts(Groundnuts)')
accuracy(fcast7, testts)
#ARIMA(5,1,2)(2,0,2)[12] is the best model through estimation and order selection. Although this model does not have the best AIC, AICc and BIC values found, it has the best ACF plot.

#Comparing best models
checkresiduals(fcast)
checkresiduals(fcast3)
checkresiduals(fcast5)
checkresiduals(fcast7)
accuracy(fcast, testts)
accuracy(fcast3, testts)
accuracy(fcast5, testts)
accuracy(fcast7, testts)
#In terms of the best model, it has to come down to between the time series decomposition that includes both trend and seasonal components against the ARIMA(5,1,2)(2,0,2)[12]. Here I would choose the ARIMA model as although the accuracy against the test set is not as good as the time series decomposition model, the ACF plot of the ARIMA model is much better. The ACF plot for ARIMA has all but one leg within the 95% limit boundary.