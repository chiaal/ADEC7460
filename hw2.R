library(fpp2)
library(Metrics)
#Hyndman Chapter 7 Exercises
#1
#1a
pigsdata <- pigs
autoplot(pigsdata) + ylab("Number of pigs slaughtered in Victoria each month") +xlab("Year")
fc <- ses(pigsdata, h=4)
round(accuracy(fc),2)
fc
fc$model
#the optimal values of alpha is found to be 0.2971 and initial state of l is 77260.0561.
#1b
s <- sd(fc$residuals)
fcmean <- fc$mean[1]
c(fcmean - (1.96*s), fcmean + (1.96*s))
#The manually computed 95% prediction interval for the first forecast is found to be 78679.97 to 118952.84 compared to the interval produced by R 78611.97 to 119020.8. The manual Lo 95 is higher than R's and the manual Hi 95 is lower than R's but the results are similar.

#2
myses <- function(y, alpha, level){
  yhat <- 10
  for(i in 1:length(y)){
    yhat <- alpha*y[i] + (1 - alpha)*yhat
  }
  yhat
}

fc <- ses(pigsdata, h=1)
a <- fc$model$par[1]
l <- fc$model$par[2]
fc
myses(pigsdata, alpha = a, level = l)
#The ses() forecast of the next observation in the series pigs returns 98816.41, which is the same forecast as the manually written one

#3
mysesp <- function(pars = c(alpha, level), y){
  error <- 0
  SSE <- 0
  alpha <- pars[1]
  level <- pars[2]
  yhat <- 10
  for(i in 1:length(y)){
    error <- y[i] - yhat
    SSE <- SSE + error^2
    yhat <- alpha*y[i] + (1-alpha)*yhat
  }
  return(SSE)
}

optimalparam <- optim(par = c(0.5, pigs[1]), y = pigs, fn = mysesp)
optimalparam$par[1]
optimalparam$par[2]
fc$model$par[1]
fc$model$par[2]
#No, only the inital level is similar between the manual function and ses() while the alpha is very different.

#4
mysesf <- function(y){
  optimalparam <- optim(par= c(0.5, y[1]), y=y, fn = mysesp)
  a <- optimalparam$par[1]
  l <- optimalparam$par[2]
  preds <-myses(y, a, l)
  return(preds)
}
mysesf(pigs)
ses(pigs, h = 1)$mean

#5
#5a
autoplot(books)
#As seen from the plot, there is an upward trend for both Paperback and Hardcover overtime with a negative correlation between the peaks of both series.
#5b
autoplot(ses(books[,1]))
autoplot(ses(books[,2]))
#5c
rmse(books[,1],ses(books[,1])$fitted)
rmse(books[,2],ses(books[,2])$fitted)

#6
#6a
(fc <- holt(books[,1], h=4))
(fc2 <- holt(books[,2], h=4))
autoplot(fc)
autoplot(fc2)
#6b
rmse(books[,1],fc$fitted)
rmse(books[,2],fc2$fitted)
#The RMSE measures of Holt's method for the two series is lower than the ses() method, this is largely due to the additional parameter that Holt's linear method uses.
#6c
#Taking a look at the plot of the forecasts, the forecasts from Holt's linear method is best as it captures the upward trend and has a tighter prediction interval.
#6d
ses(books[,1])$mean[1] + 1.96*rmse(books[,1],ses(books[,1])$fitted)
ses(books[,1])$mean[1] - 1.96*rmse(books[,1],ses(books[,1])$fitted)
ses(books[,2])$mean[1] + 1.96*rmse(books[,2],ses(books[,2])$fitted)
ses(books[,2])$mean[1] - 1.96*rmse(books[,2],ses(books[,2])$fitted)

holt(books[,1], h=1)$mean[1] + 1.96*rmse(books[,1],holt(books[,1], h=1)$fitted)
holt(books[,1], h=1)$mean[1] - 1.96*rmse(books[,1],holt(books[,1], h=1)$fitted)
holt(books[,2], h=1)$mean[1] + 1.96*rmse(books[,2],holt(books[,2], h=1)$fitted)
holt(books[,2], h=1)$mean[1] - 1.96*rmse(books[,2],holt(books[,2], h=1)$fitted)
#95% prediction interval for paperback is 141.1798 to 273.0395 for ses() and 148.4384 to 270.4951 for Holt's. 95% prediction interval for hardcover is 176.9753 to 302.1449 for ses() and 196.8745 to 303.4733 for Holt's.
#Holt's has a tighter 95% prediction interval for both paperback and hardcover.

#7
fc <- holt(eggs, h=100)
fc2 <- holt(eggs, h=100, damped = TRUE)
fc3 <- holt(eggs, h=100, damped = FALSE, biasadj = TRUE)
fc4 <- holt(eggs, h=100, damped = TRUE, biasadj = TRUE)
fc5 <- holt(eggs, h=100, damped = TRUE, biasadj = TRUE, lambda = BoxCox.lambda(eggs))
fc6 <- holt(eggs, h=100, damped = FALSE, biasadj = TRUE, lambda = BoxCox.lambda(eggs))
fc7 <- holt(eggs, h=100, damped = FALSE, biasadj = FALSE, lambda = BoxCox.lambda(eggs))

autoplot(fc)
autoplot(fc2)
autoplot(fc3)
autoplot(fc4)
autoplot(fc5)
autoplot(fc6)
autoplot(fc7)

rmse(eggs, fc$fitted)
rmse(eggs, fc2$fitted)
rmse(eggs, fc3$fitted)
rmse(eggs, fc4$fitted)
rmse(eggs, fc5$fitted)
rmse(eggs, fc6$fitted)
rmse(eggs, fc7$fitted)
#The model that gave the best RMSE is fc6(BoxCox transformation with a bias adjust)

#8
#8a
retaildata <- readxl::read_excel("retail.xlsx", skip=1)
retaildatats <- ts(retaildata[,"A3349873A"], frequency=12, start=c(1982,4))
autoplot(retaildatats)
#Looking at the plot of the retail data, as time increases the effect of seasonality has also increased.
#8b
fit <- hw(retaildatats, seasonal="multiplicative")
fit2 <- hw(retaildatats, seasonal="multiplicative", damped=TRUE)
autoplot(fit)
autoplot(fit2)
#8c
rmse(retaildatats,hw(retaildatats, seasonal="multiplicative", h=1)$fitted)
rmse(retaildatats,hw(retaildatats, seasonal="multiplicative", damped=TRUE,h=1)$fitted)
#The RMSE of the non-damped trend is abit lower so it is more preferable.
#8d
checkresiduals(fit)
#The residuals from the best method does not look like white noise as there are lags that lie outside the 95% limits.
#8e
retailtrain <- window(retaildatats, end = c(2010,12))
retailvalid <- window(retaildatats, end = c(2011))
forecast::accuracy(snaive(retailtrain),retailvalid)[2,2]
forecast::accuracy(hw(retailtrain, seasonal="multiplicative"),retailvalid)[2,2]
#No, the RMSE for the seasonal naive approach is lower than the Holt-Winters' non-damped trend

#9
retailbc <- BoxCox(retaildatats, BoxCox.lambda(retaildatats))
retailstl <- mstl(retailbc, s.window=13, robust = TRUE)
forecast(retailstl, lambda=BoxCox.lambda(retaildatats))
forecast(ets(seasadj(decompose(retaildatats, "multiplicative"))))
autoplot(forecast(retailstl, lambda=BoxCox.lambda(retaildatats)))
autoplot(forecast(ets(seasadj(decompose(retaildatats, "multiplicative")))))
#The ETS() method performed better than the STL decomposition applied to the Box-Cox transformed series, but not as good as the forecasts from the Holt Winter's method.

#10
#10a
autoplot(ukcars)
#The series starts with a downward trend until 1980 where it experiences an upward trend. Seasonality is observed throughout the time period.
#10b
ukcarsstl <- stl(ukcars, t.window = 13, s.window='periodic', robust = TRUE)
autoplot(ukcarsstl)
#10c
ukcarsstlf <- stlf(seasadj(ukcarsstl), etsmodel="AAN", damped=TRUE)
autoplot(ukcarsstlf)
#10d
ukcarsholt <- holt(seasadj(ukcarsstl), damped=FALSE)
autoplot(ukcarsholt)
#10e
ukcarsets <- ets(seasadj(ukcarsstl), damped=TRUE)
summary(ukcarsets)
#10f
rmse(ukcarsets$fitted, seasadj(ukcarsstl))
rmse(ukcarsstlf$fitted, seasadj(ukcarsstl))
rmse(ukcarsholt$fitted, seasadj(ukcarsstl))
#The additive damped trend method applied to the seasonally adjusted data has the lowest RMSE which gives the best in-sample fits.
#10g
autoplot(ukcarsstlf)
autoplot(ukcarsholt)
autoplot(forecast(ukcarsets))
#The additive damped trend method applied to the seasonally adjusted data has the most reasonable forecast.
#10h
checkresiduals(ukcarsstlf)

#11
#11a
autoplot(visitors)
#11b
visitors.train <- window(visitors, end = c(2003,4))
autoplot(visitors.train)
visitors.test <- window(visitors, start = c(2003,4), end = c(2005,4))
autoplot(visitors.test)
visitors.train.hw <- hw(visitors.train, seasonal = 'multiplicative')
autoplot(visitors.train.hw) + autolayer(visitors.test)
#11c
#Multiplicative seasonality is necessary here because the seasonality of the visitors series increases with time.
#11d
visitors.train.ets <- ets(visitors.train)
autoplot(forecast(visitors.train.ets)) + autolayer(visitors.test)
visitors.train.etsadd <- ets(BoxCox(visitors.train, lambda = BoxCox.lambda(visitors.train)), model = "AAA")
autoplot(forecast(visitors.train.etsadd))
visitors.snaive <- snaive(visitors.train)
autoplot(visitors.snaive) + autolayer(visitors.test)
visitors.train.bc <- BoxCox(visitors.train, lambda = BoxCox.lambda(visitors.train))
visitors.train.stl <- stl(visitors.train.bc, t.window=13, s.window='periodic', robust=TRUE)
visitors.train.stlbcets <- ets(seasadj(visitors.train.stl))
autoplot(forecast(visitors.train.stlbcets))
#11e
checkresiduals(visitors.train.ets)
checkresiduals(visitors.train.etsadd)
checkresiduals(visitors.snaive)
checkresiduals(visitors.train.stlbcets)
#The method that gives the best forecasts is the first ETS model and it's the closest one to resembling white noise but still does not pass the residuals test.

#12
#12a
fets <- function(y, h) {
  forecast(ets(y), h = h)
}

tsCV(qcement, fets, h=4)
tsCV(qcement, snaive, h= 4)
#12b
mean(tsCV(qcement, fets, h=4)^2,na.rm=T)
mean(tsCV(qcement, snaive, h= 4)^2,na.rm=T)
#The ETS forecasts are more accurate which is expected.

#13
bricksq.BC <- BoxCox(bricksq, BoxCox.lambda(bricksq))
a10.BC <- BoxCox(a10, BoxCox.lambda(a10))

ausbeer.train <- window(ausbeer, end = c(2007,2))
ausbeer.test  <- window(ausbeer, start = c(2007,3))
bricksq.BC.train    <- window(bricksq.BC, end = c(1991,1))
bricksq.BC.test    <- window(bricksq.BC, start = c(1991,2))
dole.train <- window(dole, end=c(1989,7))
dole.test <- window(dole, start=c(1989,8))
a10.train <- window(a10.BC, end = c(2005,6))
a10.test <- window(a10.BC, start = c(2005,7))
h02.train <- window(h02, end = c(2005,7))
h02.test <- window(h02, start = c(2005,8))
usmelec.train <- window(usmelec, end = c(2010,6))
usmelec.test <- window(usmelec, start = c(2010,7))

ausbeer.ets <- ets(ausbeer.train)
bricksq.BC.ets <- ets(bricksq.BC.train)
dole.ets <- ets(dole.train)
a10.ets <- ets(a10.train)
h02.ets <- ets(h02.train)
usmelec.ets <- ets(usmelec.train)

ausbeer.etsF <- forecast(ausbeer.ets, h=12)
bricksq.BC.etsF <- forecast(bricksq.BC.ets, h=12)
dole.etsF <- forecast(dole.ets, h=36)
a10.etsF <- forecast(a10.ets, h=36)
h02.etsF <- forecast(h02.ets, h=36)
usmelec.etsF <- forecast(usmelec.ets, h = 36)

ausbeer.sn <- snaive(ausbeer.train, h = 12)
bricksq.BC.sn <- snaive(bricksq.BC.train, h= 12)
dole.sn <- snaive(dole.train, h= 36)
a10.sn <- snaive(a10.train, h = 36)
h02.sn <- snaive(h02.train, h = 36)
usmelec.sn <- snaive(usmelec.train, h= 36)

ausbeer.stlf <- stlf(ausbeer.train, h = 12)
bricksq.BC.stlf <- stlf(bricksq.BC.train, h= 12)
dole.stlf <- stlf(dole.train, h= 36)
a10.stlf <- stlf(a10.train, h = 36)
h02.stlf <- stlf(h02.train, h = 36)
usmelec.stlf <- stlf(usmelec.train, h= 36)

forecast::accuracy(ausbeer.etsF,ausbeer.test)
forecast::accuracy(ausbeer.sn,ausbeer.test)
forecast::accuracy(ausbeer.stlf,ausbeer.test)
#ets() gave the best forecast for ausbeer
forecast::accuracy(bricksq.BC.etsF,bricksq.BC.test)
forecast::accuracy(bricksq.BC.sn,bricksq.BC.test)
forecast::accuracy(bricksq.BC.stlf,bricksq.BC.test)
#stlf() gave the best forecast for bricksq 
forecast::accuracy(dole.etsF,dole.test)
forecast::accuracy(dole.sn,dole.test)
forecast::accuracy(dole.stlf,dole.test)
#snaive() gave the best forecast for dole
forecast::accuracy(a10.etsF,a10.test)
forecast::accuracy(a10.sn,a10.test)
forecast::accuracy(a10.stlf,a10.test)
#ets() gave the best forecast for a10
forecast::accuracy(h02.etsF,h02.test)
forecast::accuracy(h02.sn,h02.test)
forecast::accuracy(h02.stlf,h02.test)
#stlf() gave the best forecast for h02
forecast::accuracy(usmelec.etsF,usmelec.test)
forecast::accuracy(usmelec.sn,usmelec.test)
forecast::accuracy(usmelec.stlf,usmelec.test)
#ets() gave the best forecast for usmelec

#14
#14a
autoplot(forecast(ets(bicoal)))
autoplot(forecast(ets(chicken)))
autoplot(forecast(ets(dole)))
autoplot(forecast(ets(usdeaths)))
autoplot(forecast(ets(lynx)))
autoplot(forecast(ets(ibmclose)))
autoplot(forecast(ets(eggs)))
#It does not always give good forecasts, the series with good forecasts using ets() are chicken, dole, usdeaths and lynx
#14b
#ets() does not work well for bicoal, ibmclose and eggs and I believe it is because it does not work well with series that have a non-constant trend and no seasonality.

#15
dole.ets <- ets(dole,model = "MAM")
dole.etsf <- forecast(dole.ets, h=1)
dole.hwm <- hw(dole, seasonal = 'multiplicative', h=1)
dole.etsf$mean
dole.hwm$mean
#The point forecasts from an ETS(M,A,M) model and Holt-Winter's multiplicative model come out to be very similar

#Chapter 8 Exercises
#1
#1a
#The differences among all three figures is that the dashed line boundaries are getting tighter due to the increases amount of random numbers. The ACF of all three look like that of a white noise series as there are almost no spikes lying outside the 95% limits. Series: x2 has two spikes that lie outside the 95% limits but still expect the autocorrelation to be close to zero.
#1b
#The dashed line boundary is defined by ±2/sqrt(T), where T is the length of time series. The boundary length decreases as the length of the time series increases, which is shown in all three plots. Critical values are defined by ±1.96/sqrt(T), which explains as the length of the time series increases, the distance of the critical values decreases, causing critical values to be at different distances from the mean of zero.

#2
autoplot(ibmclose)
ggAcf(ibmclose)
ggPacf(ibmclose)
#From the first autoplot, it can be seen that the IBM closing prices are non-stationary because there is an increasing trend and then a decreasing trend. The values of the time series do not fluctuate around a constant mean or with a constant variance. For the ACF plot, another indication that this is a non-stationary time series is that the ACF data decreases slowly instead of dropping to zero relatively quickly. For the PACF plot, the first lag always equals to the first lag of ACF, which very high and outside the 95% limits while the the other lags are close to zero. This suggests the data is non-stationary again and should be differenced.

#3
#3a
ggtsdisplay(usnetelec)
BoxCox.lambda(usnetelec)
usnetelecbc <- BoxCox(usnetelec, lambda=BoxCox.lambda(usnetelec))
ndiffs(usnetelecbc)
ggtsdisplay(diff(diff(usnetelecbc)))
#Using lambda 0.5167714 for the Box-Cox transformation and order of difference of 2
#3b
ggtsdisplay(usgdp)
BoxCox.lambda(usgdp)
usgdpbc <- BoxCox(usgdp, lambda=BoxCox.lambda(usgdp))
ndiffs(usgdpbc)
ggtsdisplay(diff(usgdpbc))
#Using lambda 0.366352 for the Box-Cox transformation and order of difference of 1
#3c
ggtsdisplay(mcopper)
BoxCox.lambda(mcopper)
mcopperbc <- BoxCox(mcopper, lambda=BoxCox.lambda(mcopper))
ndiffs(mcopperbc)
ggtsdisplay(diff(mcopperbc))
#Using lambda 0.1919047 for the Box-Cox transformation and order of difference of 1
#3d
ggtsdisplay(enplanements)
BoxCox.lambda(enplanements)
enplanementsbc <- BoxCox(enplanements, lambda=BoxCox.lambda(enplanements))
ndiffs(enplanementsbc)
nsdiffs(enplanementsbc)
ggtsdisplay(diff(diff(enplanementsbc), lag=12))
#Using lambda -0.2269461 for the Box-Cox transformation, order of difference of 1, order of seasonality difference of 1 with lag 12
#3e
ggtsdisplay(visitors)
BoxCox.lambda(visitors)
visitorsbc <- BoxCox(visitors, lambda=BoxCox.lambda(visitors))
ndiffs(visitorsbc)
nsdiffs(visitorsbc)
ggtsdisplay(diff(diff(visitorsbc), lag=12))
#Using lambda 0.2775249 for the Box-Cox transformation, order of difference of 1, order of seasonality difference of 1 with lag 12

#4
#The backshift operator notation for enplanements data, (1-B)(1-B)yt

#5
retaildata <- readxl::read_excel("retail.xlsx", skip=1)
retaildatats <- ts(retaildata[,"A3349873A"], frequency=12, start=c(1982,4))
ggtsdisplay(retaildatats)
BoxCox.lambda(retaildatats)
retaildatatsbc <- BoxCox(retaildatats, lambda=BoxCox.lambda(retaildatats))
ndiffs(retaildatatsbc)
nsdiffs(retaildatatsbc)
ggtsdisplay(diff(diff(retaildatatsbc), lag=12))
#Using lambda 0.1276369 for the Box-Cox transformation, order of difference of 1, order of seasonality difference of 1 with lag 12

#6
#6a
y <- ts(numeric(100))
e <- rnorm(100)
for(i in 2:100)
  y[i] <- 0.6*y[i-1] + e[i]
#6b
autoplot(y)
for(i in 2:100)
  y[i] <- 1*y[i-1] + e[i]
autoplot(y)
for(i in 2:100)
  y[i] <- 0.3*y[i-1] + e[i]
autoplot(y)
#As phi is changed, decreasing values causes more frequent fluctuations, while increasing values causes less fluctuations moving towards a periodic shape.
#6c
ma1 <- function(theta, sd=1, n=100){
  y1 <- ts(numeric(n))
  e1 <- rnorm(n, sd=sd)
  for(i in 2:n)
    y1[i] <- theta*e1[i-1] + e1[i]
  return(y1)
}
ma1_0.6 <- ma1(0.6)
#6d
ma1_0.3 <- ma1(0.3)
ma1_1 <- ma1(1)
autoplot(ma1_0.6)
autoplot(ma1_0.3)
autoplot(ma1_1)
#As theta is changed, decreasing values causes more frequent fluctuations, increasing values causes less fluctuations moving towards a periodic shape.
#6e
arma11 <- function(phi, theta, sd=1, n=100){
  y2 <- ts(numeric(n))
  e2 <- rnorm(n, sd=sd)
  for(i in 2:n)
    y2[i] <- phi*y2[i-1] + theta*e2[i-1] + e2[i]
  return(y2)
}
armadata <- arma11(0.6, 0.6)
#6f
ar2 <- function(phi, theta, sd=1, n=100){
  y3 <- ts(numeric(n))
  e3 <- rnorm(n, sd=sd)
  for(i in 3:n)
    y3[i] <- phi*y3[i-1] + theta*y3[i-2] + e3[i]
  return(y3)
}
ardata <- ar2(-0.8, 0.3)
#6g
autoplot(armadata)
autoplot(ardata)
#The ARMA(1,1) plot looks to be a stationary time series with no seasonality. The AR(2) plot looks to be a non-stationary time series. It's a sinusoidal pattern with seasonality and the amplitude increasing exponentially with time.

#7
#7a
ggtsdisplay(wmurders)
ndiffs(wmurders)
wmdiff <- diff(diff(wmurders, lag = 1), lag = 1)
ggtsdisplay(wmdiff)
wmdiff %>% ur.kpss() %>% summary()
auto.arima(wmdiff)
#Taking a look at the results, the appropriate model is ARIMA(1,2,1)
#7b
#No, I would not include a constant in the model as d=2 which is greater than 1. The inclusion of a constant in the model would induce a polynomial trend of order 2 in the forecast function.
#7c
#(1?????1B)((1???B)^2)(yt)= c+(1+??1B)??t, as d=2, c=0 as there is no constant
#7d
(fit <- Arima(wmurders, order=c(1,2,1)))
checkresiduals(fit)
#The ACF of the residuals show the lags are within the 95% limits and the residuals is quite normally distributed. The model is satisfactory.
#7e
forecast(fit, h=3)
#7f
autoplot(forecast(fit, h=3))
#7g
auto.arima(wmurders)
#yes, auto.arima() gives the same model as the one chosen

#8
#8a
auto.arima(austa)
#ARIMA(0,1,1) with drift was selected
checkresiduals(auto.arima(austa))
autoplot(forecast(auto.arima(austa), h=10))
#8b
(fit <- Arima(austa, order=c(0,1,1)))
autoplot(forecast(fit, h=10))
(fit1 <- Arima(austa, order=c(0,1,0)))
autoplot(forecast(fit1, h=10))
#When the ARIMA(0,1,1) without drift model is plotted, the forecasts shows it becomes a horizontal line slightly above the last the value in austa, with the prediction intervals becoming wider. When the MA term is removed, the horizontal line shifts downwards to where the last value was observed and the prediction intervals becoming less wide.
#8c
(fit2 <- Arima(austa, order=c(2,1,3), include.drift = TRUE))
autoplot(forecast(fit2, h=10))
(fit3 <- Arima(austa, order=c(2,1,3), include.drift = FALSE))
#Error occurs when trying to remove the constant
#8d
(fit4 <- Arima(austa, order=c(0,0,1), include.constant = TRUE))
autoplot(forecast(fit4,h=10))
(fit5 <- Arima(austa, order=c(0,0,0), include.constant = TRUE))
autoplot(forecast(fit5,h=10))
#8e
(fit6 <- Arima(austa, order=c(0,2,1), include.constant = FALSE))
autoplot(forecast(fit6,h=10))

#9
#9a
autoplot(usgdp)
BoxCox.lambda(usgdp) 
usgdpbc <- BoxCox(usgdp,BoxCox.lambda(usgdp))
#9b
auto.arima(usgdpbc)
checkresiduals(auto.arima(usgdpbc))
autoplot(forecast(auto.arima(usgdpbc)))
#9c
(fit212 <- Arima(usgdpbc, c(2,1,2), include.drift = TRUE))
(fit012 <- Arima(usgdpbc, c(0,1,2), include.drift = TRUE))
(fit210 <- Arima(usgdpbc, c(2,1,0), include.drift = FALSE))
autoplot(forecast(fit212))
autoplot(forecast(fit012))
autoplot(forecast(fit210))
#9d
checkresiduals(fit012)
#9e
autoplot(forecast(fit012), h=20)
#Yes, the forecast of ARIMA(0,1,2) with drift looks reasonable. The prediction interval is tight and the forecast is following the upward trend.
#9f
fitets <- ets(usgdp)
autoplot(forecast(fitets))
checkresiduals(fitets)
#The ets() forecast results is less accurate compared to the ARIMA(0,1,2) with drift selected, this is found when looking at the ACF of the residuals of ets() model, as there are morre lags that lie outside the 95% limits.

#10
#10a
autoplot(austourists)
#The timeplot of austourists shows there is seasonality and a upward trend of international tourists traveling to Australia
#10b
ggtsdisplay(austourists)
#From the ACF graph, it can be seen that lags 1-12 and 16 lies outside of the 95% limits and that autocorrelation decreases.
#10c
#From the PACF graph, it can be seen that lags 1, 2, 4, 5 and 8 lies outside the 95% limits. A seasonality difference order of 1 with lag 8 should be considered for application.
#10d
ausdiff <- diff(austourists, differences = 1, lag = 4)
ggtsdisplay(ausdiff)
#The graphs suggest that ARIMA(1,1,0) should be used
#10e
auto.arima(austourists)
#No, auto.arima() suggests a different model. The model suggested is ARIMA(1,0,0)(1,1,0)[4] with drift, which is better.
#10f
#(1-B)(1-B^(4))yt = y(t) - y(t-1) - y(t-4) + y(t-3)

#11
#11a
autoplot(usmelec)
autoplot(ma(usmelec, order=12))
#There is an upward trend, except for the period of 1983 and 2009
#11b
#Yes, the data needs transforming, a BoxCox transformation will do
BoxCox.lambda(usmelec)
usmelecbc <- BoxCox(usmelec,BoxCox.lambda(usmelec))
autoplot(usmelecbc)
#11c
ggtsdisplay(usmelecbc)
ndiffs(usmelecbc)
nsdiffs(usmelecbc)
usmelecbcdiff <- diff(diff(usmelecbc),lag=12)
ggtsdisplay(usmelecbcdiff)
#11d
auto.arima(usmelec)
usmelec1 <- Arima(usmelec, order = c(1,1,0), seasonal = c(1,1,0))
usmelec1
usmelec2 <- Arima(usmelec, order = c(2,0,2), seasonal = c(2,1,2), include.drift = TRUE)
usmelec2
#The best model is ARIMA(2,0,2)(2,1,2)[12] with drift as it produces the smallest AIC value.
#11e
checkresiduals(usmelec2)
checkresiduals(auto.arima(usmelec))
#The ARIMA(2,0,2)(2,1,2)[12] with drift residuals do not resemble like white noise so I tried the auto.arima() which produces ARIMA(1,0,2)(0,1,1)[12] with drift. This model is abit better with only two lags lying outside the 95% limits in the ACF compared to three.
#11f
autoplot(forecast(auto.arima(usmelec), h = 180))
forecast(auto.arima(usmelec), h = 180)
#11g
#I think that the forecasts are sufficiently accurate up until 5 years ahead.

#12
#12a
autoplot(mcopper)
mcopperbc <- BoxCox(mcopper, BoxCox.lambda(mcopper))
autoplot(mcopperbc)
#12b
auto.arima(mcopperbc)
#12c
(fit <- Arima(mcopperbc, order = c(1,1,1)))
(fit1 <- Arima(mcopperbc, order = c(1,1,1), include.drift=TRUE))
#The best model is found to be ARIMA(1,1,1) with drift
#12d
checkresiduals(fit1)
#12e
autoplot(forecast(fit1))
#The forecasts do not look reasonable
#12f
autoplot(forecast(ets(mcopper)))
checkresiduals(ets(mcopper))
#I would say the ARIMA model I chose is better due to the residuals in the ACF graph, but both models are poor at forecasting.

#13
#13a
autoplot(hsales)
hsalesbc <- BoxCox(hsales,BoxCox.lambda(hsales))
autoplot(hsalesbc)
#13b
ggtsdisplay(hsalesbc)
auto.arima(hsalesbc)
#needs seasonal difference of order 1 with lag 12
#13c
(fit <- Arima(hsalesbc, order = c(0,0,1), seasonal = c(2,1,2)))
(fit1 <- Arima(hsalesbc, order = c(2,0,2), seasonal = c(1,1,0)))
#The best model with the smallest AIC value is ARIMA(1,0,0)(1,1,0)[12] with drift
#13d
checkresiduals(auto.arima(hsalesbc))
checkresiduals(fit)
checkresiduals(fit1)
#The best model is still ARIMA(1,0,0)(1,1,0)[12] with drift
#13e
autoplot(forecast(auto.arima(hsalesbc), h=24))
#13f
autoplot(forecast(ets(hsalesbc), h=24))
checkresiduals(ets(hsalesbc))
#Looks like the ets() model is better as the forecast has a tighter prediction interval and the ACF graph of the residuals only have one lag that is beyond the 95% limits whereas the ARIMA model has two lags.

#14
hsalesstlf = stlf(hsales, BoxCox.lambda(hsales), s.window = 5, robust = TRUE, method = "arima",h = 24)
autoplot(hsalesstlf)
checkresiduals(hsalesstlf)
#The forecasts from the stlf() shows a tighter prediction interval with lower peaks compared to the ets() model. However, I think ets() model is still better as the ACF graph for the residuals of the stlf() model shows four lags that are outside the 95% limits compared to only one lag for ets() model.