#Chapter 5 exercises
#1
library(fpp2)
library(ggplot2)
daily20 <- head(elecdaily,20)

#1a
autoplot(daily20)
daily20 %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) +
    ylab("Demand") +
    xlab("Temperature") +
    geom_point() +
    geom_smooth(method="lm", se=FALSE)
elec <- tslm(Demand~Temperature, data=daily20)
elec
#There is a positive relationship between Demand and Temperature because when Temperature gets hotter, electricity usage will go up for cooling such as ACs.The regression model found is Demand = 39.212 + 6.757*Temperature

#1b
checkresiduals(elec)
#The model is adequate as there seems to be no significant correlation in the residuals series. Also the histogram of the residuals is not normal and skewed.

#1c
fcast <- forecast(elec, newdata=data.frame(Temperature=c(15,35)))
fcast
#The regression model gives an estimate of 140.567 for 15 degree and 275.707 degree when manually calculated. The forecasts are believable as it comes to close to when calculated using the regression model.

#1d
autoplot(daily20, facets=TRUE)
daily20 %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
fit <- tslm(Demand ~ Temperature, data=daily20)
checkresiduals(fit)
forecast(fit, newdata=data.frame(Temperature=c(15,35)))
#The prediction intervals for electricity demand when temperature is 15 degrees at 80% prediction intervals is 108.6810 to 172.4591 and at 95% prediction intervals is 90.21166 to 190.9285. When temperature is 35 degrees, the 80% prediction intervals is 245.2278 to 306.2014 and at 95% prediction intervals is 227.57056 to 323.8586.

#1e
autoplot(elecdaily)
elecdaily %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) +
  ylab("Demand") +
  xlab("Temperature") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
#The model used too little data from the original elecdaily, which caused it to overestimate the electricity demand.

#2

#2a
autoplot(mens400, xlab="Year", ylab="Winning Times(in seconds)", main="Men's 400 meters final Olympic Games")
#As the years increased, Men's 400 meter final times(in seconds) have been generally decreasing(getting faster). Some data is missing due to the Olympics not running during World War 1 and 2.

#2b
timemens <- time(mens400)
mens400.model <- tslm(mens400 ~ timemens, data=mens400)
mens400.model
mens400 %>%
  as.data.frame() %>%
  ggplot(aes(x=timemens, y=mens400)) +
  ylab("Winning Times(in seconds)") +
  xlab("Year") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
#The average rate of winning times decreasing is found to be -0.06457 seconds per year

#2c
checkresiduals(mens400.model)
#The residuals indicated that the fitted line generally fits the data

#2d
forecast(mens400.model, newdata=data.frame(timemens=2020))
#The prediction is that the winning men's 400 final time for 2020 Olympics is 42.04 seconds. The 80% prediction interval is 40.44975 to 43.63487 and the 95% prediction interval is 39.55286 to 44.53176.

#3
easter(ausbeer)
help(easter)
help(ausbeer)
#The time period of this dataset runs from the 1st quarter of 1956 to the 2nd quarter of 2010 and contains quarterly Australian beer production. The easter function returns a vector of 0's and 1's or fractional results if Easter spans March and April in the observed time period. Easter is defined as the days from Good Friday to Easter Sunday inclusively, plus optionally Easter Monday if easter.mon=TRUE.

#4
#log(y) = B0 + B1*log(x) + e
#d(log(y)) = B1*d(log(x))
#(1/y)dy = B1*(1/x)dx
#B1 = dy/dx(x/y)
#This shows that B1 is the elasticity coefficient as it reflects the ratio of the percentage change in y to the percentage change in x

#5
#5a
autoplot(fancy)
#The patterns in the data show there is always a sharp increase in sales during the Christmas season. Every March also sees a slight increase in sales, to which a small decline follows that gradually increases until the sharp spike during Christmas season. The Christmas peak sales of each year becomes higher than the previous one each year except for 1991. From 1992 onwards, the Christmas sales peak seems to be doubling from the previous year.

#5b
#Due to the seasonal variations in the data, it is necessary to take logarithms of these data as we want to see the proportional percentage change in the sales.

#5c
fancytime <- time(fancy)
surfingfestival <- c()
for(i in 1:length(fancytime)){
  month <- round(12*(fancytime[i] - floor(fancytime[i]))) + 1
  year <- floor(fancytime[i])
  if(year >= 1988 & month == 3){
    surfingfestival[i] <- 1
  } else {
    surfingfestival[i] <- 0
  }
}
fancymodel <- tslm(BoxCox(fancy, 0) ~ trend + season + surfingfestival)
fancymodel

#5d
checkresiduals(fancymodel)
#The residuals plot show that the residuals are following a pattern as time increases. The residuals are correlated with time which poses a problem.

#5e
cbind.data.frame(
  Month = factor(
    month.abb[round(12*(fancytime - floor(fancytime)) + 1)],
    labels = month.abb,
    ordered = TRUE
  ),
  Residuals = fancymodel$residuals
) %>%
  ggplot(aes(x = Month,
             y = Residuals)) +
  geom_boxplot()
#The box plots reveal that there may be a cyclical pattern of a waveform, this indicates there may be missing data that is not accounted for.

#5f
summary(fancymodel)
#all the coefficients are positive and as the season increases, so too does the coefficient value. Sales are increasing as time increases. Each month of sale is greater than the same month of the previous year. The values is also telling the percentage increase in sales.

#5g
checkresiduals(fancymodel)
#The Breusch-Godfrey test gives a p-value of 0.002494. This is smaller than 0.05, meaning the residuals suffer from autocorrelation.

#5h
fancyrange <- ts(data=fancytime, start=1994, end=c(1996,12), frequency=12)
forecast(fancymodel, fancyrange)

#5i
fancytran <- (BoxCox(fancy, 0))
forecast(fancytran)

#5j
#The predictions could be improved by using a more effective method for transformation such as using the lambda function. This method would better capture the exponential growth in the trend for the sales of the shop.

#6
#6a
gas2004 <- window(gasoline, end = 2005)
autoplot(gas2004, xlab="Year", ylab="million barrels per day", main="US finished motor gasoline product")
fourier.gas1 <- tslm(gas2004 ~ trend + fourier(gas2004, K=1))
fourier.gas2 <- tslm(gas2004 ~ trend + fourier(gas2004, K=2))
fourier.gas3 <- tslm(gas2004 ~ trend + fourier(gas2004, K=5))
fourier.gas4 <- tslm(gas2004 ~ trend + fourier(gas2004, K=10))
fourier.gas5 <- tslm(gas2004 ~ trend + fourier(gas2004, K=20))
autoplot(gas2004, ylab = "Gas Supply(Weekly)", main = "Fourier Transformation") + autolayer(fitted(fourier.gas1)) + 
  autolayer(fitted(fourier.gas2)) + autolayer(fitted(fourier.gas3)) + autolayer(fitted(fourier.gas4)) + autolayer(fitted(fourier.gas5))

#6b
CV(fourier.gas1)
CV(fourier.gas2)
CV(fourier.gas3)
CV(fourier.gas4)
CV(fourier.gas5)
#model 4 with k=10 is the appropriate number of Fourier terms to include as it has both the smallest CV and AICc value.

#6c
checkresiduals(fourier.gas4)

#6d
fc <- forecast(fourier.gas4, newdata=data.frame(fourier(gas2004,10,52)))
fc

#6e
gas2005 <- window(gasoline, start=2005, end=2006)
autoplot(fc, series = "Forecast") + autolayer(gas2005) + scale_x_continuous(limits = c(2005, 2006))
#The forecast is able to generally fit the actual data, information is retained within the 80% prediction interval but the forecast is unable to predict the sharp drop found close to the 3rd quarter of 2005.

#7
#7a
autoplot(huron)
#There seems to be a negative trend between the water level of Lake Huron as the year increases. However, it is hard to see what the actual relationship is.

#7b
huronmodel <- tslm(huron ~ trend)
huronmodel
hurontime <- time(huron)
huron1915 <- 1915
huronpw <- ts(pmax(0,hurontime-huron1915), start=1875)
huronmodelpw <- tslm(huron ~ hurontime + huronpw)

#7c
forecast(huronmodel, h=8)
t.new <- hurontime[length(hurontime)] + seq(8)
t_piece_new <- huronpw[length(huronpw)]+seq(8)
newdata <- cbind(hurontime=t.new,
                 huronpw=t_piece_new) %>%
  as.data.frame()
forecast(huronmodelpw,newdata = newdata)
#

#Chapter 6 Exercises
#1
#A 3x5MA is when a moving average of order 5 is taken and then a moving average of order 3 is applied to it. The 3x5MA can be written as ((Y1 + Y2 + Y3 + Y4 + Y5)/5 + (Y2 + Y3 + Y4 + Y5 + Y6)/5 + (Y3 + Y4 + Y5 + Y6 + Y7)/5)/3
#which results in (Y1 + Y2 + Y3 + Y4 + Y5)/15 + (Y2 + Y3 + Y4 + Y5 + Y6)/15 + (Y3 + Y4 + Y5 + Y6 + Y7)/15 = Y1/15 + 2Y2/15 + Y3/5 + Y4/5 + Y5/5 + 2Y6/15 + Y7/15, which is equivalent to a 7-term weighted moving average with weights 1/15,2/15,1/5,1/5,1/5,2/15,1/15.

#2
#2a
autoplot(plastics, xlab="Year", ylab="Sales(in thousands)", main="Product A Sales")
#From the plot, it can be seen there is seasonal fluctuations of the sales of product A, the peaks occur roughly around the summer months of year and bottom out around the end of the year. Overall, there is a general upward trend, as in the peaks increase every year such that it is higher than the previous year.

#2b
plastics %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Year") +
  ggtitle("Classical multiplicative decomposition
    of Product A")

#2c
#Yes the results support the seasonal trend of peaking during the summer months and bottom out around the end of the year. It also shows there is a trend of increasing sales with time.

#2d
plastmul <- decompose(plastics, type="multiplicative")
autoplot(seasadj(plastmul), xlab="Year", ylab="Sales(in thousands)", main="Seasonal Adjusted Product A Sales")

#2e
plast <- plastics
plast[30] = plast[30]+500
plastmul <- decompose(plast, type="multiplicative")
autoplot(seasadj(plastmul), xlab="Year", ylab="Sales(in thousands)", main="Seasonal Adjusted Product A Sales")
#The outlier effectively adds a peak to the seasonally adjusted sales of Product A.

#2f
plast[60] = plast[60]+500
plastmul <- decompose(plast, type="multiplicative")
autoplot(plastmul, xlab="Year", ylab="Sales(in thousands)", main="Product A Sales")
autoplot(seasadj(plastmul), xlab="Year", ylab="Sales(in thousands)", main="Seasonal Adjusted Product A Sales")
#There is a great effect of the outlier in the middle of the time series on the seasonal and trend component than if it were just at the end of the timeseries.

#3
retaildata <- readxl::read_excel("retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349873A"],
           frequency=12, start=c(1982,4))
library(seasonal)
myts %>% seas(x11="") -> fit
autoplot(fit) +
  ggtitle("X11 decomposition of retail time series data")
#From the X11 decomposition irregular plot, it shows there are outliers represented by the big spikes going up or down. An example is near the year 2000, there is a huge spike going up but when taking a look at the trend, data or seasonal plots, it isn't obvious there are outliers.

#4
#4a
#Looking at the decomposition plots, the trend plot shows there is an upward trend in the number of persons in the civilian labour force as time increases.
#The remaindor plot shows there are some significant outliers around the 1991-1992 year, which caused the trend to level out abit and a dip in the data plot.
#The seasonal component broken down to the months of the year shows that peak amount of persons in the labour force happen during March and December, also that the labour force loses participants during January and August.

#4b
#Yes, the recession of 1991/1992 is visible in the estimated components, particularly in the remainder plot where it shows big downward spikes. This is reflected in the trend plot leveling out and the biggest dip in the data plot.

#5
#5a
autoplot(cangas)
ggsubseriesplot(cangas)
ggseasonplot(cangas)
#The time-series plot indicates there is an upward trend of Canadian gas production as time increases and that there is a strong seasonality effect.
#Looking at the subseries plot, it can be observed that gas production is highest at the beginning and end of the year. From the same plot, gas production is seen to be lowest during the summer months.
#Looking at the seasonal plot, there is changing seasonality over time which differs from the subseries plot.
#As can be seen from the seasonal plot, the years after 1970experience a ramp up in production of gas during the summer months starting June. This could be due to trying to meet an increase in demand for gas or cheaper production for gas companies during certain months.

#5b
cangas %>%
  stl(t.window=13, s.window="periodic", robust=TRUE) %>%
  autoplot() +
  ggtitle("STL decomposition of Canadian Gas Production")

#5c
cangas %>% seas(x11="") -> fit
autoplot(fit) +
  ggtitle("X11 decomposition of Canadian  Gas Production")
cangas %>% seas() %>%
  autoplot() +
  ggtitle("SEATS decomposition of Canadian Gas Production")
#The main difference found between all three compositions is the seasonal plot and the remainder plots. The seasonal plot of X11 and SEATs are similar in shape while the STL seasonal plot is very different in shape.
#The remainder plot of all three have different scales but X11 and SEATS are similar enough whereas the STL remainder plot shows different outliers at different time periods. The STL decomposition is most suited for the cangas data.

#6
#6a
bricksq %>%
  stl(t.window=21, s.window=9, robust=TRUE) %>%
  autoplot()

#6b
brickmodel <- stl(bricksq, t.window=21, s.window=9, robust=TRUE)
autoplot(seasadj(brickmodel),xlab="Year",ylab="Clay Brick Production", main="Seasonally Adjusted Australian Quarterly Clay Brick Production")

#6c
fcast <- forecast(seasadj(brickmodel), method='naive')
fcast
autoplot(fcast)

#6d
fcast1 <- stlf(bricksq, method='naive')
fcast1
autoplot(fcast1)

#6e
checkresiduals(fcast)
checkresiduals(fcast1)
#The residuals are approximately normally distributed and look correlated

#6f
fcast1 <- stlf(bricksq, method='naive', robust=TRUE)
fcast1
autoplot(fcast1)
checkresiduals(fcast1)
#Yes, a robust STL decomposition makes a difference, as now the residuals dont look as normally distributed which should indicate less correlation.

#6g
brick2 <- window(bricksq, start=1992, end=1994)
fcast2 <- stlf(brick2, method='naive', robust=TRUE)
fcast3 <- snaive(brick2)
fcast2
fcast3
autoplot(fcast2)
autoplot(fcast3)
#The stlf() method looks better as it produces a tighter prediction interval for the forecasts.

#7
autoplot(writing)
stlf(writing, method='naive', robust=TRUE)
stlf(writing, method='rwdrift', robust=TRUE)
stlf(writing, method='naive', robust=TRUE, lambda = BoxCox.lambda(writing))
stlf(writing, method='rwdrift', robust=TRUE, lambda = BoxCox.lambda(writing))
autoplot(stlf(writing, method='naive', robust=TRUE))
autoplot(stlf(writing, method='rwdrift', robust=TRUE))
autoplot(stlf(writing, method='naive', robust=TRUE, lambda = BoxCox.lambda(writing)))
autoplot(stlf(writing, method='rwdrift', robust=TRUE, lambda = BoxCox.lambda(writing)))
#The best forecast for the writing dataset seems to be the rwdrift method without the Box-Cox transformation

#8
autoplot(fancy)
stlf(fancy, method='naive', robust=TRUE)
stlf(fancy, method='rwdrift', robust=TRUE)
stlf(fancy, method='naive', robust=TRUE, lambda = BoxCox.lambda(writing))
stlf(fancy, method='rwdrift', robust=TRUE, lambda = BoxCox.lambda(writing))
autoplot(stlf(fancy, method='naive', robust=TRUE))
autoplot(stlf(fancy, method='rwdrift', robust=TRUE))
autoplot(stlf(fancy, method='naive', robust=TRUE, lambda = BoxCox.lambda(writing)))
autoplot(stlf(fancy, method='rwdrift', robust=TRUE, lambda = BoxCox.lambda(writing)))
#The best forecast for the fancy dataset seems to be the rwdrift method with the Box-Cox transformation