#' INITIAL DATA PROCESSING
setwd("~/Documents/Study/computerScience/programming/r/data/")
data = read.csv("dailyTotalFemaleBirths.csv")
#' Viewing data
head(data)
#' Converting date strings to dates
data$Date = as.Date(data$Date)
#' Summarisation
summary(data)
#' The data is taken for the whole year, daily.
#========================
#' CONVERTING TO TIME SERIES
z = ts(data$Births, start=c(1959, 1), end=c(1959, 365), frequency=365)
# frequency=365 => 1 time unit divided into 365 partitions.
# Here, 1 time unit is 1 year.
# start = c(1959, 1) => starting from the 1st partition of the year 1959.
# end = c(1959, 365) => ending at the 365th partition of the year 1959.
ts.plot(z,
        main="Daily total female births",
        xlab="Day",
        ylab="Births")
#========================
#' CHECKING STATIONARITY
library(tseries)
adf.test(z, alternative="stationary")
#' From the augmented Dickey-Fuller test, we observe that
#' the time series can be said to be stationary for upto 7 lags
#' i.e. observations display stationarity when considering observations
#' separated by at most 7 lags from each other.
#------------
acf(z,
    main="ACF plot", 100)
pacf(z,
     main="PACF plot", 100)
#' From ACF, we can observe largely insignificant autocorrelation between
#' observations separated by upto 25 lags. The occasional significant aucorrelations
#' may be chalked down to error.
#______
#' Even from the PACF, we can observe largely insignificant autocorrelation between
#' observations separated by upto 25 lags. This indicates that even after correcting
#' for the effects of the lags between two lagged observations, the autocorrelation between
#' observations is largely insignificant.
#______
#' Both of the above, along with the fact that the data seems to display
#' constant mean and finite variance, suggests moderate to strong stationarity.
#========================
#' SUITABLE ARMA MODEL
#------------
#' Given the largely insignificant autocorrelation between observations,
#' autoregression may be entirely absent from this time series process.
#' Due to this, and due to its moderate to strong stationarity, we conclude
#' that a moving average model may be more suitable for this time series.
#' Hence, for the time series process model ARIMA(p, d, q), we put p = 0, d = 0, q = 1.
library(forecast)
#' Creating a MA(1) model...
model = auto.arima(z,
           max.p=0,
           max.d=0,
           start.q=2,
           max.q=1)
summary(model)
#' Plotting fitted values
ts.plot(model$fitted,
        main="Fitted MA(1) model",
        xlab="Day",
        ylab="Births")
#========================
#' FITTING BEST NON-STATIONARY MODEL
#------------
#' Creating an ARIMA(p, d, q) model...
nonStationaryModel = auto.arima(z)
summary(nonStationaryModel)
#' Plotting fitted values
ts.plot(nonStationaryModel$fitted,
        main="Fitted ARIMA model",
        xlab="Day",
        ylab="Births")
#========================
#' PREFERRED MODEL
#------------
#' Our preferred model would be MA(1), because not only does it
#' visually match the time plot of the data better, but also, we
#' are note preforming any differencing to make the data stationary,
#' since the time series was already concluded to be sufficiently stationary.
#' However, the fitted non-stationary model does not consider this conclusion,
#' and performs differencing of order 1 on the time series, which creates
#' discrepancies between the actual data and the fitted values.
#========================
#' TESTING MODEL ADEQUACY
#------------
residuals = model$residuals
#' Checking mean of residuals
mean(residuals)
#' Testing if the true mean of the residuals can be said to be 0
# (true mean => mean of the residuals in general, considering our residuals as a sample)
t.test(residuals, mu=0)
#' We may accept null hypthesis, given significance level = 0.05.
#' Hence, we may conclude that true mean of residuals= 0
#------------
#' Testing autocorrelation
Box.test(residuals)
#' We may accept null hypthesis, given significance level = 0.05.
#' Hence, we may conclude that autocorrelation residuals= 0
#------------
#' Testing normality
shapiro.test(residuals)
#' We may reject null hypthesis, given significance level = 0.05.
#' Hence, we may conclude that residuals are not normally distributed.
#------------
#' Due to non-normal distribution of the residuals, we may assume that
#' the residuals may be caused by more than random error, hence indicating that
#' our model may not be the fittest possible model for our data.
#========================
forecasted = forecast(model, h=20)
plot(forecasted,
     main="Fitted MA(1) model + forecasts")
#' Printing the next 20 forecasted values (mean forecasted values)
for(f in forecasted$mean){print(f)}
