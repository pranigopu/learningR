data = read.csv("~/Documents/Study/computerScience/programming/r/data/monthlyBeerProductionIndia.csv")
head(data)
summary(data)
#========================
#' DATA HANDLING
#------------
# Checking first and last dates
data$Month[1]
data$Month[length(data$Month)]

# Converting to time series
z = ts(data$Monthly.beer.production, start = c(1956, 1, 1), end = c(1995, 8, 1), frequency = 12)
#========================
#' BASIC ANALYSIS
#------------
#' Time plot
ts.plot(z,
        main="Monthly beer production in India",
        xlab="Time",
        ylab="Production")
#' From the data, there can be said to be an overall upward trend,
#' although this is not linear nor constantly increasing, as it stagnates and starts
#' dipping after around 1975. There is no sign of cyclic fluctuation yet, but given a larger dataset,
#' we may observe it. Irregular fluctuations seem to be limited, and the main source of
#' fluctuation seems to be periodic (in a smaller scale than seasonal periodicity).
#------------
#' ACF plot
acf(z, main="ACF")
acf(z, main="ACF", 100)
acf(z, main="ACF", 1000)
#' Hence, from the ACF, due to the ACF being a periodic function of the lag 1, we observe
#' autoregression.
#========================
#' MAKING DATA STATIONARY
#------------
#' Differencing
# Making time series stationary using differencing
z_notrend = diff(z, 1)
z_noseasonality = diff(z, 1)
z_stationary = diff(diff(z, 12), 1)
ts.plot(z_notrend,
        main="No trend",
        xlab="Time",
        ylab="Production")
ts.plot(z_noseasonality,
        main="No seasonality",
        xlab="Time",
        ylab="Production")
ts.plot(z_stationary,
        main="Stationary data",
        xlab="Time",
        ylab="Production")
#------------
#' Exponential smoothing
# Making time series stationary using Winter's exponential smoothing
smoothed = HoltWinters(z, gamma = TRUE)
ts.plot(smoothed$fitted,
        main="Smoothed data")
#========================
#' IDENTIFYING AND FITTING SUITABLE STATIONARY TIME SERIES MODEL
#------------
#' Model shows trend and seasonality. Seasonality implies a strong degree of autocorrelation in the data.
#' From the ACF of the data, we observe significant autocorrelation for observations between the every 1 lag.
#' Hence, an AR(1) model will be most suitable.
#------------
#' Trying to fit ARMA(1, 1)
library(forecast)
model = auto.arima(z,
           max.d = 0,
           start.p = 1,
           max.p = 1,
           start.q = 1,
           max.q = 1,
           seasonal = TRUE)
summary(model)
plot(model$fitted)
# Order of integration i.e. order of differencing = 0
# Order oving average = 1
# Order of autoregression = 1

#' This model seems to fit the data closely. ARMA(1, 1) is fairly close to the
#' suggested model of AR(1), hence this may not be surprising.
#------------
#' Identifying the best model
bestModel = auto.arima(z,
                   max.d = 0,
                   seasonal = TRUE)
summary(bestModel)
plot(bestModel$fitted)
#' We can see that the information criteria are similar to the model fitted for ARMA(1, 1).
#' Hence, we can say that ARMA(1, 1) is suitable model for our time series.
#------------
#' Forecasting next 20 points
forecasted = forecast(bestModel, h = 20)

print("Next 20 data points forecasted:")
forecasted

plot(forecasted,
     main = "Past data + forecasts for the next 20 monthts",
     xlab = "Year",
     ylab = "Price")