#' Consider a suitable dataset for AR, MA or ARMA model.
# 1
#' Identify the model by examining the ACF and PACF plot of the stationary data
# 2
#' Fit the model using auto.arima and show that the fitted model is best
#************************************
#' CREATING THE TIME SERIES
#========================
#' DATASET
#------------
#' Importing necessary data
data = read.csv("~/Documents/Study/computerScience/programming/r/data/agriculturalRawMaterial.csv")
head(data)
summary(data)
#' We notice 34 NA values for soft sawnwood prices.
#' Inspecting the dataset, we see that these
#' NA values are grouped at the tail-end of the column.
#' Hence, we can simply remove the last 34 rows of the columns.
prices = data$Soft.sawnwood.Price[c(1:(length(data$Month) - 34))]
months = data$Month[c(1:(length(data$Month) - 34))]
#------------
#' Defining and formatting the 'Month' variable
#' (for better date summary)

# Obtaining list of suitable date strings
t = c()
for(x in months)
{
  x = paste("01", x, sep = "-")
  t = c(t, x)
}
# Convering list of date strings to list of date objects
# if our format were YYYY-MM-DD (all digits), we need not have specified the format, since this format is default.
# However, this is not the case with our data.
t = as.Date(t, format = "%d-%b-%y")
# %b => abbreviated month
# %y => 2 digit year
# It can recognize century on its own.
# So for example, '93' will be interpreted as '1993'.
#------------
#' Summarising dates
summary(t)
#========================
#' CONVERTING TO TIME SERIES
Z = ts(prices, start = c(1990, 4, 1), end = c(2017, 6, 1), frequency = 12)
# frequency = 12 -> monthly frequency
#************************************
#' MAKING TIME SERIES STATIONARY
#========================
#' TIME PLOT AND POTENTIAL TIME SERIES COMPONENTS
#------------
ts.plot(Z,
        main = "Prices: 1990-2020",
        ylab = "Price",
        xlab = "Year",
        col = "darkgreen")
#' Based on the time plot, we may conclude that there is no clear seasonality (i.e. no periodic fluctuations annually).
#' We shall confirm this more rigorously using the ADF test.
#' There seems to be an overall upward trend over time, with a seemingly high level of irregular fluctuations.
#' We also see no discernible long term fluctuation pattern,
#' hence we may conclude that there is no cyclical fluctuation.
#' Hence, the major components of this time series seem to be trend and irregular fluctuations.
#========================
#' TESTING THE STATIONARITY OF THE ORIGINAL TIME SERIES
#------------
#' ADF test
#______
#' ADF test is used to check the stationarity of a time series.
#' Its null hypothesis is that there is the data is not stationary,
#' and its alternative hypothesis is that the data is stationary.
#' As with all statistical tests, the p-value indicates whether we may or may not reject the null hypothesis.
#' Only if it is lower than the level of significance (in our case, this is 5% or 0.05) may we reject the null hypothesis
#' with the given level of confidence.
#------------
#' Performing the test using the 'tseries' library
library(tseries)
#' Testing for stationarity
adf.test(Z)
#' Observing ACF plot
acf(Z)
#' Based on the ACF plot, we may conclude that there is no significant seasonality
#' in the time series. Given that the p-value for the ADF test is 0.09756 > 0.05,
#' (where 0.05 is the level of significance)
#' we may accept the null hypothesis that the time series is
#' not stationary, with a 95% confidence. Hence, we may conclude that the time series is not stationary.
#' Given the seasonal component is not significant, and given that there is no string indication of
#' long-term fluctuations, we may conclude that the trend component is significant.
#========================
#' HOW WE WOULD ELIMINATE TREND
#' (and why we need not do so in this case)
#------------
#' Based on the above conclusions, we will use the model z_t = s_t + e_t, where
#' z_t is the softlog price at time t,
#' s_t is the seasonal component at time t,
#' e_t is the error at time t (irregular fluctuations).
#' We assume there to be a constant downward trend component.
#------------
#' To use the method of differencing to remove trend and seasonality, we use the 'diff' function.
#' To remove trend, we use lag = 1 (the default lag). Since we concluded that the seasonality
#' us not significant, we will not perform differencing for it.
#
#' In our case, however, since we are using the 'auto.arima'
#' function from the 'forecast' library of R, we do not
#' need to eliminate trend and differencing on our own, since the function does the appropriate
#' differencing operations based on statistical tests that determine the significance of
#' the trend and seasonality. This fact is mentioned in the documentation of
#' this function. However, we will still eliminate trend...
Z1 = diff(Z)
ts.plot(Z1,
        main = "Prices without trend",
        ylab = "Price",
        xlab = "Year",
        col = "darkgreen")
#========================
#' TESTING STATIONARITY OF NEW TIME SERIES
#------------
#' Performing the ADF test using the 'tseries' library
#______
#' Testing for stationarity
adf.test(Z1)
#' Observing ACF plot
acf(Z1)
#' Based on the ACF plot, we can see that for all lags other than 1, the
#' autocorrelation coefficient is not significant, indicating stationarity.
#' This is supported by the ACF, whose p-value is below 0.05
#' (where 0.05 is the level of significance).
#************************************
#' FITTING THE APPROPRIATE STATIONARY MODEL
#========================
#' For this purpose, we will be making use of ARIMA models, which are briefly explained below...
#------------
#' ARIMA => Autoregressive Integrated Moving Average
#______
#' ARIMA is a generalization of AR models, MA (moving average) models
#' and I (integrated) models. These components are explained below:
# 1
#' AR (Autoregression): Current values depend on some number of
#' past observations upto a certain lag.
# 2
#' I (Integrated): A time series is said to be integrated of order d
#' if taking repeated differences between z_t values results in a
#' stationary process. For example, if z_t is integrated of order 2,
#' then z_t - z_(t-1) - z_(t-2) is a stationary process.
#' Here, 'integrated' refers the property of the time series being
#' integrated at some order, i.e. it refers to time series that are
#' non-stationary with respect to mean, but can become stationary
#' after some number of repeated differencing.
# 3
#' MA (Moving Average): The value at each time point is smoothened
#' by averaging some number of past and future values around it.
#______
#' ARIMA model has three integer components, namely:
# 1
#' Order of autoregression of the time series (p). This indicates
#' the maximum lag at which observations are included i.e. extent to
#' which past values are used to model current values.
# 2
#' Order of integration of the time series (d). This
#' specifies the non-seasonal (i.e. trend) part of the
#' model.
# 3
#' Order of moving average of the time series (q). This indicates the
#' number of values around and including the current time point's
#' value that are averaged, in order to smoothen the current time
#' point's value. For example, MA(3) implies that each time point's
#' value is replaced by the average of the just preceding, current
#' and just succeeding value.
#______
#' ARIMA models are denoted as ARIMA(p, d, q). Note that here,
#' seasonality is not considered, only trend. To include the
#' seasonal component, we must use seasonal ARIMA, which is beyond the
#' scope of this assignment.
#========================
library(forecast)
#------------
#' We will not only be fitting an appropriate ARIMA model for our dataset(s),
#' but also trace the models that the function has considered.
#' This is to demonstrate that the function is intended to search for the best
#' possible ARIMA model (with the most suitable orders of autoregression (p), integration (d) and moving average(q))
#------------
#' Also, since our focus is on ARMA models in particular, we will be limiting
#' the integration component of the ARIMA model to 0, by using the argument
#' max.d = 0.
#------------
#' Fitting the stationary dataset we have obtained...
auto.arima(Z1, max.d = 0, seasonal = "FALSE", trace = TRUE)
#' To show that the data is made stationary if it is not, we shall also fit
#' the original time series data...
auto.arima(Z, max.d = 0, seasonal = "FALSE", trace = TRUE)
#' At least in this case, there is very little difference between
#' the estimated values of the models, whether you use the original
#' dataset, or a stationary dataset derived from the original
#' non-stationary dataset.