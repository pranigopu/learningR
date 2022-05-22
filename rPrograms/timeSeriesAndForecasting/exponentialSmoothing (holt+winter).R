#' REQUIRED LIBRARIES
#------------
# ASTSA => Applied Statistical Time Series Analysis
# install.packages("astsa")
library(astsa)

# install.packages("forecast")
library(forecast)

# install.packages("tseries")
library(tseries)
#========================
#' INTRODUCTION & AIM
#------------
#' Forecasting future observations is the main aim of time series analysis.
#' Modelling the deterministic components such as trend and periodicity are key in this endeavour,
#' although future forecasts will almost always be subject to some random error.
#' Smoothing involves taking averages of past values to predict future values.
#' Exponential smoothing involves taking a weighted average of past values to predict future values.
#' Here, more recent observations are given more weightage, and weights decrease as we go to more previous observations.
#' In this assignment, our aim is to use Holt's and Winter's exponential smoothing methods to create
#' models for our time series, in order to predict five future observations.
#========================
#' DATA
#------------
#' Importing necessary data
setwd("~/Documents/Study/computerScience/programming/r/data/")
data = read.csv("agriculturalRawMaterial.csv")[c(1, 4)]
head(data)
#------------
#' Defining and formatting the 'Month' variable
t = c()
for(x in data$Month)
{
  x = paste("01", x, sep = "-")
  t = c(t, x)
}
t = as.Date(t, format = "%d-%b-%y")
# %b => abbreviated month
# %y => 2 digit year
# It can recognize century on its own.
# So for example, '93' will be interpreted as '1993'.
#************************************
#' HOLT-WINTER EXPONENTIAL SMOOTHING
#========================
#' Defining and reformatting the 'Copra.Price' variable
z = c()
for(p in data$Copra.Price)
{
  p = gsub(',', '', p)
  # Removing commas in the numbers
  # 1st argument => what to replace
  # 2nd argument => what to put instead
  # 3rd argument => full string
  z = c(z, p)
}
z = as.numeric(z)
#------------
#' Summarizing the data
df = data.frame(t, z)
summary(df)
#' We can see there are 22 missing values in the price column.
#' Checking in the dataset itself, values for copra prices are available
#' until a certain point, after which we have these missing values.
#' Hence, we can simply remove the tail end of the dataset where these
#' missing values are concentrated.
t_new = t[c(1:(length(t) - 22))]
z_new = z[c(1:(length(z) - 22))]
df = data.frame(t_new, z_new)
summary(df)
#------------
#' Creating a time series for softlog prices
Z = ts(z_new, start = c(1990, 4, 1), end = c(2018, 6, 1), frequency = 12)
# frequency = 12 -> monthly frequency
#========================
#' TIME PLOT AND POTENTIAL TIME SERIES COMPONENTS
#------------
#' General function to create time plots with specified intervals
timeplot = function(min, max)
{
  ts.plot(Z,
          main = paste("Copra prices: ", min, "-", max, sep = ''),
          ylab = "Price",
          xlab = "Year",
          xlim = c(min, max),
          col = "darkgreen")
}
#------------
#' Time plots for different intervals of time
#______
# To study the graphs more closely for recognizing time series components on an annual level...
timeplot(1990, 2018)
timeplot(1990, 1997)
timeplot(1997, 2004)
timeplot(2004, 2011)
timeplot(2011, 2018)
#' Based on the time plots, we may conclude that there is no clear seasonality (i.e. no periodic fluctuations annually).
#' There seems to be an overall upward trend over time, with a seemingly high level of irregular fluctuations.
#' We may also observe a discernible long term fluctuation pattern over a around a year period,
#' hence we may conclude that there is some cyclical fluctuation.
#' However, the major components of this time series seem to be trend and irregular fluctuations.
#========================
#' TESTING STATIONARITY OF TIME SERIES
#------------
#' Performing the test using the 'tseries' library
#______
#' Original time series stationarity
adf.test(Z)
#' Observing ACF plot
acf(Z)
#========================
#' FORECASTING MODEL CREATION
#------------
#' In our time plots, we have observed no seasonal component.
#' Hence, the exponential smoothing model appropriate for our time series is Holt's exponential smoothing,
#' which applies for time series without seasonality.
#' To obtain Holt's method using the HoltWinters function, you must give the argument 'gamma = FALSE'.
#' Otherwise, it will apply Winter's method of exponential smoothing.
model = HoltWinters(Z, gamma = FALSE)
print(model)
#' The constants alpha and beta are smoothing factors present in the model.
ts.plot(model$fitted)
#========================
#' FORECASTING USING ABOVE MODEL
#------------
forecastedData = forecast(model, h = 5)
#' The above command uses the exponential smoothing model we created for our data
#' to forecast the following five observations i.e. for the next five months.
forecastedData
#' Plotting the forecasted data
plot(forecastedData,
     main = "Past data + forecasts for the next 5 monthts",
     xlab = "Year",
     ylab = "Price")