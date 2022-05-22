#' NEED FOR ELIMINATION OF TREND AND SEASONALITY
#------------
#' The elimination of trend and seasonality in the data is done in order to make the time series stationary.
#' This makes it easier to analyse and model.
#========================
#' DATA FOR BOTH QUESTIONS
#------------
#' Importing necessary data
setwd("~/Documents/Study/computerScience/programming/r/data/")
data = read.csv("agriculturalRawMaterial.csv")[c(1, 18, 20)]
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
#' PART 1: REMOVING TREND FROM DATA ON SOFTLOG PRICES
#========================
#' Defining and reformatting the 'Softlog.Price' variable
z = c()
for(p in data$Softlog.Price)
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
#' We can see there are 34 missing values in the price column.
#' Checking in the dataset itself, values for softlog prices are available
#' until a certain point, after which we have these missing values.
#' Hence, we can simply remove the tail end of the dataset where these
#' missing values are concentrated.
t_new = t[c(1:(length(t) - 34))]
z_new = z[c(1:(length(z) - 34))]
df = data.frame(t_new, z_new)
summary(df)
#------------
#' Creating a time series for softlog prices
Z = ts(z_new, start = c(1990, 4, 1), end = c(2017, 6, 1), frequency = 12)
# frequency = 12 -> monthly frequency
#========================
#' TIME PLOT AND POTENTIAL TIME SERIES COMPONENTS
#------------
ts.plot(Z,
        main = "Softlog prices: 1990-2017",
        ylab = "Price",
        xlab = "Year",
        col = "darkgreen")
#' Based on the time plot, we may conclude that there is no clear seasonality (i.e. no periodic fluctuations annually).
#' There seems to be an overall downward trend over time, with a seemingly high level of irregular fluctuations.
#' We also see no discernible long term fluctuation pattern,
#' hence we may conclude that there is no cyclical fluctuation.
#' Hence, the major components of this time series seem to be trend and irregular fluctuations.
#========================
#' ELIMINATING TREND THROUGH METHOD OF DIFFERENCING
#------------
#' We will use the model z_t = m_t + e_t, where
#' z_t is the softlog price at time t,
#' m_t is the trend level at time t,
#' e_t is the error at time t (irregular fluctuations).
#' We assume there to be a constant downward trend component.
#------------
#' To use the method of differencing to remove trend, we use the 'diff' function.
#' Here, since we want to remove trend, and since we assume constant trend,
#' we use lag = 1 i.e. we find take differences between each unit of time
#' (the unit of time in our case in one month).
Z1 = diff(Z, lag = 1)
ts.plot(Z1,
         main = "Softlog prices without trend",
         ylab = "Price",
         xlab = "Year",
         col = "darkgreen")
#========================
#' TESTING STATIONARITY OF TIME SERIES
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
#______
# install.packages("tseries")
library(tseries)
#' Original time series stationarity
adf.test(Z)
#' Observing ACF plot
acf(Z)
#______
#' Trend-removed time series stationarity
adf.test(Z1)
#' Observing ACF plot
acf(Z1)
#************************************
#' PART 2: REMOVING TREND & SEASONALITY FROM DATA ON RUBBER PRICES
#========================
#' Defining and reformatting the 'Rubber.Price' variable
z = c()
for(p in data$Rubber.Price)
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
#------------
#' Creating a time series for softlog prices
Z = ts(z, start = c(1990, 4, 1), end = c(2020, 6, 1), frequency = 12)
# frequency = 12 -> monthly frequency
#========================
#' TIME PLOT AND POTENTIAL TIME SERIES COMPONENTS
#------------
ts.plot(Z,
        main = "Rubber prices: 1990-2020",
        ylab = "Price",
        xlab = "Year",
        col = "darkgreen")
#' Based on the time plot, we may conclude that there may be some seasonality (i.e. some periodic fluctuations annually),
#' as we see how at the start of most years, the prices shoot upwards, and tend to fall before rising again the next year.
#' There is no constant trend over time, but we may interpret the graph's movement from 2000 to 2011 as a roughly upward trend.
#' Since we cannot confirm whether this trend flattens over time, we may consider the existence of a trend component.
#' There is a noticeable of irregular fluctuations.
#' We also see no discernible long term fluctuation pattern,
#' hence we may conclude that there is no cyclical fluctuation.
#' Hence, the major components of this time series seem to be trend and irregular fluctuations.
#========================
#' ELIMINATING TREND THROUGH METHOD OF DIFFERENCING
#------------
#' We will use the model z_t = m_t + + s_t + e_t, where
#' z_t is the softlog price at time t,
#' m_t is the trend level at time t,
#' e_t is the error at time t (irregular fluctuations).
#' We assume there to be a constant upward trend component,
#' although this is not always the case in the data, as far as we can see.
#------------
#' To use the method of differencing to remove trend, we use the 'diff' function.
#' Seasonality is the existence of periodic patterns that span over a period of 12 months.
#' Hence, we will use lag = 12 to eliminate seasonality.
Z1 = diff(Z, lag = 12)
ts.plot(Z1,
        main = "Rubber prices without seasonality",
        ylab = "Price",
        xlab = "Year",
        col = "darkgreen")
#' To use the method of differencing to remove trend, we use the 'diff' function.
#' Here, since we want to remove trend, and since we assume constant trend,
#' we use lag = 1 i.e. we find take differences between each unit of time
#' (the unit of time in our case in one month).
Z2 = diff(Z1, lag = 1)
ts.plot(Z2,
        main = "Rubber prices without trend & seasonality",
        ylab = "Price",
        xlab = "Year",
        col = "darkgreen")
#========================
#' TESTING STATIONARITY OF TIME SERIES
#------------
#' Performing the test using the 'tseries' library
#______
#' Original time series stationarity
adf.test(Z)
#' Observing ACF plot
acf(Z)
#______
#' Seasonality-removed time series stationarity
adf.test(Z1)
#' Observing ACF plot
acf(Z1)
#______
#' Trend-removed time series stationarity
adf.test(Z2)
#' Observing ACF plot
acf(Z2)