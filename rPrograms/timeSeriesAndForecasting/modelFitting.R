#========================
#' DATASET
#------------
setwd("~/Documents/Study/computerScience/programming/r/data/")
data = read.csv("monthlyBeerProductionIndia.csv")
head(data)
summary(data)
#------------
#' Dividing dataset into training and testing data
# (To test for model accuracy later)
fullDataLength = length(data$Month)
#' 80% of the data will be for training, the rest for testing
trainingDataLength = as.integer(fullDataLength*80/100)
trainingData = data$Monthly.beer.production[c(1:trainingDataLength)]
testingData = data$Monthly.beer.production[c((trainingDataLength+1):fullDataLength)]
#' Note that the above data values do not contain any columns, they are simply vectors of numeric values.
#========================
#' TRAINING DATA HANDLING
#------------
#' Checking first and last dates
data$Month[1]
data$Month[trainingDataLength]
# Alternate code (since we have converted the month column to date objects)
# min(data$Month[1])
# max(data$Month[1])

# Converting training data to time series
z = ts(trainingData, start = c(1956, 1, 1), end = c(1987, 8, 1), frequency = 12)
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
#' we may observe it, especially considering the wave-like motion of the data's plot overall.
#' Irregular fluctuations seem to be limited, and the main source of
#' fluctuation seems to be periodic (in a smaller scale than seasonal periodicity i.e. for a time frame below a year).
#' We also see that the range of fluctuation seems to be limited around the general trend of the data,
#' implying constant variance.
#------------
#' ACF plot
acf(z, main="ACF")
acf(z, main="ACF", 100)
acf(z, main="ACF", 1000)
#' Hence, from the ACF, due to the ACF being a periodic function of the lag 1, we observe
#' autoregression of order 1. Furthermore, we saw in the time plot that the variance may be constant.
#' However, we observed some sort of trend, which is not consistent, and may infact present an overall lack of trend
#' in the long run (which we cannot confirm right now).
#------------
#' Stationarity testing
# H_0: Data is not stationary
# H_1: Data is stationary
library(tseries)
adf.test(z)
#' p-value is below 0.05. Hence, for a 0.05 significance level, we may reject the null hypothesis and
#' conclude that the data is stationary.
#========================
#' FINDING THE BEST ARMA MODEL
#------------
#' Model shows no clear trend or periodicity, hence we give 'seasonal=FALSE'.
#' We expect there to be at least an AR(1) (higher orders may be possible) component.
#' Data was tested to be stationary using the ADF test,
#' so we expect no integration component.
#------------
library(forecast)
model = auto.arima(z, seasonal = FALSE, max.d = 0)
summary(model)
plot(model$fitted,
     main="Best model fitted to data",
     ylab="Frequency")
#' We see that model's plot closely resembles the actual data, indicating a goodness of fit.
#' We can perform residual analysis to further confirm whether this model is suitable...
#========================
#' QUICK RESIDUAL ANALYSIS
#------------
#' Residual analysis checks whether the residuals of a model
#' (i.e. the differences between the model's estimates and the actual values)
#' are uncorrelated and normally distributed. The reasoning behind this is discussed at the end of this section.
residuals = model$residuals
#' Checking mean of residuals
mean(residuals)
#' The mean is close to 0. However, to see if the mean is close enough to zero that we may consider
#' the mean of all possible residuals to be 0,
#' we will perform a one-sample t-test to compare the residual mean to the theoretical value 0.
#' For the sample of residuals and theoretical mean 0, we have the following hypotheses...
# H_0: True mean of residuals is 0
# H_1 True mean of residuals not 0
t.test(residuals, mu=0)
#' p-value exceeds 0.05. Hence, given a 0.05 significance level, we may accept the null hypothesis and
#' conclude that the true mean of the residuals of this model is 0.
#______
#' Using Portmanteau test (Box-Pierce test) to test for uncorrelatedness
# H_0: No significant autocorrelation
# H_1: Significant autocorrelation for at least some lags
Box.test(residuals)
#' p-value is greater than 0.05. Hence, given a 0.05 significance level, we may accept the null hypothesis
#' and conclude that there is no significant autocorrelation in the residuals i.e. they are uncorrelated.
#______
#' Using Shapiro-Wilk test to test for normality
# H_0: No difference between normal distribution and sample distribution
# H_1: There is difference between normal distribution and sample distribution
shapiro.test(residuals)
#' p-value of the test statistic is above 0.05. Hence, for a 0.05 significance level,
#' we may accept the null hypothesis and conclude that the residuals are normally
#' distributed.
#------------
#' Based on our residual analysis, we can conclude that the residuals obtained from the estimated model are
#' uncorrelated and normally distributed. From uncorrelatedness, we may conclude that the errors are independently generated
#' i.e. they are not dependent on previously obtained errors. The more correlated the errors are, the more accurately we can model them using autoregression models.
#' Hence, ierrors are not being sufficiently uncorrelated indicates that the errors are deterministic to some degree, and we have not modelled this deterministic aspect,
#' which means our model is consistently more inaccurate than it could be.
#
#' From normality, we may conclude that the errors are not determinined by any external deterministic factors.
#' This is because the residuals are expected to be 0 most of the time, and the probability of obtaining a residual other than 0
#' must be lesser the further the residual is from 0. The probability must only depend on the distance from 0, and not the sign.
#' All this implies that the residuals must follow a standard normal distribution.
#' Since we have already determinined the the true mean of the residuals is 0, all we need to confirm is normality.
#' I the residuals are not normally distributed, and especially if the distribution is assymmetric, it may indicate the
#' presence of an external deterministic factor affecting the residuals to behave differently than expected.
#
#' Combining both the above points, we may conclude that the source of errors
#' (i.e. deviations between the model estimates and the actual values) is not systematic (i.e. there is no practical way to model these errors).
#========================
#' OBTAINING AND TESTING FORECASTS
#------------
#' For this, we will compare the forecasts for the testing data's time periods to the actual testing data from these time periods.
#______
#' Forecasts
testingDataLength = fullDataLength - trainingDataLength
forecasted = forecast(model, h = testingDataLength)
#' Plotting forecasts
plot(forecasted,
     main = "Past data + forecasts for the next 96 monthts",
     xlab = "Year",
     ylab = "Price")
#' Comparing with plot of actual values
# Converting whole data set into time series object
data$Month[1] # Minimum date
data$Month[fullDataLength] # Maximum date
full = ts(data$Monthly.beer.production, start=c(1956, 1, 1), end=c(1995, 8, 1), frequency=12)
ts.plot(full,
        main="Monthly beer production in India",
        xlab="Time",
        ylab="Production")
#' Through the above two plots, we can observe that the expected forecasts
#' were lower than the moving average of the actual values. This may suggest one or both of the following:
# Inaccurate model, due to missing deterministic factors or insufficient training data
# Extraneous factors that could not have been reliably modelled

#' However, we saw from our residual analysis that there is a high likelihood that
#' our model takes most, if not all deterministic factors into account. Hence, we may conclude
#' that while our model may not be extremely accurate for forecasting, the inaccuracy for the given test data
#' could be because of extraneous factors. Most likely, the main requirement may be more training data.