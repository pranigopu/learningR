#========================
#' DATASET
#------------
setwd("~/Documents/Study/computerScience/programming/r/data/")
data = read.table("earthquakesTimeSeries.txt", header = TRUE, sep = "\t")
head(data)
summary(data)
#========================
#' EARTHQUAKE FREQUENCY TIME SERIES OBJECT
#------------
z = ts(data$Quakes, start=c(1916, 1, 1), end=c(2015, 1, 1), frequency = 1)
#========================
#' BASIC ANALYSIS
#------------
ts.plot(z,
        main="Earthquakes per year",
        ylab="Frequency")
acf(z, 50, main="ACF for earthquakes per year")
#' We see strong positive autocorrelation for lags 1 and 3.
#' We see no clear evidence of periodic fluctuations, including seasonality.
#========================
#' FINDING THE BEST ARMA MODEL
#------------
#' Model shows no clear trend or periodicity, hence we give 'seasonal=FALSE'.
#' Since this assignment requires fitting for an ARMA model, we put the order of differencing as 0, using 'max.d=0'.
#' The ARMA model finally selected is the one that gives the least
#' residual sum of squares (residual => difference between model's estimate and actual value).
#------------
library(forecast)
model = auto.arima(z, seasonal = FALSE, max.d = 0)
summary(model)
plot(model$fitted,
     main="Best model fitted to data",
     ylab="Frequency")
#' We see that while the estimated observations plot does not closely resemble
#' the actual observations plot, this is because of smoothening, which follows the average values
#' and disregards much of the irregular fluctuations. Hence, this model may be more accurate overall,
#' since it has clearly not been overfitted to irregular fluctuations.
#========================
#' RESIDUAL ANALYSIS FOR THE BEST ARIMA MODEL
#------------
residuals = model$residuals
#' Basic analysis
plot(residuals,
     main="Model residuals",
     ylab="Residuals")
#' We see that the residuals are centered around the mean and have a limited range.
#------------
#' ASSUMPTION 1: Residuals are uncorrelated
#______
#' Using ACF plot...
acf(residuals, main="ACF of model residuals")
#' From this ACF plot, we see that the residuals are a stationary time series,
#' with insignificant correlation betwee residuals at any lag, since all autocorrelation coefficients
#' lie between the threshold values -0.2 and 0.2.
#' Hence, we can conclude that the residuals are uncorrelated.
#' Furthermore, given that they are centered at zero, we may conclude that
#' the residuals form a white noise sequence.
#______
#' Using Portmanteau test (Box-Pierce test)...
# H_0: No significant autocorrelation
# H_1: Significant autocorrelation for at least some lags
Box.test(residuals)
#' p-value of the test statistic exceeds 0.05. Hence, for a 0.05 significance level,
#' we may accept the null hypothesis and conclude that there is no significant
#' autocorrelation in the residuals i.e. they are uncorrelated.
#------------
#' ASSUMPTION 2: Residuals are normally distributed
#______
#' Using Q-Q plot (quantile-quantile plot) with respect to a normal distribution...
# Plotting sample quantiles against theoretical normal distribution quantiles...
qqnorm(residuals)
# Plotting the line formed when sample and theoretical normal distribution quantiles would be equal...
qqline(residuals)
#' From the above, we see that many of the points mostly fall on the line.
#' This indicates that the residuals can be considered to be drawn representatively from a normal distribution.
#' Hence, we may conclude that the residuals are normally distributed.
#______
#' Using Shapiro-Wilk test...
# H_0: No difference between normal distribution and sample distribution
# H_1: There is difference between normal distribution and sample distribution
shapiro.test(residuals)
#' p-value of the test statistic is above 0.05. Hence, for a 0.05 significance level,
#' we may accept the null hypothesis and conclude that the residuals are normally
#' distributed.
#------------
#' CONCLUSIONS & INTERPRETATIONS
#______
#' The residuals from the above model (as optimised by the auto.arima function) produces
#' residuals that are uncorrelated and normally distributed.
#' Hence, this indicates that the obtained model is reliable overall, since it has neither been overfitted not underfitted.