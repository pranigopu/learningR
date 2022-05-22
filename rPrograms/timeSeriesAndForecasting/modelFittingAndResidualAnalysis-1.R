#========================
#' DATASET
#------------
data = read.csv("~/Documents/Study/computerScience/programming/r/data/annualCommonStockPrice (US).csv")
year = data$Year
annualStockPrice = data$Annual.common.stock.price..US.
data = data.frame(year, annualStockPrice)
head(data)
summary(data)
#========================
#' CONVERTING ANNUAL STOCK PRICE TO TIME SERIES OBJECT
#------------
z = ts(annualStockPrice, start=c(1871, 1, 1), end=c(1960, 1, 1), frequency = 1)
#========================
#' BASIC ANALYSIS
#------------
ts.plot(z,
        main="Annual stock price",
        ylab="Stock price")
acf(z, 50, main="ACF for annual stock price")
#' We see strong positive autocorrelation upto 9 lags.
#' Hence, we can infer some degree of autoregression.
#' Furthermore, we see no clear evidence of periodic fluctuations.
#========================
#' TRYING TO FIT ARMA(1, 1)
#------------
library(forecast)
model = auto.arima(z,
                   max.d = 0,
                   start.p = 1, max.p = 1,
                   start.q = 1, max.q = 1,
                   seasonal = FALSE)
summary(model)
plot(model$fitted, main="ARMA(1,1) fitted to data", ylab="Stock price")
#' Parameters are
#' phi = 0, theta = 0.9245
#========================
#' FINDING THE BEST ARIMA MODEL
#------------
#' Model shows trend but no periodicity, hence we give 'seasonal=FALSE'.
#' To see the various models that have been fitted by the function to our time series
#' we put 'trace=TRUE'. The model finally selected is the one that gives the least
#' residual sum of squares (residual => difference between model's estimate and actual value).
#------------
bestModel = auto.arima(z, seasonal = FALSE)
summary(bestModel)
plot(bestModel$fitted,
     main="Best model fitted to data",
     ylab="Stock price")
#' As we can see, while the ARMA(1, 1) model roughly fits the data,
#' the best model derived here, which is an ARIMA(1, 2, 3) model, fits the
#' data much more closely
#========================
#' RESIDUAL ANALYSIS FOR THE BEST ARIMA MODEL
#------------
residuals = bestModel$residuals
#' Basic analysis
plot(residuals,
     main="Best model residuals",
     ylab="Residuals")
#' We see that the residuals are centered around the mean and have a limited range.
#------------
#' ASSUMPTION 1: Residuals are uncorrelated
#______
#' Using ACF plot...
acf(residuals, main="ACF of best model residuals")
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
#' From the above, we see that many of the points do not fall on the line.
#' This indicates that the residuals may not have been drawn representatively from a normal distribution.
#' Hence, we may conclude that the residuals are not normally distributed.
#______
#' Using Shapiro-Wilk test...
# H_0: No difference between normal distribution and sample distribution
# H_1: There is difference between normal distribution and sample distribution
shapiro.test(residuals)
#' p-value of the test statistic is below 0.05. Hence, for a 0.05 significance level,
#' we may reject the null hypothesis and conclude that the residuals are not normally
#' distributed.
#------------
#' CONCLUSIONS & INTERPRETATIONS
#______
#' The residuals from the best model (as decided by the auto.arima function) produces
#' residuals that are uncorrelated but not normally distributed.
#' Hence, this indicates that the obtained model may not be ideal for the given time series,
#' possibly due to overfitting, since we saw that the plot of the estimates from the model
#' closely matched the plot of the actual observations