#' DATA SET
setwd("~/Documents/Study/computerScience/programming/r/data/")
data = read.csv("weatherAustralia.csv")[c(-6:-8, -9:-11, -22, -23)]
# (Categorical and redundant variables have been removed)
head(data)
#-----------------------
#' Cleaning the data (removing null values and unused columns)
data = na.omit(data[c(-1, -2)])
head(data)
#-----------------------
# Target variable (response)...
summary(data$Rainfall)
#' We want to be able to predict rainfall.
#' Hence, we want to grasp the relationship between the various variables and rainfall.
#' For the best model, we want to eliminate the unreliable or insignificant predictors,
#' and retain the ones that reduce variation error and increase R-square.
#-----------------------
# Predictor variables (regressors)...
summary(data[-3])
#========================
#' USING IN-BUILT STEPWISE MODEL SELECTION FUNCTION
#
#' This is to identify the best predictors among the available fields,
#' by estimate the significance of their regression coefficients.
#' For our purpose, we will use a combination of forward and backward selection,
#' thereby eliminating as many irrelvant variables as possible while retaining as
#' many relevant variables as possible.
#------------------------
#' Full model (with all regressors)
fullModel = lm(data$Rainfall~., data)
#' This is the model with all the available regressors.
#' This will be used as a source for the regressors, when we create the best fitting model.
summary(fullModel)
#' As can be seen, this is a relatively poor model,
#' with an adjusted R-square value of 0.1405.
#' However, we see that the p-value for the F-statistic for this model
#' is below 0.05, meaning that at least one regressor is significantly linearly related
#' to the response, given a 0.05 significance level.
#------------------------
#' Applying the step function
results = step(fullModel, direction = "both", scope = formula(fullModel))
#' Obtaining the selected variables and their coefficients...
results$coefficients
#' From the above, we see that the final selected regressors contain all the possible regressors except
#' MinTemp (minimum temperature for the day) and WindSpeed3pm (wind speed at 3PM).
#____________
#' From the results of this function, we obtain the following model..
f = formula(Rainfall~.-MinTemp-WindSpeed3pm)
finalModel = lm(f, data)
# (All regressors except MinTemp and WindSpeed3pm)
summary(finalModel)
#========================
#' ANALYSING THE FINAL MODEL
#------------------------
#' Testing for autocorrelation
#
#' Autocorrelation is the presence of correlation between current and past values of the response variable.
#' This can occur if past values have some impact on future values of the variable.
#' Autocorrelation may also happen at a lag i.e. current values may be related to a past value before the adjacent past value.
#' To check for autocorrelation, we wil use the Burbin-Watson test.
library(lmtest)
#' From this, we will be using the dwtest function, which accepts the basic linear relationship of the variables as the model.
dwtest(f, data = data)
# (NOTE: f is the previously created formula for the final model)
#' p-value of the test statistic is much below 0.05, meaning it is significant for a 0.05 significance level
#' (i.e. it is unlikely enough to be considered as significant indication for autocorrelation).
#' Hence, we may conclude that there is autocorrelation within the response variable, and judging by the sign,
#' this is a positive autocorrelation.
#' This means that we may conclude that past rainfall have a significant implact on future rainfall.
#' Since our data set is a time-series data, and since rainfall tends to occur on a seasonal basis, and not randomly,
#' this result makes sense, since a certain amount or range of amounts of rainfall is likely to be continually sustained over a period of many days,
#' and likely to rise or fall consistently over a period of time.
#' 
#------------------------
#' Testing for multicollinearity
#
#' Multicollinearity is the presence of moderate to strong linear correlation between
#' two or more regressors within a linear regression model.
# Checking the correlation matrix of the regressors...
library(ppcor)
#' Choosing columns of regressors
# (All rows from all except the 3rd column)
X = data[-3]
correlMatrix = pcor(X, method = "pearson")
# Correlation coefficients...
correlMatrix$estimate
# p-values for the correlation coefficients...
correlMatrix$p.value
# FIELDS IN THE MATRIX
# estimate => matrix of the partial correlation coefficient between two variables.
# p.value => a matrix of the p values of the correlation coefficients.
# statistic	=> a matrix of the value of the test statistics (not sure what this means).
# n => sample size.
# EXPLANATION
#' The p-value of a correlation coefficient for two variables (obtained from a sample) is the probability that you would have found the current result if the correlation coefficient were in fact zero
#' (i.e. if you found correlation in the sample while there is none in the population i.e. wrongly rejected null hypothesis).
#' If this probability is lower than the significance level, the correlation coefficient is said to be statistically significant.
#' Significance level is the proportion of the lowest probability values of a statistic that, if the statistic actually takes such a value, you would consider it as significantly different from the population.
#' Here, the population distribution contains probabilities of getting values assuming that the null hypothesis is true (in this case, null hypothesis is that there is zero correlation between the two variables).
# CONCLUSIONS
#' We see that most of the regressors have weak positive correlation.
#' The only ones (that I can spot) with moderate correlation are Pressure3pm and WindSpeed3pm.
#' The p-values for these coefficients are both significant and insignificant, meaning that
#' some regressors are associated, maybe not strongly, but enough to have been less likely similar by chance.
#____________
#' Searching for a potential cause of multicollinearity
#------
#' Approach
#
#' We need the VIF (variance inflation factor i.e. the factor by which a variable's regression coefficient has inflated from the constant variance of the error term) of each regressor.
#' As a thumb rule, if the VIF of a regressor is greater than 10, it indicates that the particular regressor is the cause of the multicollinearity in the model.
library(car)
vif(finalModel)
# CONCLUSIONS
#' We can see that MaxTemp, Pressure9am, Pressure3pm, Temp9am and Temp3pm have VIF values greater than 10.
#' This indicates that all these regressors are contributing to the multicollinearity in the model.
#' Significant correlation between regressors of the same thing in the same day (ex. Temp9am and Temp3pm) may be expected to be correlated,
#' since they are taken in the same day merely 6 hourse apart, and the weather is not likely to change drastically in that time period.
#' Also, some quantities are related physically to some degree, such as air pressure and wind speed.
#' Given all this, it is not surprising that multicollinearity exists in the model.
#------------------------
#' Testing for heteroscedasticity
#
#' Homoscedasticity in a model means that the error is constant i,e, error terms are equal for a given regressor value (hence, it must be measured across models taken from different samples of the same population).
#' The best way for checking homoscedasticity is to make a scatterplot with the residuals against the regressor.
#' However, due to a large number of variables in the model, we will not do this.
#' Instead, to test the degree of heteroscedasticity in the response 'Rainfall',
#' we will use the Breusch-Pagan test (note that the null hypothesis claims homoscedasticity).
library(lmtest)
bptest(finalModel)
#' As we can see, the p-value of the test statistic BP is less than 0.05.
#' This means we may reject the null hypothesis, hence accepting the claim that the error terms in the model are significantly unequal.
#' This violates one of the assumptions of linear regression modelling, hence indicating that our model may not be a reliable predictor of the response i.e. Rainfall.
#------------------------
#' Residual plots
#
#' Residual plots are designed to plot the key values of the model, after considering all the regressors.
#' Jence, it is a way for us to analyse the model, and conclude on its reliablility.
plot(finalModel)