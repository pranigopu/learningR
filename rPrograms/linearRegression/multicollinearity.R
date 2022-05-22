#' 1. DATA SET
myData = read.csv("~/Documents/Study/computerScience/programming/r/data/multicollinearityData.csv")
head(myData)
#========================
#' LINEAR REGRESSION MODEL
model = lm(y~., data = myData)
summary(model)
#========================
#' 2. FINDING CORRELATION BETWEEN REGRESSORS
#------------------------
#' Necessary package(s)
# install.packages("MASS")
# install.packages("ppcor")
library(ppcor)
#' Official documentation: https://rdrr.io/cran/ppcor/man/pcor.html
# ppcor => partial and semi-partial (part) correlation
# Calculates partial and semi-partial (part) correlations in a linear regression model.
# It also calculates the p-values of each correlation coefficient (discussed later)
#------------------------
#' Choosing columns of independent variables
# All rows from all except the 1st column...
xs = myData[, -1]
head(xs)
#------------------------
#' Finding correlation matrix for independent variables
#
#' The p-value of a correlation coefficient for two variables (obtained from a sample) is the probability that you would have found the current result if the correlation coefficient were in fact zero (i.e. if you found correlation in the sample while there is none in the population i.e. wrongly rejected null hypothesis).
#' If this probability is lower than the significance level, the correlation coefficient is said to be statistically significant.
# NOTE
#' Significance level is the proportion of the lowest probability values of a statistic that, if the statistic actually takes such a value, you would consider it as significantly different from the population.
#' Here, the population distribution contains probabilities of getting values assuming that the null hypothesis is true (in this case, null hypothesis is that there is zero correlation between the two variables).
# Correlation matrix...
correlMatrix = pcor(xs, method = "pearson")
correlMatrix
#' FIELDS IN THE RESULT
# estimate => matrix of the partial correlation coefficient between two variables.
# p.value => a matrix of the p values of the correlation coefficients.
# statistic	=> a matrix of the value of the test statistics (not sure what this means).
# n => sample size.
#========================
#' SEARCHING FOR CAUSE OF MULTICOLLINEARITY
#------------------------
#' Approach
#
#' We need the VIF (variance inflation factor i.e. the factor by which a variable's regression coefficient has inflated from the constant variance of the error term) of each regressor.
#' As a thumb rule, if the VIF of a regressor is greater than 10, it indicates that the particular regressor is the cause of the multicollinearity in the model.
#------------------------
#' Library needed
# install.packages("car")
library(car) # Contains VIF calculation function.
#------------------------
#' Applying the VIF function on the linear regression model...
vif(model)