#'INTRODUCTION TO HOMOSCEDASTICITY
#
#' Homoscedasticity in a model means that the error is constant along the values of the dependent variable.
#' The best way for checking homoscedasticity is to make a scatterplot with the residuals against the dependent variable.
#========================
#' DATA SET
setwd("/Users/pranav/Documents/Study/computerScience/programming/r/data/")
data = read.csv("expensesAndIncome.csv")
head(data)
#========================
#' VARIABLES
#
#' Expenditure depends on income, since expenditure is made based on the received income.
#' Making a regression model for income and expenses will aim to see the relationship between income and expenditure.
y = data$Expenditure
x = data$Income
#========================
#' CREATING LINEAR REGRESSION MODEL
model = lm(y~x)
summary(model)
#========================
#' VISUALISING HOMOSCEDASTICITY
plot(x, y,
     type = "p",
     main = "Expenditure with respect to Income",
     xlab = "Income",
     ylab = "Expenditure",
     col = "blue",
     pch = 16,
     las = 1)
abline(model, lw = 2)
#' As can be seem, the errors noticeably increase in magnitude the larger the incomes.
#' This indicates the presence of heteroscedasticity in the data.
#========================
#' TESTING FOR HOMOSCEDASTICITY
#
#' To test the degree of heteroscedasticity in the response 'Expenditure',
#' we will use the Breusch-Pagan test.
#' To perform this, we need to import the 'lmtest' package.
# install.packages("lmtest")
library(lmtest)
#' Performing the test...
bptest(model)