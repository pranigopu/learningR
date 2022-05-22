#' 1. DATA SET
myData = read.csv("~/Documents/Study/computerScience/programming/r/data/justSomeData.csv")
head(myData)
#========================
#' LINEAR REGRESSION MODEL
model = lm(y~., data = myData)
summary(model)
#========================
#' USING THE DURBIN WATSON TEST TO CHECK FOR AUTOCORRELATION
#
#' Autocorrelation is the presence of correlation between current and past values of the response variable.
#' This can occur if past values have some impact on future values of the variable.
#' For example, if the response variable is monthly expenditure, excess expenditure in one month may compel people to spend less the next month.
#' Autocorrelation may also happen at a lag i.e. current values may be related to a past value before the adjacent past value.
#' For example, current investments may have an impact on future investments after a time gap (after seeing how well the investment has performed).
#------------------------
#' The required package...
# install.packages("lmtest")
library(lmtest)
#' From this, we will be using the dwtest function, which accepts the basic linear relationship of the variables as the model.
#' For example, y~x, or y~x1+x2 etc.
dwtest(y~., data = myData)
#' p-value of the test statistic is 0.6585, meaning it is not significant for a 0.05 significance level
#' (i.e. it is not unlikely enough to be considered as significant indication for autocorrelation),
#' Hence, we may conclude that there is no autocorrelation within the response variable.
#' This means that we may conclude that past values of y don't affect future values.
#' Of course, this is not 100% conclusive, and there may still be positive autocorrelation, as suggested by the DW value being 2.2278.