setwd("/Users/pranav/Documents/Study/computerScience/programming/r/data")
completeData = read.csv("patientSatisfaction.csv")
head(completeData)

#' IDENTIFYING INDEPENDENT AND DEPENDENT VARIABLES
x = completeData$Anxiety
y = completeData$Satisfaction

#' SCATTER PLOT
library(stats)
plot(x, y,
     type = "p",
     main = "Satisfaction with respect to anxiety levels",
     xlab = "Anxiety",
     ylab = "Satisfaction",
     col = "blue",
     pch = 16,
     las = 1)
# type => the kind of plot. "p" means points, "l" means lines
# col => colour
# pch => point character type
# las => the type of orientation of the labels on the axes
#' CORRELATION
myData = data.frame(x, y)
cor(myData)

#' ESTIMATED LINEAR REGRESSION MODEL
myLinRegModel = lm(y ~ x)
myLinRegModel
y_predicted = fitted.values(myLinRegModel)
#' The other more obvious way to get fitted or predicted y values would be to use the equation\n
#' y_predicted = 90.997 - 6.174*x\n
#' The difference is using the function, every predicted y-value is attached to an index.
#' Furthermore, using the function demonstrably yields more accurate results, due to the lack of rounded up estimates for the coefficients.
# EXPLANATION OF CLAIMS
#' You will see that the predicted y-values obtained form these two methods are very close but not the same.
#' And, since the sum for the function-obtained values equals the sum of observed y-values, I conclude that using the function yields more accurate results.
#' Note that these findings are a result of comparison I performed personally.\n\n
#' PLOTTING THE REGRESSION LINE ALONE
plot(x, y_predicted,
     type = "l",
     main = "Predicted linear regression model",
     xlab = "Anxiety",
     ylab = "Predicted satisfaction",
     col = "black",
     lwd = 2)
# lwd => line width

#' PLOTTING THE REGRESSION LINE WITH THE SCATTER PLOT
plot(x, y,
     type = "p",
     main = "Satisfaction with respect to anxiety levels",
     xlab = "Anxiety",
     ylab = "Satisfaction",
     col = "blue",
     pch = 16,
     las = 1)
abline(myLinRegModel, lwd = 2)
# This function adds one or more regression lines through the current plot.

#' COMPARING SUMS OF PREDICTED AND OBSERVED VALUES
sum(y_predicted)
sum(y)

#' OBTAINING RESIDUALS
obtainedResiduals = residuals(myLinRegModel)
sum(y - y_predicted)
#' The other, more obvious way to derive residuals is to simply get the difference\n
#' y - y_predicted\n
# EXPLANATION OF CLAIMS
#' You will see that the residuals obtained form these two methods are very close but not the same.
#' And, in this case at least, since the sum for the function-obtained residuals is two times smaller than the sum of simple difference obtained residuals, I assume that using the function yields more accurate results.
#' Note that these findings are a result of comparison I performed personally.