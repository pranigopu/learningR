setwd("~/Documents/Study/computerScience/programming/r/data/")
d = read.csv("justSomeData.csv")[c(1:6)]
head(d)
#========================
#' FULL MODEL BEST FIT AND RESIDUAL ANALYSIS
#
#' We will be finding the best fit model for all the regressors together and the response.
#' We will also confirm the normality assumption (i.e. residuals of the model are normally distributed).
#
#' FITTED MODEL
model = lm(y~., d)
summary(model)
#' SCATTER PLOTS FOR VARIOUS REGRESSORS
# Function for doing this...
scatterPlot = function(x, y)
{
  plot(x, y,
       type = "p",
       xlab = "Regressor",
       ylab = "Response",
       col = "blue",
       pch = 16,
  )
  # To draw the fitted regression line.
  abline(lm(y~x, data.frame(x, y)), lwd = 2)
}
# Loop for function calls...
for(x in d[c(2:6)]){scatterPlot(x, d$y)}
#' NORMALITY ASSUMPTION VERIFICATION
plot(model)
#========================
#' BEST FIT MODEL THROUGH VARIABLE SELECTION
#
model = lm(y~., d)
#' This is the model with all the available regressors.
#' This will be used as a source for the regressors, when we create the best fitting model.
summary(model)

#' STEP FUNCTION
#
#' Chooses the best regression model using the AIC stepwise variable selection algorithm.
#' Best in this context means the model that is has the regressors and coefficients that best explain or match the responses, given the data.
#' Hence, it is not only best fitting for a given set of regressors, it is also best fitting among all possible models using the available regressors.
#
#' (AIC => Akaike’s information criterion.
#' It compares the quality of a set of statistical models to each other)
#
#' USAGE
# step(object, scope, scale = 0,
# direction = c("both", "backward", "forward"),
# trace = 1, keep = NULL, steps = 1000, k = 2)
#
#' Argument "object" is an object representing a model of an appropriate class (mainly "lm" and "glm").
#' This is used as the initial model in the stepwise search (variable selection) for the best regressors for modelling the given response.
#' Initial model implies the model with the response and an initial set of regressors and coefficients on top of which more regressors will be added.
#' Typically, it is a model with only the response, intercept and error term.
#
#' Argument "scope" defines the range of models examined in the stepwise search.
#' It holds the model or models containing the different regressors that may be selected for the final model returned by the function.
#' This option could contain a single model, or two models "lower" and "upper",
#' wherein the regressors in the lower model are a subset of the regressors in the upper model.
#' In the case of "lower" and "upper" models, the step function performs a stepwise search for every model from the lower to the upper (and the models in between, with respect to rhe regressors used).
#
#' Argument "direction" the mode of stepwise search, can be one of "both", "backward", or "forward", with a default of "both".
#' If the scope argument is missing the default for direction is "backward".
#' Forward implies that we start with less regressors, and keep adding and fitting more.
#
#' MAKING THE INITIAL MODEL
initialModel = lm(y~1, data = d)
# 1 as the regressor implies the constant term, whose coefficient is the intercept.
summary(initialModel)
#' PERFORMING THE STEPWISE SEARCH
step(initialModel, direction = "forward", scope = formula(model))
#' From the function's results, we see that x1 and x2 are the best regressors for our response y, with the given coefficients leading to the best fitting model possible for the data and available regressors.
#
#' FINAL MODEL (BEST MODEL)
bestModel  = lm(y~x1+x2, data = d)
summary(bestModel )

#' NORMALITY ASSUMPTION VERIFICATION
plot(model)