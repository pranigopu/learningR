setwd("~/Documents/Study/computerScience/programming/r/data/")
myData = read.csv("justSomeData.csv")
head(myData)

#' RAW MODEL (FULL MODEL)
#
#' This is the model with all the available regressors.
#' This will be used as a source for the regressors, when we create the best fitting model.
rawModel = lm(y~., data = myData)
summary(rawModel)

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
#' Backward implies that we start with all regressors, and keep removing insignificant or less significant regressors to arrive at the most explanatory model.
#
#' Since this is backward selection, the initial model is the full model.
#
#' PERFORMING THE STEPWISE SEARCH
step(rawModel, direction = "backward")

#' From the function's results, we see that x1 and x2 are the best regressors for our response y, with the given coefficients leading to the best fitting model possible for the data and available regressors.
#
#' FINAL MODEL (BEST MODEL)
cookedModel  = lm(y~x1+x2+x5+x6+x9, data = myData)
summary(cookedModel )
