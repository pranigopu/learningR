setwd("/Users/pranav/Documents/Study/computerScience/programming/r/data")
completeData = read.csv("patientSatisfaction1.csv")
head(completeData)

#' IDENTIFYING INDEPENDENT AND DEPENDENT VARIABLES
x = completeData$Anxiety
y = completeData$Satisfaction

#' SCATTER PLOT
library(stats)
# type => the kind of plot. "p" means points, "l" means lines
# col => colour
# pch => point character type
# las => the type of orientation of the labels on the axes

#' ESTIMATED LINEAR REGRESSION MODEL
myLinRegModel = lm(y ~ x)

#' HYPOTHESES (beta 1)
# H_0: beta 1 = 0 (change in x has no linear effect on y)
# H_0: beta 1 =/= 0 (change in x has an linear effect on y)
#'\n
#' HYPOTHESES (beta 0)
# H_0: beta 1 = 0 (no true mean effect)
# H_0: beta 1 =/= 0 (some true mean effect)
#
#' SUMMARY OF ESTIMATED LINEAR REGRESSION MODEL
summary(myLinRegModel)
#' R-squared (i.e. ratio between the regression sum of squares i.e. the variation from the mean due to regression and the total sum of squares i.e. the variation of the actual data from the mean)
#' measures the proportion of variation explained by our estimated regression model.
#' The more it explains, it means the better the model fits the data
#' R-squared value is 0.2629, meaning only 26.29% of the variation is explained by our estimated regression model.\n\n
#' Pr(>|t|) is the p-value.
#' It denotes the probability that the calculated t-value (estimated for the population) is lesser than the table t-value.
#' Hence, it denotes the probability that the null hypothesis is true.\n\n
#' For intercept and x-coefficient i.e. slope, Pr(>|t|) is less than the significance level 0.05.
#' Hence, we reject the null hypotheses for both beta 1 and beta 0, and conclude that both parameters are significant, when estimated for the population.
#' In other words, we may conclude that
# There is some linear relationship between x and y in the population
# y exists in some amount, even when unaffected by x
#' These conclusions makes our estimation of their linear relationship more meaningful, since we can say (assuming the population is normally distributed)
#' with 95% confidence that a linear relationship exists.\n
#' (This means that if we take samples with similar or the same properties as the one we have taken, for around 95% of of the cases, we can expect a linear relationship between x and y in the population)

confint(myLinRegModel)

#' PERFORMING ANOVA ON REGRESSION MODEL
#
#' ANOVA is used conclude whether the means of different classes/groups regarding a certain characteristic can be said to be equal in the population,
#' and if not, which classes/groups can be said to differ significantly in terms of their means.
#' ANOVA is always calculated using a sample\n
#' In regression model, the different classes, or levels, are the different values of the independent variable in the sample, x i.e. anxiety level in this case.
#' Each x-value has two replications, namely the actual y-value and the predicted y-value.
#' If the means for all these levels i.e. x-values can be said to be equal in the population, this indicates weak or no linear relationship between x and y in the population.
#' Hence, ANOVA on a linear regression model helps decide whether the model is significant i.e. whether the variables linearly related to some degree.
#' This serves the same purpose as our previous hypothesis testing.
#' Here the hypotheses are
# H_0: Means are equal => no linear relationship => beta 1 = 0
# H_1: Means are different => some linear relationship => beta 1 =/= 0
anova(myLinRegModel)
#' Pr(>F) is the p-value.
#' It denotes the probability that the calculated F-value (estimated for the population) is lesser than the table F-value.
#' Hence, it denotes the probability that the null hypothesis is true.\n\n
#' In our case, Pr(>F) is less that the significance level 0.05.
#' Hence, we may reject H_0 and conclude that the model is significant.
#' Hence, we may conclude that the model predicts the responses to some degree.
#' In our case, that would mean that we may conclude that the satisfaction affects anxiety to some degree.