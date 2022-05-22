setwd("~/Documents/Study/computerScience/programming/r/data/")
# DATA SET
myData = read.csv("cropYield.csv")
head(myData)
summary(myData)
x = myData$fertilizer
y = myData$yield
myData = data.frame(x, y)
head(myData)
#' ASSUMPTION 1: Response if normally distributed in the population
# Hypotheses:
# H_0: Sample is drawn from a normal population
# H_0: Sample is not drawn from a normal population
#' If p-value is greater than the level of significance, we accept the null hypothesis.
#' For our purposes, let's fix the level of significance at 0.05 or 5%
#' (meaning that if the sample values are from the 5% lowest probability region of the estimated normal distribution
#' i.e. the region under the distribution curve with 5% of the values with the lowest probabilities of occuring,
#' then the sample values are considered to be out of the normal distribution i.e. not normally distributed).
shapiro.test(y)
#  p-value = 0.3311 > 0.05,
#' Hence, we may conclude that the sample is taken from a normally distributed population
#' (normally distributed with respect to yield).

#' ASSUMPTION 2: Variances of responses of each treatment level are equal in the population
# (This test must be used when we may conclude that the population is normally distributed (with respect to the given dependent variable)
# The hypotheses of this test are as follows...
# H_0: Variances within each group is equal to the others
# H_1: Variances within wach group differ for at least two groups
#' In our case, a group contains the outcomes of a particular fertilizer.
bartlett.test(y ~ x, myData)
#' p-value = 0.6517 > 0.05.
#' Hence, we may conclude there is close to equal variation within each group, when measured for the population.
#' Of course, we cannot measure for the population (yet), so this is an estimate, given a 5% significance level.
#' For our case, this means the variations in yield for each type of fertilizer's application are estimated to be close to equal in the population.

#' ASSUMPTION 3: Experimental error is normally distributed
#
#' Experimental error in Statistics is the difference bewteen the (generally estimated) true value and the measured value of a characteristic.
#' In our case, the experimental error would be the difference between the measured recovery time for a drug type and the estimated true or mean value of the recovery time for the drug type.
#' It is akin to variation of outcomes for the same treatment, or in this case, for the same variety.
#' To test if the experimental error is normally distributed, we will first obtain a set of residuals (i.e. differences between measured and estimated true values).
#' Then, we will run them through the Shapiro-Wilk test.
e = residuals(aov(y ~ x, data = myData))
# "residuals" is a generic function which extracts model residuals from objects returned by modeling functions.
# Note that .anova is an attribute of a list such as circumferences, and .residual is an attribute of .anova.
# We will run the data set "circumferences.anova.residual" through the Shapiro-Wilk test.
shapiro.test(e)
#' p-value = 0.1044 > 0.05.
#' Hence, we may conclude that the experimental errors follow a normal distribution.