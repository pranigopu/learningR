# Drug type: independent variable / treatment
# Recovery time (in hours): dependent variable / response (based on the drug type)

drug1 = c(10, 25, 14, 16, 19, 19)
drug2 = c(27, 17, 18, 22, 24, 14)
drug3 = c(30, 20, 19, 19, 25, 27)
drug4 = c(17, 19, 19, 16, 17, 14)
drugTypes = c(rep(1, 6), rep(2, 6), rep(3, 6), rep(4, 6))
recoveryTimes = c(drug1, drug2, drug3, drug4)

#' To measure the likelihood of the sample of circumference values being drawn from a normally distributed population, we use the Shapiro-Wilk test.
#' The hypotheses of this test are as follows...
# H_0: Sample is drawn from a normal population
# H_0: Sample is not drawn from a normal population
#' If p-value is greater than the level of significance, we accept the null hypothesis.
#' For our purposes, let's fix the level of significance at 0.05 or 5%
#' (meaning that if the sample values are from the 5% lowest probability region of the estimated normal distribution
#' i.e. the region under the distribution curve with 5% of the values with the lowest probabilities of occuring,
#' then the sample values are considered to be out of the normal distribution i.e. not normally distributed).
shapiro.test(recoveryTimes)
#  p-value = 0.3311 > 0.05,
#' Hence, we may conclude that the sample is taken from a normally distributed population
#' (normally distributed with respect to recovery times).

#' To measure if the variances in the recovery times for each drug type is not statistically significantly different fom each other, we use Bartlett’s test.
#' This test must be used when we may conclude that the population is normally distributed (with respect to the given dependent variable).
#' The hypotheses of this test are as follows...
# H_0: Variances within each group is equal to the others
# H_1: Variances within wach group differ for at least two groups
#' In our case, a group contains the outcomes of a particular treatment.
myDataFrame = data.frame(drugTypes, recoveryTimes)
bartlett.test(recoveryTimes ~ drugTypes, myDataFrame)
#' p-value = 0.6517 > 0.05.
#' Hence, we may conclude there is close to equal variation within each group, when measured for the population.
#' Of course, we cannot measure for the population (yet), so this is an estimate, given a 5% significance level.
#' For our case, this means the variation in recovery times for each type of drug are estimated to be close to equal in the population.

#' Now, we wish to check if the experimental errors are distributed normally.
#' Experimental error in Statistics is the difference bewteen the (generally estimated) true value and the measured value of a characteristic.
#' In our case, the experimental error would be the difference between the measured recovery time for a drug type and the estimated true or mean value of the recovery time for the drug type.
#' It is akin to variation of outcomes for the same treatment, or in this case, for the same variety.
#' To test if the experimental error is normally distributed, we will first obtain a set of residuals (i.e. differences between measured and estimated true values).
#' Then, we will run them through the Shapiro-Wilk test.
e = residuals(aov(recoveryTimes ~ drugTypes, data = myDataFrame))
# "residuals" is a generic function which extracts model residuals from objects returned by modeling functions.
# Note that .anova is an attribute of a list such as circumferences, and .residual is an attribute of .anova.
# We will run the data set "circumferences.anova.residual" through the Shapiro-Wilk test.
shapiro.test(e)
#' p-value = 0.1044 > 0.05.
#' Hence, we may conclude that the experimental errors follow a normal distribution.