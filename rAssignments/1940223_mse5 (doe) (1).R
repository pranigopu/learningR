setwd("~/Documents/Study/computerScience/programming/r/data/")
# DATA SET
myData = read.csv("cropYield.csv")
head(myData)
summary(myData)
x1 = myData$fertilizer
x2 = myData$density
y = myData$yield
myData = data.frame(x1, x2, y)
head(myData)

# HYPOTHESES
#' H_0: There is no difference in the means of fertilisers
#' H_1: Means are not equal with respect to fertiliser
#
#' H_0: There is no difference in the means of densities
#' H_1: Means are not equal with respect to density
#
#' H_0: There is no difference in the means of the interactions between density and fertiliser
#' H_1: There is difference in the means of the interactions between density and fertiliser
#
#' Significance level = 0.05
#
# VISUALISING DISTRIBUTION OF YIELD W.R.T. FACTORS
boxplot(y~x1, main = "Distribution of yield w.r.t. fertiliser type")
boxplot(y~x2, main = "Distribution of yield w.r.t. soil density")
boxplot(y~x1:x2, main = "Distribution of yield w.r.t. interaction of density & fertiliser")

# ANOVA
model = aov(y~x1+x2+x1:x2)
# x1:x2 denotes all the interactions of the levels of x1 and x2.
summary(model)

# POST-HOC ANALYSIS
library(multcompView)
model = aov(y~x1)
t = TukeyHSD(model)
t$x[,'p adj']