#' AIM
#
#' Given a demo of 2^2 factorial experiment using a suitable dataset and give your conclusion.
#========================
#' TWO FACTORIAL DESIGN
#
#' A two-factor factorial design is an experimental design in which data is collected for all possible combinations of the levels of the two factors of interest.
#' In our case, we also have two factors of interest, namely fertilizer type and soil density.
#' Data for crop yield is collected for all combinations of these factors' levels, hence having a two-factorial experimental design.
#========================
#' DATA SET
setwd("~/Documents/Study/computerScience/programming/r/data/")
myData = read.csv("cropYield.csv")
head(myData)
summary(myData)
x1 = myData$fertilizer
x2 = myData$density
y = myData$yield
myData = data.frame(x1, x2, y)
head(myData)
#========================
#' ANOVA
#
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
#========================
#' POST-HOC ANALYSIS
library(multcompView)
model = aov(y~x1)
TukeyHSD(model)
#' Only NP and N have significantly different mean crop yields.
#========================
#' FINAL CONCLUSIONS
# Selecting rows with fertilizer type 'NP', 'N' and 'P'
np = subset(myData, x1  == 'NP')
n = subset(myData, x1  == 'N')
p = subset(myData, x1  == 'P')
# Comparing mean crop yields 
mean(np$y)
mean(n$y)
mean(p$y)
#' As we can see, the mean crop yields for NP and N are much further apart,
#' while the mean crop yield for P lies between both, and is hence not significantly different from either one.