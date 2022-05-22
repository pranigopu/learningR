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
fertilizer = c()
irrigation = c()
yield = c()
for(i in c(1:length(myData$fertilizer)))
{
  if(myData$fertilizer[i] != 'NP')
  {
    fertilizer = as.factor(c(x1, myData$fertilizer[i]))
    irrigation = as.factor(c(x2, myData$irrigation[i]))
    yield = c(y, myData$yield[i])
  }
}
myData = data.frame(fertilizer, irrigation, yield)
head(myData)
summary(myData)
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
boxplot(yield~fertilizer, main = "Distribution of yield w.r.t. fertiliser type")
boxplot(yield~irrigation, main = "Distribution of yield w.r.t. irrigation")
boxplot(yield~fertilizer:irrigation, main = "Distribution of yield w.r.t. interaction of irrigation & fertiliser")

# ANOVA
model = aov(yield~fertilizer+irrigation+fertilizer:irrigation)
# x1:x2 denotes all the interactions of the levels of x1 and x2.
summary(model)

# POST-HOC ANALYSIS
library(multcompView)
TukeyHSD(model)
