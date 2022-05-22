#' Latin square design or LSD is an experimental design.
#' Hence, it is applied for the data collection process, not the analysis.
#' To do this, we must have at least three factors and a response.
#' For best results, all factors must have equal number of levels.
#' Two of these factors may be usable in classify the experimental units.
#' If yes, then these are the blocking factors. Together, they form a grid for experimental units to sit in.
#' The third factor is considered as the treatment, and each level is applied randomly to the experimental units so that
#' every cell of th grid i.e. blocking factor combo is subjected to every treatment at only once.
#' It is assumed that there is no interaction between any of the factors.
#
setwd("~/Documents/Study/computerScience/programming/r/data/")
myData = read.csv("cropYieldTruncated.csv")[-3]
head(myData)
#' GOAL
#
#' Find out the significance of fertilizer type's effect on yield, with plot and soil density as the blocking factors.
#' This is to account for the effect soil density may have on the crops, and the potential unaccounted effects on the crops in different plots.
b1 = myData$block
b2 = myData$density
t = myData$fertilizer
y = myData$yield
myData = data.frame(y, t, b1, b2)
#' ANOVA TEST
model = lm(y~., myData)
aovModel = aov(model)
aovModel
#' CONCLUSIONS
#
#' Since the p-value for the treatment (fertilizer type) is below the significance level 0.05, we may reject the null hypothesis for fertilizer type.
#
#' POST HOC ANALYSIS
library(lsmeans)
lsmeans(model, "t")
pairs(lsmeans(aovModel, "t"))
#' Given a 0.05 significance level, only N and NP have significantly different means.