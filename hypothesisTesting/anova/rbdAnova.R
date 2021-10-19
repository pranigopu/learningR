setwd("~/Documents/Study/computerScience/programming/r/data/")
myData = read.csv("computersBasics.csv")[c(2, 5, 6)]
head(myData)
y = myData$price
bf = as.factor(myData$ram) # Blocking factor
#' RAM size is chosen as the blocking factor since it is generally the most significant factor in determining the price of the computer.
#' Hence, we would expect there to be substantial mean price differences between groups of computers having different RAM sizes.
tr = as.factor(myData$screen) # Treatment
myData = data.frame(tr, bf, y)
summary(myData)
#' Visualising distribution of prices w.r.t. blocks i.e. RAM sizes...
boxplot(y~bf, main = "Distribution of prices w.r.t. RAM sizes", xlab = "RAM sizes", ylab = "Price", col = "blue")
#' Here we see that each RAM has quite a visually distinct range of prices from other RAM sizes.
#' This indicates homogeneity within blocks, and heterogeneity between blocks.
#
#' Visualising distribution of prices w.r.t. treatment i.e. screen sizes...
boxplot(y~tr, main = "Distribution of prices w.r.t. screen sizes", xlab = "Screen sizes", ylab = "Price", col = "magenta")

# ANOVA
# Multiple linear regression model for two regressors...
model = lm(y~tr + bf)
# ANOVA analysis on the above model
anova(model)
#' Here we can see that both the factors are significant, since their p-values are each less than the significance level 0.05.

# POST HOC ANALYSIS
# install.packages("lsmeans")
library(lsmeans)
#' The lsmeans function from this package computes least-squares means for specified factors or factor combinations in a linear model.
#' Optionally, it makes comparisons or contrasts among them.
# Least square estimates of y averaged for blocks.
#' I.e. the total least square estimates of y for a particular block is averaged.
#' Here, we have 6 blocks (for 6 RAM sizes) So, for each level of the treatment (screen size), the estimates are added and divided by 6.
lse_over_blocks = lsmeans(model, "tr")

# Least square estimates of y averaged over levels of the treatment.
#' I.e. the total least square estimates of y for a particular treatment level is averaged.
#' Here, we have 3 treatment levels (for 3 screen sizes) So, for each level block (created by RAM size), the estimates are added and divided by 3.
lse_over_treatments = lsmeans(model, "bf")

# Finding the pairs of treatment (i.e. screen size) values with significantly different means...
pairs(lse_over_blocks)

# Finding the pairs of blocks (created by RAM size) values with significantly different means...
pairs(lse_over_treatments)
#' As expected (due to distinct ranges for each block i.e. RAM size), each block has a significantly different mean price than any other block.