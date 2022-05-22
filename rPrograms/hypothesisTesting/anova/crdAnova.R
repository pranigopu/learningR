# ESTIMATING WHICH SCREEN SIZES HAVE SIGNIFICANTLY DIFFERENT AVERAGE PRICES
library(dplyr)
# CRD
#' CRD i.e. completely randomised design is the simplest experimental design.
#' It involves the random assignment of treatments on a random sample of relatively homogenous units.

# DATA
setwd("~/Documents/Study/computerScience/programming/r/data/")
myData = read.csv("computersBasics.csv")[c(1, 2, 5, 6)]
# Only including relevant columns.
myData$screen = as.factor(myData$screen)
# Converted screen size to factor type for future purposes.
head(myData)
summary(myData)
#' I want to find out how a computer's screen size can affect the computer's price on average.
#' Since RAM size is generally a significant factor in the price of the computer,
#' we will keep the data relatively homogenous by focussing on only one RAM size, 16GB.
#' This is done because CRD is most effective for homogenous data, and not very accurate otherwise.
myData = filter(myData, myData$ram == 16)
# (Only including the required columns)
myData$screen = as.factor(myData$screen) # Converting x values to factor values, as they should be.
head(myData)

# Visualising the distribution of data based on screen size...
boxplot(myData$price~myData$screen)

# CREATING MODEL FOR ANOVA TEST
aovModel = aov(price ~ screen, myData)
summary(aovModel)

#' Pr(>F) is the p-value to consider here. It is the probability of the null hypothesis being true.
#' It is lower than 0.05, the significance level, hence, we reject the null hypothesis.
#' Hence, at least two screen sizes have singificantly different mean prices.

# POST HOC ANALYSIS
#install.packages("multcompView")
library(multcompView) 
tukey = TukeyHSD(x = aovModel, conf.level = 0.95)
# Only including the p value and converting to data frame for neater view...
tukey = as.data.frame(tukey$screen[,'p adj'])
names(tukey)[1] = paste("p adj")
tukey