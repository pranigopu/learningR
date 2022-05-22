setwd("/Users/pranav/Documents/Study/computerScience/programming/r/data/")
# install.packages('plyr')
library(plyr)
# Function to check if value lies in a certain range
inRange = function(x, min, max)
# min is included from range while max is excluded
{
  if(x >= min & x < max) {TRUE} else {FALSE}
}

# Function to return the class corresponding to the value
# (based on the range and divisions)
returnClass = function(x, min, max, divisions)
# min is included from range while max is excluded
{
  step = (max - min) / divisions
  i = 1
  while(min < max)
  {
    if(inRange(x, min, min + step)){break}
    min = min + step
    i = i + 1
  }
  i
}

# THE DATA
originalData = read.csv("weatherAustralia.csv")[c(2, 4, 5)]
summary(originalData)
maxObservations = length(originalData$Location)
minRainfall = min(originalData$Rainfall,  na.rm = TRUE)
maxRainfall = max(originalData$Rainfall,  na.rm = TRUE) + 1
x1 = x2 = y = c()
# Getting an idea for the distribution of rainfall values
hist(originalData$Rainfall, main = "Frequency distribution of rainfall levels", xlab = "Rainfall", ylab = "Frequency")

# REMOVING NULL VALUES
for(i in c(1:maxObservations))
{
  location = originalData$Location[i]
  rainfall = originalData$Rainfall[i]
  maxTemp = originalData$MaxTemp[i]
  if(!is.na(location) & !is.na(rainfall) & !is.na(maxTemp))
  {
       x1 = c(x1, as.factor(location))
       x2 = c(x2, returnClass(rainfall, minRainfall, maxRainfall, 20))
       y = c(y, maxTemp)
  }
}
x1 = as.factor(x1)
x2 = as.factor(x2)
myData = data.frame(x1, x2, y)
head(myData)
# Visually verifying the distribution of rainfall levels
barplot(count(x2)$freq, main = "Frequency distribution of rainfall levels", xlab = "Rainfall", ylab = "Frequency")
# Visually comparing maxTemp distribution w.r.t. location
boxplot(y~x1, main = "Distribution of maximum temperature by location", xlab = "Location", ylab = "Max. temperature")
# Visually comparing maxTemp distribution w.r.t. rainfall level
boxplot(y~x2, main = "Distribution of maximum temperature by rainfall level", xlab = "Rainfall level", ylab = "Max. temperature")

# install.packages("lsmeans")
library(lsmeans)
#' The lsmeans function from this package computes least-squares means for specified factors or factor combinations in a linear model.
#' Optionally, it makes comparisons or contrasts among them.

# ANOVA
# Multiple linear regression model for two regressors...
model = lm(y~x1 + x2)
summary(model)
#' Here, we see the estimates for the intercept and coefficients of the interactions of x1 and x2.
anova(model)
#' Here we can see that both the factors are significant, since their p-values are each less than the significance level 0.05.

# POST HOC ANALYSIS
# Least square estimates of y averaged over levels of x2
#' I.e. the total least square estimates of y for a particular level of x1 is averaged.
#' Here, x2 has 20 levels. So, for each level of x1, the estimates are added and divided by 20.
lse_over_x2 = lsmeans(model, "x1")

# Least square estimates of y averaged over levels of x1
#' Same principle as the last one.
lse_over_x1 = lsmeans(model, "x2")

# Least square estimates of y averaged
# (for all replications of each interaction of x1 and x2)
lse_over_interactions = lsmeans(model, c("x1", "x2"))

# Finding the pairs of x1 values (locations) with significantly different means
pairs(lse_over_x2)

# Finding the pairs of x2 values (rainfall) with significantly different means
pairs(lse_over_x1)

# Finding the pairs of interactions with significantly different means
pairs(lse_over_interactions)

# ALTERNATE APPROACH TO TWO-WAY ANOVA
# library(multcompView)
# model = aov(y~x1 + x2 + x1:x2, myData)
# print(model)
#' A balanced design is an experimental design where all cells (i.e. treatment combinations) have the same number of observations.
#' This does not seem to be the case for us.

# model = aov(y~x1 + x2, myData)
# Since interaction effect is statistically insignificant, we exclude it.
# tukey = TukeyHSD(x = model, conf.level = 0.95)
# tukey