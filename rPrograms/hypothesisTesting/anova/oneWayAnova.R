setwd("~/Documents/Study/computerScience/programming/r/data/")
# ESTIMATING WHICH LOCATIONS HAVE SIGNIFICANTLY DIFFERENT AVERAGE RAINFALL

# We need only the location and rainfall fields, and date fields to establish the time frame.
myData = read.csv("weatherAustralia.csv")[c(1, 2, 5)]
# Converting dates from strings to dates.
myData$Date = as.Date(myData$Date, "%d/%m/%y")
head(myData)
summary(myData)

#' Before moving ahead, it is important to note that the location is the factor, and the rainfall level depends on it (to some degree).
#' The different locations are the different levels of this factor

# Different levels of the factor "location"
levels = unique(myData$Location)
i = 1
for(level in levels)
{
  print(paste(i, ":", level))
  i = i + 1
}

# Removing NA values
max = length(myData$Location)
sum(is.na(myData$Location))
sum(is.na(myData$Rainfall))
x = y = c()
for(i in c(1:max))
{
  if(!is.na(myData$Location[i]) & !is.na(myData$Rainfall[i]))
  {
    x = c(x, myData$Location[i]);
    y = c(y, myData$Rainfall[i]);
  }
}

# Converting x to factor form
#' For the levels of the independent variable (i.e. treatment or variety) to be treated as levels, you must convert the values to factor values i.e. non-numeric.
#' This is vital, since aov calles lm, and if lm encounters the independent variables values as numeric, it will create a single linear regression model between the independent variable and the response.
#' Making the independent variable values as factors makes each lm call make a separate linear regression model for each level i.e. each value of the independent variable i.e. each class.
x = as.factor(x)

# Creating the new data frame
myData = data.frame(x, y)

# Box plot to visualise the distribution of values for each location...
boxplot(y~x)

# Creating the ANOVA model
lm(y ~ x)
aovModel = aov(y ~ x, myData)
summary(aovModel)

#' Pr(>F) is the p-value to consider here. It is the probability of the null hypothesis being true.
#' It is lower than 0.05, the significance level, hence, we reject the null hypothesis.
#' Hence, at least two locations have singificantly different mean daily rainfalls.

#install.packages("multcompView")
library(multcompView)
tukey = TukeyHSD(x = aovModel, conf.level = 0.95)
tukey
# To get p-values for each pair alone, use the extract_p function as follows...
# extract_p(tukey)

# QUICK NOTE ON P-VALUE
#' The adjusted p value is the smallest significance level
#' at which a particular comparison will be declared statistically significant as part of the multiple comparison testing.
#' For example, for the 1st pair 2-1, p adj = 0.7957830, meaning if the difference between the means of 2 and 1 is to be considered statistically significant,
#' then you must choose a significance level of at least 0.7957830.
#' However, our chosen significance level is 0.05 or 5%, way below the required minimum.
#' Hence, with 0.05 or 5% significance, the difference is not considered statistically significant
#' i.e. the difference is within the range from which the values (differences between means here) are expected to occur 95% of the time
#' (confidence interval is 95%, hence the range of 'non-outlier' values covers 95% of the most probably occurring values).
#
#' Hence, we see that the following classes (locations in this case) have significantly different means...
# 3-1
# 4-1
# 3-2
# 4-2
# 4-3
# 5-4