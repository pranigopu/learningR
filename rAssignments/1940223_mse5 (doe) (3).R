setwd("~/Documents/Study/computerScience/programming/r/data/")
myData = read.csv("cropYield.csv")
head(myData)
y = myData$yield
x = myData$fertilizer # Treatment
x_levels = length(unique(x))
total_observations = length(x)

# FUNCTIONS USED
# Correction factor
cf = sum(y)^2 / length(y)
# Treament sum of squares
lms = function(level, regressor, response)
# (Level mean square)
{
  sum = 0
  n = 0
  for(i in c(1:(length(regressor))))
  {
    if(regressor[i] == level)
    {
      sum = sum + response[i]
      n = n + 1
    }
  }
  return(sum^2 / n)
}
rss = function(regressor, response)
# (Regression sum of squares)
{
  sum = 0
  levels = unique(regressor)
  for(level in levels)
  {
    sum = sum + lms(level, regressor, response)
  }
  return(sum - cf)
}
# Total sum of squares
tss = 0
for(i in y)
{
  tss = tss + i^2
}
tss = tss - cf
#------------------------------------------------
# FOR RBD
bf = myData$density # Blocking factor
bf_levels = length(unique(bf))
#' Soil density size is chosen as the blocking factor since it may be a significant factor in affecting yields (apart from fertilizer type).
myData = data.frame(x, bf, y)
head(myData)
summary(myData)
#' Visualising distribution of yields w.r.t. treatment i.e. fertilizer types...
boxplot(y~x, main = "Distribution of yields w.r.t. fertilizer types", xlab = "Fertilizer", ylab = "Yield", col = "blue")
#
#' Visualising distribution of yields w.r.t. blocks i.e. soil density
boxplot(y~bf, main = "Distribution of yields w.r.t. soil density", xlab = "Soil density", ylab = "Yield", col = "magenta")

# LINEAR REGRESSION MODEL
# Multiple linear regression model for two regressors...
model = lm(y~x + bf)
summary(model)
# Here, we see error DF as 67.
error_df = 67

# ERROR MEAN SQUARE
# Error sum of squares...
ess = tss - rss(x, y) - rss(bf, y)
# Error mean square...
ems_rbd = ess / error_df
#------------------------------------------------
# FOR CRD
myData = data.frame(x, y)
summary(myData)
#' Visualising distribution of yields w.r.t. treatment i.e. fertilizer types...
boxplot(y~x, main = "Distribution of yields w.r.t. fertilizer types", xlab = "Fertilizer", ylab = "Yield", col = "blue")

# LINEAR REGRESSION MODEL
# Multiple linear regression model for two regressors...
model = lm(y~x)
summary(model)
# Here, we see error DF as 69.
error_df = 69

# ERROR MEAN SQUARE
# Error sum of squares...
ess = tss - rss(x, y)
# Error mean square...
ems_crd = ess / error_df
#------------------------------------------------
# RELATIVE EFFICIENCY
# EMS of CRD
ems_crd
# EMS of RBD
ems_rbd
# Relative efficiency of RBD w.r.t. CRD
re = 100* (1/ems_rbd) / (1/ems_crd)
re
#------------------------------------------------
#' Hence, we see that in this case, RBD has proved to be around 5.6296% more efficient than CRD.