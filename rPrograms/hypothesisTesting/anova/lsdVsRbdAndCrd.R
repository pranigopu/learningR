# DATA SET
setwd("~/Documents/Study/computerScience/programming/r/data/")
myData = read.csv("cropYieldTruncated.csv")[-3]
head(myData)
#------------------------
# DATA USED
t = myData$fertilizer
y = myData$yield
#========================
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
#========================
# LATIN SQUARE DESIGN
#' Latin square design or LSD is an experimental design.
#' Hence, it is applied for the data collection process, not the analysis.
#' To do this, we must have at least three factors and a response.
#' For best results, all factors must have equal number of levels.
#' Two of these factors may be usable in classify the experimental units.
#' If yes, then these are the blocking factors. Together, they form a grid for experimental units to sit in.
#' The third factor is considered as the treatment, and each level is applied randomly to the experimental units so that
#' every cell of th grid i.e. blocking factor combo is subjected to every treatment at only once.
#' It is assumed that there is no interaction between any of the factors.
b1 = myData$block
b2 = myData$density
data = data.frame(y, t, b1, b2)
#------------------------
# LINEAR REGRESSION MODEL
data = data.frame(y, t, b1, b2)
model = lm(y~., data)
summary(model)
#' We see that the error degrees of freedom is 65.
error_df = 65
#------------------------
# ERROR MEAN SQUARE
# Error sum of squares...
ess = tss - rss(t, y) - rss(b1, y) - rss(b2, y)
# Error mean square...
ems_lsd = ess / error_df
ems_lsd
#========================
# RANDOMISED BLOCK DESIGN
#' We will use the same blocking factors as LSD, but will make separate RBD models for each.
#------------------------
# LINEAR REGRESSION MODEL
# Multiple linear regression model for two regressors...
model = lm(y~t + b1)
summary(model)
# Here, we see error DF as 67.
error_df = 67
#------------------------
# ERROR MEAN SQUARE FOR RBD WITH BLOCKING FACTOR 1 (b1)
# Error sum of squares...
ess = tss - rss(t, y) - rss(b1, y)
# Error mean square...
ems_rbd_b1 = ess / error_df
ems_rbd_b1
#------------------------
# ERROR MEAN SQUARE FOR RBD WITH BLOCKING FACTOR 2 (b2)
# Error sum of squares...
ess = tss - rss(t, y) - rss(b2, y)
# Error mean square...
ems_rbd_b2 = ess / error_df
ems_rbd_b2
#========================
# COMPLETELY RANDOMISED DESIGN
#------------------------
# LINEAR REGRESSION MODEL
# Multiple linear regression model for one regressor...
model = lm(y~t)
summary(model)
# Here, we see error DF as 69.
error_df = 69

# ERROR MEAN SQUARE
# Error sum of squares...
ess = tss - rss(t, y)
# Error mean square...
ems_crd = ess / error_df
ems_crd
#========================
# RELATIVE EFFICIENCIES
# LSD VS. RBD WITH BLOCKING FACTOR 1
100*(1/ems_lsd)/(1/ems_rbd_b1)
# LSD VS. RBD WITH BLOCKING FACTOR 2
100*(1/ems_lsd)/(1/ems_rbd_b2)
# LSD VS. CRD
100*(1/ems_lsd)/(1/ems_crd)
#------------------------
#' As we can see, LSD is 6.1133% and 3.5943% more efficient than the two RBD models respectively.
#' LSD is 9.4263% more efficient than CRD model.
#' Note that greater efficiency in a model means that it takes smaller samples to achieve
#' more accurate predictions and estimations (predictions about the significance of effects of the factors, and estimations about the linear regression model that gets created before the ANOVA test).