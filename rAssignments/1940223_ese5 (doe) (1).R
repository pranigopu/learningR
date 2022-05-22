#' AIM
#
#' Demonstration of whether Latin Square Design (LSD) is more efficient
#' or not as compare to that of RCBD with the same experiment material using a suitable
#' dataset / example and give your conclusion.
#========================
#' DATA SET
setwd("~/Documents/Study/computerScience/programming/r/data/")
myData = read.csv("cropYield.csv")
head(myData)
#------------------------
# DATA USED
t = myData$fertilizer
y = myData$yield
#------------------------
#' The treatment and response variabes will remain the same for both experimental designs.
#' However, depending on the design, we will either choose one or two other factors whose
#' effect on the response we want to correct for, in order to get a more accurate conclusion
#' for the significance of the treatment's effects on the variation in the response.
#========================
#' FUNCTIONS USED TO CALCULATE ERROR MEAN SQUARES
# Correction factor
cf = sum(y)^2 / length(y)
#------------------------
#' Functions to calculate treament sum of squares
#
#' Regression sum of squares (for treatment or block)
#' is calculated by summing the means of the squares of the sums
#' of each of the regressor's level's replications, and then subtracting the correction factor from this value.
#' So, for treatment mean square, we would first find the sum
#' of the replications for each level at a time, square each of these sums, and divide each one by the number
#' of replications in the level. This process is achieved using the following functions...
# Level mean square
# (Used to find the mean of the squared sum of replications for each level)
lms = function(level, regressor)
{
  sum = 0
  n = 0
  for(i in c(1:(length(y))))
  {
    if(regressor[i] == level)
    {
      sum = sum + y[i]
      n = n + 1
    }
  }
  return(sum^2 / n)
}
# Regression sum of squares
rss = function(regressor)
{
  sum = 0
  levels = unique(regressor)
  for(level in levels)
  {
    sum = sum + lms(level, regressor)
  }
  return(sum - cf)
}
#------------------------
# Total sum of squares
tss = 0
for(i in y)
{
  tss = tss + i^2
}
tss = tss - cf
#------------------------
#' NOTE: Error sum of squares = Total sum of squares - (Sum of all regression sum of squares)
#=======================
#' LATIN SQUARE DESIGN
#
#' In this design, we consider two blocking factors that may effect the variance in the response.
#' This way, we can potentially minimise systematic error arising from two sources of variation other than the treatment itself.
#' This tends to make latin square design more efficient than RBD or CRD.
b1 = myData$block
b2 = myData$density
data = data.frame(y, t, b1, b2)
#------------------------
#' LINEAR REGRESSION MODEL
data = data.frame(y, t, b1, b2)
model = lm(y~., data)
summary(model)
#' We see that the error degrees of freedom is 65.
error_df = 65
#------------------------
#' ERROR MEAN SQUARE
# Error sum of squares...
ess = tss - rss(t) - rss(b1) - rss(b2)
# Error mean square...
ems_lsd = ess / error_df
ems_lsd
#========================
#' RANDOMISED BLOCK DESIGN
#
#' In this design, we will be choosing only one blocking factor, unlike two chosen for LSD.
#' Hence, we account for only one variable's effects on the response apart from the treatment,
#' potentially making it less accurate than LSD.
#' We will use the same blocking factors as LSD, but will make separate RBD models for each.
#------------------------
#' LINEAR REGRESSION MODEL
# Multiple linear regression model for two regressors...
model = lm(y~t + b1)
summary(model)
# Here, we see error DF as 67.
error_df = 67
#------------------------
#' ERROR MEAN SQUARE FOR RBD WITH BLOCKING FACTOR 1 (b1)
# Error sum of squares...
ess = tss - rss(t) - rss(b1)
# Error mean square...
ems_rbd_b1 = ess / error_df
ems_rbd_b1
#------------------------
#' ERROR MEAN SQUARE FOR RBD WITH BLOCKING FACTOR 2 (b2)
# Error sum of squares...
ess = tss - rss(t) - rss(b2)
# Error mean square...
ems_rbd_b2 = ess / error_df
ems_rbd_b2
#========================
#' RELATIVE EFFICIENCY
#
#' This compares the precision of LSD to the precision of RBD.
#' In other words, it compares how much lesser or greater the variation in experimental error is
#' in LSD compared to RBD.
#------------------------
# LSD VS. RBD WITH BLOCKING FACTOR 1
re_b1 = 100*(1/ems_lsd)/(1/ems_rbd_b1)
# Increase / descrease
re_b1 - 100
# LSD VS. RBD WITH BLOCKING FACTOR 2
re_b2 = 100*(1/ems_lsd)/(1/ems_rbd_b2)
# Increase / descrease
re_b2 - 100
#------------------------
#' As we can see, LSD is 5.6161% a more efficient than the RBD design using the 1st blocking factor.
#' However, we see that LSD is 2.033605% less efficient than the RBD design using the 2nd blocking factor.
#' This suggests that the first blocking factor is an ineffective blocking factor, since it seems to have a adverse effect
#' on the variation in experimental error, increasing it rather tha minimising it.
#' Note that greater efficiency in a model means that it takes smaller samples to achieve
#' more accurate predictions and estimations (predictions about the significance of effects of the factors, and estimations about the linear regression model that gets created before the ANOVA test).