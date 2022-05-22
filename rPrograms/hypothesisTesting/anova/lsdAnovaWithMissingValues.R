#' Latin square design or LSD is an experimental design.
#' Hence, it is applied for the data collection process, not the analysis.
#' To do this, we must have at least three factors and a response.
#' For best results, all factors must have equal number of levels.
#' Two of these factors may be usable in classify the experimental units.
#' If yes, then these are the blocking factors. Together, they form a grid for experimental units to sit in.
#' The third factor is considered as the treatment, and each level is applied randomly to the experimental units so that
#' every cell of th grid i.e. blocking factor combo is subjected to every treatment at only once.
#' It is assumed that there is no interaction between any of the factors.
#------------------------
setwd("~/Documents/Study/computerScience/programming/r/data/")
myData = read.csv("computersBasicsWithMissingValues.csv")
head(myData)
y = myData$price
bf_1 = as.factor(myData$ram) # Blocking factor
#' RAM size is chosen as the blocking factor since it is generally the most significant factor in determining the price of the computer.
#' Hence, we would expect there to be substantial mean price differences between groups of computers having different RAM sizes.
bf_2 = myData$premium
#' Whether or not a model is premium obviously affects its price.
#' Hence, this is the second blocking factor.
tr = as.factor(myData$screen) # Treatment
#========================
#' FUNCTIONS
categorySum = function(searchRange, category, sumRange)
{
  n = 1
  sum = 0
  for(x in searchRange)
  {
    if(x == category & !is.na(sumRange[n])){sum = sum + sumRange[n]}
    n = n + 1
  }
  return(sum)
}
categoryMean = function(searchRange, category, sumRange)
{
  n = 1
  sum = 0
  notNullCount = 0
  for(x in searchRange)
  {
    if(x == category & !is.na(sumRange[n]))
    {
      sum = sum + sumRange[n]
      notNullCount = notNullCount + 1
    }
    n = n + 1
  }
  return(sum/notNullCount)
}
categoryCount = function(searchRange, category)
{
  return(sum(searchRange == category))
}
categoryCountNotNull = function(searchRange1, category, searchRange2)
{
  return(sum(searchRange1 == category & !is.null(searchRange2)))
}
missingValueEstimate = function(t, b1, b2, r, i)
{
  # r is the observed response values.
  # i is the index of the missing value in the response that you want to estimate.
  # t is the treatment levels corresponding to each response.
  # b1 is the 1st category of blocks corresponding to each response.
  # b2 is the 2nd category of blocks corresponding to each response.
  treatmentMean = categoryMean(t, t[i], r)
  block1_mean = categoryMean(b1, b1[i], r)
  block2_mean = categoryMean(b2, b2[i], r)
  return((treatmentMean + block1_mean + block2_mean) / 2)
}
getMissingValueIndices = function(r)
{
  mvi = c() # Missing value indices
  max = length(r) # Total number of observations.
  for(i in c(1:max)){if(is.na(r[i])){mvi = c(mvi, i)}}
  return(mvi)
}
missingValueEstimation = function(t, b1, b2, r)
{
  # Getting the indices of all missing values...
  mvi = getMissingValueIndices(r)
  #------------------------
  # Initial estimation for all missing values but one...
  rCopy = r # This is important, as it helps us ensure that previous initial estimates must not affect other intial estiamtes
  for(i in mvi[2:length(mvi)]){r[i] = missingValueEstimate(t, b1, b2, rCopy, i)}
  # We have left one missing value at the start.
  # A missing value is left at the start and not at the end for convenience in coding in the final estimation.
  #------------------------
  # Final estimation for all missing values...
  for(i in mvi)
  {
    r[i] = NA
    r[i] = missingValueEstimate(tr, b1, b2, r, i)
  }
  return(r)
}
hasMissingValues = function(sov, r)
{
  i = 1
  for(x in sov)
  {
    if(is.na(r[i]))
    {
      return(TRUE)
    }
    i = i + 1
  }
  return(FALSE)
}
#========================
#' MISSING VALUE ESTIMATION
if(hasMissingValues(tr, y))
{
  #------------------------
  #' Getting all the missing values' indices
  mvi = getMissingValueIndices(y)
  #------------------------
  #' Showing all the missing value rows
  for(i in mvi)
  {
    print(paste("Index", i, ", tr:", tr[i], ", bf_1:", bf_1[i], ", bf_2:", bf_2[i]))
    # Note that tr is screen size, bf_1 is RAM capacity and bf_2 is whether or not the product is premium.
  }
  #------------------------
  #' Estimating the missing values
  y = missingValueEstimation(tr, bf_1, bf_2, y)
  #------------------------
  #' Showing all the estimated missing values
  for(i in mvi)
  {
    print(paste("Index", i, ", y:", y[i]))
    # Note that tr is screen size, bf_1 is RAM capacity and bf_2 is whether or not the product is premium.
  }
}
#========================
#' SPECIFIC DATA FRAME FOR THE CHOSEN FIELDS (INCLUDING ESTIMATED MISSING VALUES)
data = data.frame(tr, bf_1, bf_2, y)
head(data)
#========================
#' ANOVA TEST
#------------------------
#' Hypotheses (for all factors in general)
# H_0: Factor's level's means are not significantly different
# H_1: Factor's level's means are significantly different
#------------------------
model = lm(y~., data)
aovModel = aov(model)
summary(aovModel)
#' As we can see, all factors, including the blocking factors, have significant impact on prices.
#========================
#' CONCLUSIONS
#------------------------
#' Since the p-value for the treatment (screen size) is below the significance level 0.05, we may reject the null hypothesis for screen size.
#' Hence, we must perform a post-hoc analysis to determine which levels of the treatment i.e. which screen sizes have significantly different mean prices.
#========================
#' POST HOC ANALYSIS
library(lsmeans)
pairs(lsmeans(aovModel, "tr"))
#' Given a 0.05 significance level, only screen sizes 14 and 15 don't have significantly different price levels.