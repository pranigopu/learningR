setwd("~/Documents/Study/computerScience/programming/r/data/")
myData = read.csv("computersBasicsWithMissingValues.csv")[c(2, 5, 6)]
head(myData)
y = myData$price
bf = as.factor(myData$ram) # Blocking factor
#' RAM size is chosen as the blocking factor since it is generally the most significant factor in determining the price of the computer.
#' Hence, we would expect there to be substantial mean price differences between groups of computers having different RAM sizes.
tr = as.factor(myData$screen) # Treatment

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
missingValueEstimate = function(t, b, r, i)
{
  # r is the observed response values.
  # i is the index of the missing value in the response that you want to estimate.
  # t is the treatment levels corresponding to each response.
  # b is the blocks corresponding to each response.
  treatmentMean = categoryMean(t, t[i], r)
  blockMean = categoryMean(b, b[i], r)
  return((treatmentMean + blockMean) / 2)
}
getMissingValueIndices = function(r)
{
  mvi = c() # Missing value indices
  max = length(r) # Total number of observations.
  for(i in c(1:max)){if(is.na(r[i])){mvi = c(mvi, i)}}
  return(mvi)
}
missingValueEstimation = function(t, b, r)
{
  # Getting the indices of all missing values...
  mvi = getMissingValueIndices(r)
  #------------------------
  # Initial estimation for all missing values but one...
  rCopy = r # This is important, as it helps us ensure that previous initial estimates must not affect other intial estiamtes
  for(i in mvi[2:length(mvi)]){r[i] = missingValueEstimate(t, b, rCopy, i)}
  # We have left one missing value at the start.
  # A missing value is left at the start and not at the end for convenience in coding in the final estimation.
  #------------------------
  # Final estimation for all missing values...
  for(i in mvi)
  {
    r[i] = NA
    r[i] = missingValueEstimate(tr, bf, r, i)
  }
  return(r)
}
hasMissingValues = function(sov, level, r)
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
sumOfSquares = function(sovLevels, sov, r, cf)
{
  # sov is source of variation.
  # r is the observed response values.
  # mv the number of missing values.
  # sovDf is the degrees of freedom for the sov.
  # totalDf is the total degrees of freedom.
  # errorDf is the error degrees of freedom.

  # Treatment sum of squares...
  ss = 0 # Sum of squares.
  for(x in sovLevels)
  {
    n = categoryCount(sov, x) # No. of observations per level.
    ss = ss + categorySum(sov, x, r)^2 / n
  }
  print(ss)
  ss = ss - cf
  print(ss)
  return(ss)
}
pairMeanDifferences = function(sov, sovLevels, r, errSs, errDf)
{
  i = 1
  j = 1
  dStdError = 0
  while(i < length(sovLevels))
  {
    j = i + 1
    while(j <= length(sovLevels))
    {
      print(c(i, j))
      d = abs(categoryMean(sov, sovLevels[i], r) - categoryMean(sov, sovLevels[j], r))
      a = categoryCountNotNull(sov, sovLevels[i], r)
      b = categoryCountNotNull(sov, sovLevels[j], r)
      dStdError = sqrt(errSs*(1/a + 1/b))
      cd = abs(qt(0.05, errDf) * dStdError)
      print(paste(d, ">", cd, "?", d > cd))
      j = j + 1
      print("----")
    }
    i = i + 1
  }
}
#' ESTIMATING MISSING VALUES
#------------------------
# The number of missing values are...
sum(is.na(y))
#' Estimating all missing values using the above functions
y = missingValueEstimation(tr, bf, y)
# Confirming no missing values remain...
sum(is.na(y))
# Making a new data frame with these values...
newData = data.frame(tr, bf, y)
head(newData)

#' VISUALISING THE DATA
newData = data.frame(tr, bf, y)
summary(newData)
#' Visualising distribution of prices w.r.t. blocks i.e. RAM sizes...
boxplot(y~bf, main = "Distribution of prices w.r.t. RAM sizes", xlab = "RAM sizes", ylab = "Price", col = "blue")
#' Here we see that each RAM has quite a visually distinct range of prices from other RAM sizes.
#' This indicates homogeneity within blocks, and heterogeneity between blocks.
#
#' Visualising distribution of prices w.r.t. treatment i.e. screen sizes...
boxplot(y~tr, main = "Distribution of prices w.r.t. screen sizes", xlab = "Screen sizes", ylab = "Price", col = "magenta")

#' ANOVA
#------------------------
# Degrees of freedom and other basic measures...
trLevels = unique(tr)
bfLevels = unique(bf)
#------------
mvi = getMissingValueIndices(myData$price)
totalDf = length(y) - length(mvi)
trDf = length(trLevels) - 1
bfDf = length(bfLevels) - 1
errorDf = trDf*bfDf - length(mvi)
cf = sum(y)^2 / length(y)

# Total sum of squares
tss = sum(y^2) - cf

# Treatment sum of squares...
trss = sumOfSquares(trLevels, tr, y, cf)
mtrss = trss / trDf

# Block sum of squares...
bfss = sumOfSquares(bfLevels, bf, y, cf)
mbfss = bfss / bfDf

# Error sum of squares...
ess = tss - trss - bfss
mess = ess / errorDf

# F statistics...
# Calculated...
Ftr = mtrss / mess
Fbl = mbfss / mess
# Critical...
Ftr_crit = qf(0.05, trDf, errorDf)
Fbl_crit = qf(0.05, bfDf, errorDf)
# Comparison
Ftr > Ftr_crit
Fbl > Fbl_crit

#' CONCLUSIONS
#------------------------
#' Both blocking factor and treatment are estimated to have insignficant differences between means of different pairs of levels.
#------------

#' POST HOC ANALYSIS
#------------
pairMeanDifferences(tr, trLevels, myData$price, mess, errorDf)
pairMeanDifferences(bf, bfLevels, myData$price, mess, errorDf)