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
categoryCount = function(searchRange, category)
{
  return(sum(searchRange == category))
}
sumOfSquares = function(sovLevels, sov, r, cf)
{
  # sov is the data of the source of variation.
  # r is the observed response values.
  
  # Treatment sum of squares...
  ss = 0 # Sum of squares.
  for(x in sovLevels)
  {
    n = categoryCount(sov, x) # No. of observations per level.
    ss = ss + categorySum(sov, x, r)^2 / n
  }
  ss = ss - cf
  return(ss)
}
errorMeanSquared_rbd = function(t, b, r, errorDf)
{
  # b is the blocking factor data.
  # t is the treatment data.
  # r is the response variable data.
  #========================
  # Levels and degrees of freedom
  tLevels = unique(t)
  tDf = length(tLevels) - 1
  #------------
  bLevels = unique(b)
  bDf = length(bLevels) - 1
  #------------
  totalDf = length(r) - 1
  #========================
  # To calculate error mean square for rows or columns in LSD
  #------------
  cf = sum(r)^2 / length(r)
  trss = sumOfSquares(tLevels, t, r, cf) # Treatment sum squared
  bss = sumOfSquares(bLevels, b, r, cf) # Block sum squared
  tss = sum(r^2) - cf # Total sum squared
  ess = tss - bss - trss # Error sum squared
  #------------
  mbss = bss / tDf
  mess = ess / errorDf
  #------------
  mess_other = (bDf*mbss + (tDf + errorDf)*mess) / (bDf + tDf + errorDf)
  return(mess_other)
}
relativeEfficiency_rbd = function(t, b, r, mess_lsd, tDf, bDf, oDf)
{
  # b is the blocking factor data.
  # t is the treatment data.
  # r is the response variable data.
  # bDf is the degrees of freedom of the blocking factor.
  # oDf is the degrees of freedom of the other blocking factor in the latin squared design.
  #========================
  errorDf_lsd = tDf*bDf*oDf
  errorDf_rbd = tDf*bDf
  mess_rbd = errorMeanSquared_rbd(t, b, y, errorDf_lsd)
  re = 100 * mess_rbd / mess_lsd
  pf = 1
  if(errorDf_lsd < 20)
  {
    pf = (errorDf_lsd + 1)*(errorDf_rbd - 3) / (errorDf_rbd + 1)*(errorDf_lsd - 3)
  }
  
  return(re*pf)
}
#' DATA SET
setwd("~/Documents/Study/computerScience/programming/r/data/")
myData = read.csv("cropYieldTruncated.csv")[-3]
head(myData)

#' LSD VS RBD
b1 = myData$block
b2 = myData$density
t = myData$fertilizer
y = myData$yield
#========================
# Levels and degrees of freedom
tLevels = unique(t)
tDf = length(tLevels) - 1
#------------
b1Levels = unique(b1)
b1Df = length(b1Levels) - 1
#------------
b2Levels = unique(b2)
b2Df = length(b2Levels) - 1
#------------
errorDf_lsd = tDf*b1Df*b2Df
#------------
cf = sum(y)^2 / length(y)
cf
#========================
tss = sum(y^2) - cf # Total sum of squares
b1ss = sumOfSquares(unique(b1), b1, y, cf) # Block b1 sum of squares
b2ss = sumOfSquares(unique(b2), b2, y, cf) # Block b2 sum of squares
trss = sumOfSquares(unique(t), t, y, cf) # Treatment sum of squares
ess_lsd = tss - b1ss - b2ss - trss
mess_lsd = ess_lsd / errorDf_lsd

#' RELATIVE EFFICIENCY WITH RBD
#
#' Relative efficiency of LSD w.r.t. RBD
relativeEfficiency_rbd(t, b1, y, mess_lsd, tDf, b1Df, b2Df)

#' Error mean square with RBD where b2 is the blocking factor
relativeEfficiency_rbd(t, b2, y, mess_lsd, tDf, b1Df, b2Df)

#' RELATIVE EFFICIENCY WITH CRD
ess_crd = tss - trss
mess_crd = ess_crd / (length(y) - length(tLevels))
a = b1Df*sumOfSquares(b1Levels, b1, y, cf)
a = a + b2Df*sumOfSquares(b2Levels, b2, y, cf)
a = a + (tDf + errorDf_lsd)*mess_crd
a = a / (tDf + b1Df + b2Df + errorDf_lsd)
re = 100 * a / mess_lsd
pf = 1
if(errorDf_lsd < 20)
{
  errorCrd = b1Df + b2Df + errorDf_lsd
  pf = (errorDf_lsd + 1)*(errorCrd - 3) / (errorCrd + 1)*(errorDf_lsd - 3) 
}
re = re * pf
re