#' FIND OUT THE GAIN IN EFFICIENCY IN USING STRATIFIED SAMPLING WITH PROPORTIONAL ALLOCATION OVER SRS
#
#' Efficiency of an estimator means that the variance of the estimator, measured accross multiple samples, is relatively small.
library(dplyr)

#' The population, strata, and other key values...
popl = iris
head(popl)
stra1 = filter(popl, popl$Petal.Width > 0 & popl$Petal.Width <= 0.5)
stra2 = filter(popl, popl$Petal.Width > 0.5 & popl$Petal.Width <= 1)
stra3 = filter(popl, popl$Petal.Width > 1 & popl$Petal.Width <= 1.5)
stra4 = filter(popl, popl$Petal.Width > 1.5 & popl$Petal.Width <= 2)
stra5 = filter(popl, popl$Petal.Width > 2 & popl$Petal.Width <= 2.5)

n = 60
N = length(popl$Petal.Width)

#' The following apply only for stratified sampling with proportional allocation...
#' The sizes of the various strata, denoted by Nh, h = 1, 2, 3, 4, 5...
N1 = length(stra1$Petal.Width)
N2 = length(stra2$Petal.Width)
N3 = length(stra3$Petal.Width)
N4 = length(stra4$Petal.Width)
N5 = length(stra5$Petal.Width)
Nh = c(N1, N2, N3, N4, N5)
print(Nh)

#' The appropriate sample size is given by n * Nh / N
n1 = round(n * N1 / N)
n2 = round(n * N2 / N)
n3 = round(n * N3 / N)
n4 = round(n * N4 / N)
n5 = round(n * N5 / N)
nh = c(n1, n2, n3, n4, n5)
print(nh)

#' Finding various values of ybarst, measured for 10000 samples...
ybarstList = c()
max = 10000
for(i in c(1:max))
{
  sample1 = sample(stra1$Petal.Width, n1, replace = FALSE)
  sample2 = sample(stra2$Petal.Width, n2, replace = FALSE)
  sample3 = sample(stra3$Petal.Width, n3, replace = FALSE)
  sample4 = sample(stra4$Petal.Width, n4, replace = FALSE)
  sample5 = sample(stra5$Petal.Width, n5, replace = FALSE)
  samples = c(sample1, sample2, sample3, sample4, sample5)
  
  ybar1 = mean(sample1)
  ybar2 = mean(sample2)
  ybar3 = mean(sample3)
  ybar4 = mean(sample4)
  ybar5 = mean(sample5)
  ybarh = c(ybar1, ybar2, ybar3, ybar4, ybar5)
  
  ybarst = sum(Nh * ybarh) / N
  ybarstList = c(ybarstList, ybarst)
}
#' Finding the variance of the 10000 ybarst values...
prop = var(ybarstList)

#' Finding the variance of 10000 sample means for samples taken using SRS WOR
srswor = var(replicate(max, mean(sample(popl$Petal.Width, n, replace = FALSE))))

#' Comparing the efficiency of the sample means taken using the stratified samples, and the sample means taken using SRS WOR...
if(prop > srswor) {
  print("SRS is more efficient than proportional allocation.")
  print(paste("Gain in efficiency is", 1/srswor - 1/prop))
} else if(prop < srswor) {
  print("Proportional allocation is more efficient than SRS.")
  print(paste("Gain in efficiency is", 1/prop - 1/srswor))
} else {
  print("Proportional allocation and SRS are equally efficient.")
}