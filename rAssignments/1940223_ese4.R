setwd("~/Documents/Study/computerScience/programming/r/data/")
data = read.csv("weatherAustralia.csv")
library(dplyr)
head(data, 5)
# Getting rid of null values for the chosen field
windgustspeeds = c()
locations = c()
popl_size = length(data$WindGustSpeed)
popl_size
for (i in c(1:popl_size))
{
  flag = 0
  if(!is.na(data$WindGustSpeed[i]))
  {
    windgustspeeds = c(windgustspeeds, data$WindGustSpeed[i])
    locations = c(locations, data$Location[i])
  }
}
popl = data.frame(windgustspeeds, locations)
head(popl, 10)

# Checking if NA values are still there
sum(is.na(windgustspeeds))
# Resetting population size
popl_size = length(windgustspeeds)
# Population total and mean
popl_total = sum(windgustspeeds)
popl_mean = popl_total/popl_size

# Dividing population into strata.
summary(data$Location)
#' We can see we have five locations.
#' We shall divide the data by the locations, since wind gust speeds are more likely to be more homogenously distributed for a single location than for multiple locations, considering the current lack of information about these locations.
#' Hence, we have five strata, as follows...
stratum1 = filter(popl, popl$locations == 1)
stratum2 = filter(popl, popl$locations == 2)
stratum3 = filter(popl, popl$locations == 3)
stratum4 = filter(popl, popl$locations == 4)
stratum5 = filter(popl, popl$locations == 5)
# Using optimum allocation to find the sample size for each stratum
sample_size = 1000
strata = c(stratum1, stratum2, stratum3, stratum4, stratum5)
N = c()
S = c()
denominator_sum = 0
for(i in c(1:5))
  {
    Nh = length(strata[2 * i - 1]$windgustspeeds)
    Sh = sd(strata[2 * i - 1]$windgustspeeds)
    denominator_sum = denominator_sum + Nh * Sh * Sh
    N = c(N, Nh)
    S = c(S, Sh * Sh)
}
print(denominator_sum)
print(N)
print(S)
n = c()
for(i in c(1:5))
{
  x = N[i] * S[i] * sample_size / denominator_sum
  n = c(n, x)
}
print(n)

# Taking samples
sample1 = sample(strata[2 * 1 - 1]$windgustspeeds, n[1], replace = FALSE)
sample2 = sample(strata[2 * 2 - 1]$windgustspeeds, n[2], replace = FALSE)
sample3 = sample(strata[2 * 3 - 1]$windgustspeeds, n[3], replace = FALSE)
sample4 = sample(strata[2 * 4 - 1]$windgustspeeds, n[4], replace = FALSE)
sample5 = sample(strata[2 * 5 - 1]$windgustspeeds, n[5], replace = FALSE)

# Estimating population mean and total
ybar1 = mean(sample1)
ybar2 = mean(sample2)
ybar3 = mean(sample3)
ybar4 = mean(sample4)
ybar5 = mean(sample5)

ybarh = c(ybar1, ybar2, ybar3, ybar4, ybar5)
print(ybarh)

# Estimated mean
ybarst = sum(Nh * ybarh) / popl_size
print(ybarst)

# Estimated population total
est_popl_total = ybarst * popl_size
print(est_popl_total)

# Confidence intervals
# Finding the bound on error of estimation for ybarst through computation...
ybarstList = c()
max = 10000
for(i in c(1:max))
{
  sample1 = sample(stratum1$windgustspeeds, n[1], replace = FALSE)
  sample2 = sample(stratum2$windgustspeeds, n[2], replace = FALSE)
  sample3 = sample(stratum3$windgustspeeds, n[3], replace = FALSE)
  sample4 = sample(stratum4$windgustspeeds, n[4], replace = FALSE)
  sample5 = sample(stratum5$windgustspeeds, n[5], replace = FALSE)
  
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
var_ybarst = var(ybarstList)

# Checking if population can be said to follow normal distribution...
hist(popl$windgustspeeds, breaks = 100)
# It is negatively skewed, hence cannot be said to follow normal distribution.

# Given that we need the 95% confidence interval, bound on error of estimate, B, is 0.05.
B = 0.05
# Using Chebychev's inequality, we have that
# P((X - mu) ≥ k*sigma) ≤ 1/k^2 ... (1)
# where X is the variable, mu is its mean, sigma is its standard deviation, and k is the number of standard deviations from the mean.
# Applying to estimated population mean and the given bound on error of estimation, we get
# # P((ybarst - popl_mean) ≥ B) ≤ 1/k^2 ... (2)
# We do this because we want to find k, that is, the number of standard deviations required so that the error of estimation i.e. p - P is under B i.e. the bound on error of estimation.
# Comparing (1) and (2), we see that
# B = k * sigma, where sigma is the standard deviation of the variable.
# But the variable is estimated population proportion, i.e. p.
# Hence, sigma = sqrt(V(ybarst))
# Hence, B = k * sqrt(V(ybarst))
# Hence, k = B / sqrt(V(ybarst))

# Variance of ybarst is given by var_ybarst

# Finding k
k = B / sqrt(var_ybarst)
print(k)

# Hence, the 95% confidence interval for estimate of Ybar i.e. population mean is
c(ybarst - k*sqrt(var_ybarst), ybarst + k*sqrt(var_ybarst))

# Now, variance of estimated population total is given by population size times variance of proportion.
# Hence, V(N_est) = N * V(ybarst)
# Similarly the 95% confidence interval for estimate of A i.e. population total is
c(est_popl_total - k*popl_size*sqrt(var_ybarst), est_popl_total + k*popl_size*sqrt(var_ybarst))