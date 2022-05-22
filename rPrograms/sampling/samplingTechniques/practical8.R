#' Consider a dataset of your choice as a population.
#' Give the estimates for population mean, population total and their confidence interval by choosing a variable of interest from the population using systematic sampling and write a report on it.
#' Note: sample size should be appropriate enough to give the estimates.

setwd("~/Documents/Study/computerScience/programming/r/data/")
data = read.csv("weatherAustralia.csv")
head(data)

#' The data set contains information about daily weather observations accross Australia, taken for the past 10 years.
#' The target variable is RainTomorrow, or the prediction of whether or not there will be rain tomorrow, and how much.
#' I will be focussing on estimating the mean minimum temperature in Autralia.

popl = c()
popl_size = length(data$MinTemp)
for (i in c(1:popl_size))
  {
    if(!is.na(data$MinTemp[i]))
    {
      popl = c(popl, data$MinTemp[i])
    }
}

# Checking if NA values are still there
sum(is.na(popl))
# Resetting population size
popl_size = length(popl)
# Population total and mean
popl_total = sum(popl)
popl_mean = popl_total/popl_size

popl_size / 25
#' Population size is divisible by 25.
#' For our sample, we will pick the 14th element of every group of 20 elements of the population.

sample = c()
sample_size = popl_size / 25 - 1
for (i in c(0:sample_size))
{
  sample = c(sample, popl[i * 20 + 14])
}

# Resetting sample size
sample_size = length(sample)
# Sample mean
sample_mean = sum(sample) / sample_size
# Estimated population total
est_popl_total = sample_mean * popl_size

#' Hence, the estimated mean and total of the population is given by
sample_mean
est_popl_total
#' Hence, the estimated mean minimum temperature in Australia is 12.02379°C, and the estimated total of minimum temperatures is 1731126°C.
#
#' Comparing to population mean and total
popl_mean
popl_total

#' We can do this because sample mean is an unbiased estimator of population mean.
#' This implies estimated total given by population size times sample mean is an unbiased estimator of population total.
#' As we can see, the estimates are close to the actual values.
#
#' FINDING THE CONFIDENCE INTERVAL...
#
#' Finding the variance of sample mean
# V(y-bar_r) = ((N - 1)/N) * S^2 - ((n - 1)/n) * S^2_wsy
# where S^2 is the population mean square, and S^2_wsy is mean square within systematic sample.
S2 = var(popl)
S2_wsy = var(sample)
N = popl_size
n = sample_size
V_ybar_r = abs(((N - 1)/N) * S2 - ((n - 1)/n) * S2_wsy)
#' Bound on error of estimate is
B = 2 * sqrt(V_ybar_r)
#' Calculating the confidence interval for population mean estimate...
c(sample_mean - B, sample_mean + B)
#' Hence, population mean's estimate is likely to fall in this interval (75% likelihood, if population is not normal, 95% if population is normal)
#' Calculating the confidence interval for population total estimate...
c(est_popl_total - N * B, est_popl_total + N * B)
#' Hence, population total's estimate is likely to fall in this interval (75% likelihood, if population is not normal, 95% if population is normal)