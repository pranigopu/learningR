library(samplingbook)
head(ToothGrowth)
popl = ToothGrowth$supp
popl
#' Let P be the proportion of guinea pigs with supplement type VC.
#' Let N be the population size.
N = length(popl)
P = sum(popl == "VC") / N
P
#' A sample of 8 elements is drawn
set.seed(4)
sample = sample(popl, 8)
sample
#' Let p be the estimate of P.
#' Let n be the sample size.
n = length(sample)
p = sum(sample == "VC") / n
p
#' p can be proved to be an unbiased estimator of P.
#' Hence, an unbiased estimator for the number of guinea pigs on VC supplement
#' is N * p
N * p
#' The actual value is
N * P
#' Consider pMean, which is the mean of sample proportions taken across many samples...
pMean = mean(replicate(10000, sum(sample(popl, 8) == "VC") / length(sample)))
#' As can be seen, the mean pf sample proportions tends towards the true
#' population proportion as more samples are taken.
#' This indicates unbiasedness.
#
#' Hypergeometric distribution defines the probability of there being
#' a specified number of successes in a sample taken without replcaement
#' draw from a finite population with a fixed and known number of successes.
#
#' Since p depends on the number of "successes" in the sample i.e. the number
#' of elements in the sample with supplement as VC, the probability distribution
#' of p across multiple samples follows hypergeometric distribution.
#
#' We don't need to specify this in the function to find the confidence interval,
#' of course, but it is good to know, and helps in the interpretation.
#
#' To estimate the 95% confidence interval for the value of p, we use to following...
Sprop(y = (sample == "VC"), n = n, N = N, level = 0.95)
#' Hence, we may say with 95% confidence that the sample proportion will lie between
#' 0.0411 and 0.7089. In other words, out of 100 samples, we may expect 95 samples to
#' have the sample proportion lie between 0.0411 and 0.7089. This means that
#' the estimate for the proportion of guinea pigs with VC supplement can be said to
#' be between 0.0411 and 0.7089, with 95% confidence on this estimate.
?ToothGrowth
