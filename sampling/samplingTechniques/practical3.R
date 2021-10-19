popl = USArrests$Murder
popl
#' QUESTION 1
#
#' Draw a random sample of size 15 using with replacement procedure and estimate the average no. of murder arrests in the US and also obtain the 95% confidence limits for this.
sample = sample(popl, 6, replace = TRUE)
#' Average number of murders, based on this sample, is
sample_mean = mean(sample)
sample_mean
#' To obtain the 95% confidence interval...
popl_var = var(popl)
popl_size = length(popl)
sample_size = length(sample)
popl_mean_square = ((popl_size) / (popl_size - 1)) * popl_var
upper = sample_mean + sqrt(((popl_size - 1) / popl_size) * popl_mean_square / sample_size) * qt(0.05, sample_size - 1, lower.tail = FALSE)
lower = sample_mean - sqrt(((popl_size - 1) / popl_size) * popl_mean_square / sample_size) * qt(0.05, sample_size - 1, lower.tail = FALSE)
# We get the confidence interval as
c(lower, upper)
#' In other words, around 95% of our estimates of the average murder arrests in USA,
#' inferred from samples of size 6, will be within the above interval.
#
#' QUESTION 2
#
#' Show that the sample mean is unbiased for population mean in case of SRSWR.
#
#' An unbiased estimator is one whose expected value is equal to the parameter.
#' To verify this, we will draw 1000,000 samples of size 50 from the population, and average their means.
sample_means = replicate(1000000, mean(sample(popl, 50, replace = FALSE)))
sample_means_mean = mean(sample_means)
popl_mean = mean(popl)

sample_means_mean
popl_mean

#' Hence, we see that the average of the means of a million samples of size 50 is very close to the population mean.
#' Hence, we may conclude that sample mean is an unbiased estimator of population mean in SRSWR.
#
#' QUESTION 3
#
#' Obtain the sampling distribution of the estimate of average no. of murder 
#' arrests in the US and represent it by a histogram.
sample_means = replicate(10000, mean(sample(popl, 30, replace = FALSE)))
hist(main = "Sampling distribution of sample means", xlab = "Sample means", ylab = "Frequency", sample_means)
#
#' QUESTION 4
#
#' Verify that the SRSWOR provides the better estimate of population mean than the
#' SRSWR (use formulae to calculate the variances in both cases, SRSWOR and SRSWR, by taking
#' a sample of same size and compare the variances).
# Variance of means taken from 100 samples with replacement...
vars_mean_srswr = var(replicate(100, mean(sample(popl, 10, replace = TRUE))))
# Variance of means taken from 100 samples without replacement...
vars_mean_srswor = var(replicate(100, mean(sample(popl, 10, replace = FALSE))))
vars_mean_srswr
vars_mean_srswor

#' As we can see, the variance of means in SRSWOR is lesser than the variance of means in SRSWR.
#' This indicates that the mean taken from samples without replacement is a more efficient estimator
#' than the mean taken from samples with replacement.