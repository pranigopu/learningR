#' QUESTION 2
# Sample 1
mean1 = 64.3
sd1 = 7.1
n1 = 21
# Sample 2
mean2 = 68.8
sd2 = 7.4
n2 = 23
#' Here, we have two samples taken from normal distributions.
#' Data is not given, only sample statistics.
#' Hence, these samples cannot be considered as paired since their sizes differ.
#' Additionally, we may assume that classes are not divided based on ability or inclination.
#' Then, since the subjects are the same, we can expect the overall test results to have a similar variance in each class.
#' Hence, we use pooled t-test.
#
#------------
#' AIM
#
#' We need to find out if the performance of the two classes in the same subject is equal or not, not just for these samples but in general.
#' In other words, we need to check if the populations of the respective class' results are significantly different or not.
#-----------
#' CHECKING ASSUMPTIONS
#
#' Assumption 1...
#
#' Since there is not data, we must assume the samples follow normal distribution.
#
#' Assumption 2...
#
#' Since the results of students are not affected the results of other students (we assume there is no malpractice), the samples are drawn independently.
#
#' Assumption 3...
#
#' Cannot do boxplot without data.
#
#------------
#' POOLED VARIANCE
#
sd_pooled = sqrt(((n1 - 1)*sd1*sd1 + (n2 - 1)*sd2*sd2)/(n1 + n2 - 2))
sd_pooled
#------------
#' STANDARD ERROR (of differences of sample means)
se = sd_pooled*sqrt(1/n1 + 1/n2)
se
#------------
#'POOLED T-TEST
#
#' Confidence interval of the difference of population means
# t value is for 21 + 23 - 2 = 42 degrees of freedom, and 0.05 significance level.
# Also, this is a two-tailed distribution, since we are not specifying whether the mean of sample 1 is greater or lesser than that of sample 2.
# Hence, t = 2.018
ci_upper = (mean1 - mean2) + 2.018*se
ci_lower = (mean1 - mean2) - 2.018*se
ci_upper
ci_lower
#' Hence we can say with 95% confidence that the difference between class 1's mean and class 2's mean is between -8.921118 and -0.07888159.
#' In other words, the mean of class 1's test results are between 0.5 and 9 points less than that of class 2.
#' We may say that the classes have, in general, different performances in the subject, with class 2 generally better than class 1.