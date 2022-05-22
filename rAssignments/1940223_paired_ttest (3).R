#' QUESTION 3
data = c(116, 111, 101, 120, 99, 94, 106, 115, 107, 101, 110, 92)
#' Here, we have a sample of IQ test results for a group of people given fish oil supplement for one year.
#' Since there is only one sample, we do a one sample t-test.
#
#------------
#' AIM
#
#' We need to find out if the mean of the population of people given the fish oil supplement for one year would equal 100.
#-----------
#' CHECKING ASSUMPTIONS
#
#' Assumption 1...
#
shapiro.test(data)
#' We see that p = 0.9103> 0.05, hence the sample can be said to be taken from a normal distribution.
#
#' Assumption 2...
#
#' Since the IQ results of one person are not affected by the IQ results of other people (we assume there is no malpractice), the samples are drawn independently.
#
#' Assumption 3...
#
boxplot(data, xlab = "IQ results")
#' No outliers.
#
#------------
#' ONE SAMPLE T-TEST
#
t.test(data, mu = 100)
#' Here we see that p = 0.03826 < 0.05, hence we cannot say that the sample's population's mean is not 100.
#' Furthermore, the 95% confidence interval of the population mean is (100.3886, 111.6114), which does not contain 100.