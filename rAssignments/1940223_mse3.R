data = USArrests
head(data, 10)
rape = data$Rape
murder = data$Murder
#' DATASET DESCRIPTION
#
#' This data set contains statistics, in arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973. Also given is the percent of the population living in urban areas. A data frame with 50 observations on 4 variables.
#' The variable "rape" contains the rapes per state, while the variable "murder" contains the murders per state.
#
#' QUESTION 1
#
#' Consistency is the property of an estimator wherein the value of the estimator indefinitely approaches the parameter as the sample size gets indefinitely large.
#' In other words, a consistent estimator is an estimator wherein the probability of the estimator equalling the parameter approaches 1 as the sample size approaches infinity.
#' In other words, the estimator becomes indefinitely more accurate with larger and larger sample sizes.
#
#' Choose parameter as population mean and choose estimator as sample mean.
#
#' Let the population be the data of the column "Murder"
population_mean = mean(murder)
population_mean
for(i in c(50, 100, 500, 1000, 2500, 5000))
    {
      sample_means = replicate(10, mean(sample(murder, size = i, replace = TRUE)))
      print(abs(population_mean - mean(sample_means)))
    }
#' As can be seen, the difference between the population mean and the sample mean generally gets closer to 0 as the sample size gets larger.
#' Note that this is an overall tendency, and may not be true for a particular pair of samples, where one is smaller.
#' This indicates that sample mean is a consistent estimator of population mean.
#
#' QUESTION 2
#
#' We only have the murders per 100,000 people for 1973.
#' Take the population as the number of murders per 100,000 people in USA states for all time.
#' We will try to test whether the average murders per 100,000 people in USA states for all time equals 8, the closest integer to the sample mean.
#
#' Now, we do not knw the standard deviation of the population.
#' Hence, we use the t-test on the sample mean to estimate the population mean.
#' The assumptions of a one sample t-test is as follows...
#  1. The population follows normal distribution
#  2. The sample values are independent (i.e. one does not affect the other)
#  3. There are no outliers.
#' The hypotheses of one sample t-test are...
#  H_0: Population mean is 8
#  H_1: Population mean is not 8
#' If p > 0.05, we accept H_0.
#
#' For testing normality of the population using the sample, we use the Shapiro-Wilk test.
#' The hypotheses of the Shapiro-Wilk test are as follows...
#  H-0: The sample's population follows normal distribution
#  H_1: The sample's population does not follow normal distribution
#' If p > 0.05, we accept H_0.
shapiro.test(murder)
#' p = 0.06674 > 0.05.
#' Hence, we may say that the sample's population i.e. the murders per 100,000 people for all time follows normal distribution.
#
#' Now, performing the t-test...
t.test(murder, mu = 8)
#' p = 0.7322 > 0.05.
#' Hence, we accept H_0.
#' Hence, we may say that the average murders per 100,000 people in USA states for all time equals 8.
#' Note that it is not wise to estimate values with data from only one year, because different years may have very different conditions that could affect crime rates.
#
#' QUESTION 3
#
# Let P_1 denote the set of rapes per 100,000 people in USA states for all time
# Let P_2 denote the set of murders per 100,000 people in USA states for all time
#' We want to find out whether P_1 is similar to P_2
#' (note that these two sets of measures are the populations).
#' To do this, we use the data of 1973 as our sample.
#' Using the rates of murder and rape per 100,000 people given in the dataset, we estimate the difference of the means of P_1 and P_2, and test whether the difference is significant or not.
#
#' Since we don't know the population standard deviations, we use two sample t-test.
#' Also, since murder and rape are different crimes, committed by different people in various conditions, we cannot consider them paired.
#
#' To check whether the corresponding populations of the samples can be said to have equal variances, we use the F-test.
#' The F-test has the following hypotheses...
#  1. The variances of the two populations are equal
#  2. The variances of the two populations are not equal
#' If p > 0.05, we accept H_0.
var.test(murder, rape)
#' p = 3.347e-07 = 0.0003347 < 0.05.
#' Hence we reject H_0 i.e. we take the two populations to have unequal variances.
#
#' Now, two sample t-test for unequal variances has the following assumptions...
#  1. The populations follow normal distribution
#  2. The samples are independent and random
#  3. There are no outliers
#' The hypotheses of thw two sample t-test are...
#  H_0: The difference of the two populations' means is 0
#  H_1: The difference of the two populations' means is not 0
#' i.e. H_0 suggests the populations are similar enough to be equal, while H_1 is the contrary.
#' If p > 0.05, we accept H_0.
#
#' First checking for normality using the Shapiro-Wilk test (discussed in question 2)...
shapiro.test(murder)
#' p = 0.06674 > 0.05
shapiro.test(rape)
#' p = 0.0251 < 0.05
#
#' Hence, while the population for murders may be said to follow normal distribution, the population for rapes may not be said to follow normal distribution.
#' We proceed with the test anyway, since it is a robust test, keeping in mind that the results may be inaccurate due to all the assumptions not being fulfilled.
#
#' The two sample t-test is as follows...
t.test(murder, rape, var.equal = FALSE, paired = FALSE)
#' p = 1.237e-13 < 0.05.
#' Hence, we reject H_0.
#' Hence, we may say that P_1 and P_2 are significantly different
#' i.e. we may say that the no. of murders for each state is significantly different from the no. of rapes for each state, for all time.