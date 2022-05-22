# DATA SETS
# Data set 1
#' It represents the sales revenues in 5 different stores, before and after an advertisement campaign.
advertEffect = 
  data.frame(
    Store = c(1:5),
    OldSalesRevenue = c(6.4, 5.3, 7.6, 5.5, 7.8),
    NewSalesRevenue = c(7.9, 6.2, 7.1, 6.9, 8.7)
  )
advertEffect
# Data set 2
#' It represents the effect of 3 different drugs on the hours of sleep.
#' Thecontrol group is the group for comparing the effects against.
drugEffect = read.csv("~/Downloads/dataset_for_Q2.csv")
drugEffect
#------------------------
# QUESTIONS FOR EACH DATA SET
# 1. Use appropriate tests to check equality of means
# 2. Explain clearly why its paired or unpaired
# 3. Do the analysis
#------------------------
# TESTS USED
#------------
# Shapiro-Wilk test:
#' It estimates if a sample's population is normally distributed
# H_0: Data is normally distributed
# H_1: Data is not normally distributed
# If p-value derived from the test > 0.05, H_0 is accepted.
#------------
# Paired t-test:
#' It checks if there is a difference between the population means of two dependent samples, together called a paired sample.
#' (A paired sample consists of two samples derived from the same set of entities/events, with the difference being in the conditions of sample collection, called intervention)
# H_0: There is no difference between the sample population means
# H_1: There is difference between the sample population means
# If p-value derived from the test > 0.05, H_0 is accepted.
#------------------------
# FOR DATA SET 1
sample1 = advertEffect$OldSalesRevenue
sample2 = advertEffect$NewSalesRevenue
#------------
# Box plot of old and new sales revenue
boxplot(
  sample1,
  sample2,
  main = "Comparing old and new sales revenues",
  names = c("Old", "New"),
  ylab = "Sales revenue"
)
#------------
# Histograms of old and new sales revenue
hist(sample1, main = 'Old sales revenue distribution')
hist(sample2, main = 'New sales revenue distribution')
#------------
# The samples are paired because
# 1. They are derived from the same entities 
#    (the same stores)
# 2. They differ due to a change in condition of sample collection 
#    (due to advertisement campaign)
#------------
# Performing Shapiro-Wilk test to confirm normality
shapiro.test(sample1)
shapiro.test(sample2)
#' In both samples, p > 0.05, hence both sample's populations may be said to follow normal distribution.
#------------
# Performing paired t-test
t.test(sample2, sample1, paired = TRUE)
# p = 0.07838 > 0.05
#' Hence, H_0 is accepted.
#' Hence, we may conclude that the advertisement campaign has no effect on the sales revenue.
#------------------------
# FOR DATA SET 2
#------------
# T-TEST BETWEEN CONTROL AND DRUG 1
sample1 = drugEffect$Control
sample2 = drugEffect$drug1
#------
# Box plot of old and new sales revenue
boxplot(
  sample1,
  sample2,
  main = "Estimating effect of drug 1",
  names = c("Control", "Affected by drug 1"),
  ylab = "Hours of sleep"
)
#------
# Histograms of control and drug 1 affected hours of sleep
hist(sample1, main = 'Distribution of hours of sleep in control group')
hist(sample2, main = 'Distribution of hours of sleep in group given drug 1')
#------
# The samples are not paired because they are derived from different entities
#------
# Performing Shapiro-Wilk test to confirm normality
shapiro.test(sample1)
shapiro.test(sample2)
#' In both samples, p > 0.05, hence both sample's populations may be said to follow normal distribution.
#------
# Performing paired t-test
t.test(sample2, sample1, paired = FALSE)
# p = 0.4007 > 0.05
#' Hence, H_0 is accepted.
#' Hence, we may conclude that the drug 1 has no effect the hours of sleep.
#------------
# T-TEST BETWEEN CONTROL AND DRUG 2L
sample2 = drugEffect$drug2L
#------
# Box plot of old and new sales revenue
boxplot(
  sample1,
  sample2,
  main = "Estimating effect of drug 2L",
  names = c("Control", "Affected by drug 2L"),
  ylab = "Hours of sleep"
)
#------
# Histograms of control and drug 2L affected hours of sleep
hist(sample1, main = 'Distribution of hours of sleep in control group')
hist(sample2, main = 'Distribution of hours of sleep in group given drug 2L')
#------
# The samples are not paired because they are derived from different entities
#------
# Performing Shapiro-Wilk test to confirm normality
shapiro.test(sample1)
shapiro.test(sample2)
#' In both samples, p > 0.05, hence both sample's populations may be said to follow normal distribution.
#------
# Performing paired t-test
t.test(sample2, sample1, paired = FALSE)
# p = 0.005076 < 0.05
#' Hence, H_0 is rejected.
#' Hence, we may conclude that the drug 2L has an effect the hours of sleep.
#' Also, mean of differences (i.e. sample2 - sample1) is 2.33.
#' Hence, we can say that drug 2L increases the hours of sleep.
#------------
# T-TEST BETWEEN CONTROL AND DRUG 2R
sample2 = drugEffect$drug2R
#------
# Box plot of old and new sales revenue
boxplot(
  sample1,
  sample2,
  main = "Estimating effect of drug 2L",
  names = c("Control", "Affected by drug 2R"),
  ylab = "Hours of sleep"
)
#------
# Histograms of control and drug 2L affected hours of sleep
hist(sample1, main = 'Distribution of hours of sleep in control group')
hist(sample2, main = 'Distribution of hours of sleep in group given drug 2R')
#------
# The samples are not paired because they are derived from different entities
#------
# Performing Shapiro-Wilk test to confirm normality
shapiro.test(sample1)
shapiro.test(sample2)
#' In both samples, p > 0.05, hence both sample's populations may be said to follow normal distribution.
#------
# Performing paired t-test
t.test(sample2, sample1, paired = FALSE)
# p = 0.01155 < 0.05
#' Hence, H_0 is rejected.
#' Hence, we may conclude that the drug 2L has an effect the hours of sleep.
#' Also, mean of differences (i.e. sample2 - sample1) is 2.32.
#' Hence, we can say that drug 2R increases the hours of sleep.
#------------
# T-TEST BETWEEN DRIG 2L AND DRUG 2R
sample1 = drugEffect$drug2L
sample2 = drugEffect$drug2R
#------
# Box plot of old and new sales revenue
boxplot(
  sample1,
  sample2,
  main = "Estimating effect of drug 2R compared to drug 2L",
  names = c("Affected by drug 2L", "Affected by drug 2R"),
  ylab = "Hours of sleep"
)
#------
# Histograms of control and drug 2L affected hours of sleep
hist(sample1, main = 'Distribution of hours of sleep in group given drug 2L')
hist(sample2, main = 'Distribution of hours of sleep in group given drug 2R')
#------
# The samples are not paired because they are derived from different entities
#------
# Performing Shapiro-Wilk test to confirm normality
shapiro.test(sample1)
shapiro.test(sample2)
#' In both samples, p > 0.05, hence both sample's populations may be said to follow normal distribution.
#------
# Performing paired t-test
t.test(sample2, sample1, paired = FALSE)
# p = 0.9902 > 0.05
#' Hence, H_0 is accepted
#' Hence, we may conclude that the drug 2R has different effect to the hours of sleep as compared to drug 2L.
#' Hence, we may say that drug 2L and drug 2R are identical in their effects on the hours of sleep.
#------------
# OVERALL CONCLUSION
#' Based on the results, we can say that while drug 1 seems to be inffective in affecting the hours of sleep, drug 2L and drug 2R do affect the hours of sleep identically.