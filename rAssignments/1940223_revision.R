#' 1. INDEPENDENT SAMPLE T-TEST
#
#' Data set
# Description...
#' This data set records the weights of a sample of 10 tigers.
setwd("~/Documents/Study/computerScience/programming/r/data/")
myData = read.csv("tigerWeights.csv")
myData
#' ------------
# Since we do not know the population standard deviation, we use the t-test to help us determine the population mean.
#' ------------
#
#' 1.1. SHAPIRO-WILK TEST
# The assumption of a t-test is that a sample's parent population is normally distributed.
# The assumption of the t-test here is that the weight of the tigers throughout the world are normally distributed.
# We use the Shapiro-Wilk test to check the likelihood of the sample being derived from a population following normal distribution.
shapiro.test(myData$Weight)
#' According to the test, since p = 0.06822 > 0.05, we may conclude that the sample is derived from a normally distributed population.
#
#' ------------
#
#' 1.2. T-TEST
# Finding the 95% confidence interval of the mean weight for the whole population of tigers...
t.test(myData$Weight)
#' According to the t-test, we can say with 95% confidence that the mean weight of tigers throughout the world lies between 117.5491 kg and 230.2509 kg.
#' Since we may conclude that the sample's population is normally distributed, and since the t-test is itself robust, there is a good chance of the confidence interval being accurate.
#
#' ------------------------------------
#
#' 2. PAIRED SAMPLE T-TEST
#
#' Data set
# Description...
#' This data set records the maximum running distances before and after a certain training.
#' Before training distances are given in PrevMaxDist.
#' After training distances are given in CurMaxDist.
myData = read.csv("runnerTrainingResults.csv")
myData
#' ------------
# Since we do not know the population standard deviation, we use the t-test to help us determine the population mean.
#' ------------
#
#' 2.1. SHAPIRO-WILK TEST
# The assumption of a t-test is that each sample's parent population is normally distributed.
# The assumption of the t-test here is that the following populations are normally distributed
# 1. max distances run by people throughout the world without the training
# 2. max distances run by people thoughout the world after training (projected)
# We use the Shapiro-Wilk test to check the likelihood of the sample being derived from a population following normal distribution.
shapiro.test(myData$PrevMaxDist)
shapiro.test(myData$CurMaxDist)
#' According to the test, since in both cases, p > 0.05, we may conclude that both samples' parent populations follow normal distribution.
#
#' ------------
#
#' 2.2. PAIRED T-TEST
# Finding if the training has significant effects on the max distance run by a person...
t.test(myData$PrevMaxDist, myData$CurMaxDist, paired = TRUE)
#' According to the t-test, given that p = 0.03527 < 0.05, we may conclude that the training does not have a significant effect on a person's maximum running distance.
#' The confidence interval of the difference of means between the populations of max running distances before and after training is (-2.5487042, -0.1179624).
#' Since the order is (PrevMaxDist, CurMaxDist), we can say from the confidence interval that the training may have some positive effect, but seeing the actual values and given the p-value, this positive effect is not significant.