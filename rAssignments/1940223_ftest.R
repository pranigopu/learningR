#' F TEST
#
#' F test is a test to check the equality in the variances of two populations, given the samples from these populations.
#' The hypotheses of these tests are
# H_0: The variances of these populations are equal
# H_1: The variances of these populations are unequal (with some type of inequality)
#' F test's main assumption is that the populations follow normal distributions.
#
#' QUESTION 1
#
#' Generate/ enter 2 samples (remember to use rnorm if generating) and test for equality of variances.
#
#' The samples are
q1_data1 = rnorm(30, mean = 5, sd = 6)
q1_data2 = rnorm(20, mean = 3, sd = 8)
q1_data1
q1_data2
#' We know that the populations are derived from a normal distribution.
#' Nevertheless, let us confirm it with the Shapiro-Wilk test...
shapiro.test(q1_data1)
shapiro.test(q1_data2)
#' For both cases, p > 0.05. Hence, we may conclude that the samples follow normal distribution (amazing revelations here).
#
#' Hyptheses are
# H_0: Population variances are equal
# H_0: Population variances are unequal
#
#' Now for the F-test...
var.test(q1_data1, q1_data2)
#' p > 0.05. 
#' Hence, we accept H_0.
#' Hence, we may conclude that the samples are drawn from populations with equal variances.
#
#' QUESTION 2
#
#' Use any in-built data set, choose any 2 variables and test for equality of variances
#' (or you can import a data set and do this question)
#
#' Data set chosen: handdFootLength.csv
q2_data = read.csv("~/Downloads/handFootLength.csv")
head(q2_data, 5)
#' The samples are
q2_data1 = q2_data$Hand.Length
q2_data2 = q2_data$Foot.Length
#' To test whether the samples are drawn from normal distributions, we use the Shapiro-Wilk test...
shapiro.test(q2_data1)
shapiro.test(q2_data2)
#' In both cases, p > 0.05.
#' Hence, we may conclude that both samples are drawn from normal distributions.
#
#' Hyptheses are
# H_0: Population hand and foot lengths have equal variance
# H_0: Population hand and foot lengths have unequal variance
#
#' Now for the F-test...
var.test(q2_data1, q2_data2)
#' p  = 0.9525 > 0.05.
#' Hence, we may accept H_0.
#' Hence, we may conclude that population hand and foot lengths have equal variances.