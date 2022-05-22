#' OBJECTIVE
#
#' The yield in quintals for paddy crop of 50 villages in a certain area are given in the excel sheet.
#' Select a simple random sample of size 10 units using without replacement procedure and estimate the average yield per plot
#' along with the five number summary statistics of the population.
#' Interpret your results.
#
#' DATA DESCRIPTION
#
#' The data
setwd("~/Documents/Study/computerScience/programming/r/data/")
myData = read.csv("cropYield.csv")
head(myData, 10)
#' Dimensions of the data set
dim(myData)
# 50 rows, 1 column
#' Certain data descriptors
summary(myData)
#' The data contains 50 entries, with one field that is the yield of paddy crops measured in quintals.
#' The lowest yield measured is 10 quintals, and the highest measured is 75 quintals.
#' The mean yield in this data set is 31.82 quintals.
#
#' ANALYSIS
#
#' Question summary
# Population size: 50
# Required sample size: 10
# Estimates required: All the parameters in listed in the smmary function i.e.
#   - Minimum
#   - Maximum
#   - 1st quartile
#   - Median
#   - 3rd quartile
#   - Mean
# NOTE: Sampling must be done without replacement
sampleSize = 10
set.seed(132)
# Setting the seed fixes the random sample generation, so the sample is random but predictable
population = myData$yield
sample = sample(population, 10, replace = FALSE)
sample
#' The estimates, compared to the parameters, are as follows
#
#' Minimum and maximum
# Sample
min(sample)
max(sample)
# Population
min(population)
max(population)
#
#' 1st quartile, median and 3rd quartile
# Sample
sample = sort(sample, decreasing = FALSE)
sample[length(sample) * 1 / 4]
median(sample)
sample[length(sample) * 3 / 4]
#Population
population = sort(population, decreasing = FALSE)
population[length(population) * 1 / 4]
median(population)
population[length(population) * 3 / 4]
#
#' Mean
# Sample
mean(sample)
# Population
mean(population)
#
#' INTERPRETATION
#
#' The variable being studied is the yeild of paddy crops measured in quintals, in a certain area.
#' Estimations of the required parameters are as follows:
# Minimum and maximum...
#' 16 quintals, 44 quintals
# 1st quartile,  median and 3rd quartile...
#' 24 quintals, 33.5 quintals, 36 quintals
# Mean
#' 32.4 quintals