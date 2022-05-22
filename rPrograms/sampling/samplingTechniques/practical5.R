library(dplyr)
popl = iris
head(popl)
#'Strata are based on species
stra1 = filter(popl, popl$Species == "virginica")
stra2 = filter(popl, popl$Species == "setosa")
stra3 = filter(popl, popl$Species == "versicolor")
#' Variable of interest is sepal length.
#' The sizes of each stratum is as follows...
N1 = length(stra1$Sepal.Length)
N2 = length(stra2$Sepal.Length)
N3 = length(stra3$Sepal.Length)
c(N1, N2, N3)
#' Let the sample size be 20.
#' Sampling from each stratum...
set.seed(1)
sample1 = sample(stra1$Sepal.Length, 20, replace = FALSE)

set.seed(2)
sample2 = sample(stra2$Sepal.Length, 20, replace = FALSE)

set.seed(3)
sample3 = sample(stra3$Sepal.Length, 20, replace = FALSE)
#' Sample mean is an unbiased estimator of population mean.
#' So, we will take the means of each sample, to get the estimated mean for each stratum.
sample1_mean = mean(sample1)
sample2_mean = mean(sample2)
sample3_mean = mean(sample3)
c(sample1_mean, sample2_mean, sample3_mean)

#' To estimate the whole population's mean sepal length, we calculate the combined sample's mean sepal length...
(sample1_mean + sample2_mean + sample3_mean)/3

#' Population size times sample mean is an unbiased estimator of population total.
#' To estimate each stratum's total sepal length...
stra1_total = length(stra1$Sepal.Length) * sample1_mean
stra2_total = length(stra2$Sepal.Length) * sample2_mean
stra3_total = length(stra3$Sepal.Length) * sample3_mean
c(stra1_total, stra2_total, stra3_total)

#' To estimate the whole population's total sepal...
stra1_total + stra2_total + stra3_total

#' In conclusion, we estimate that the mean sepal length of the whole population to be
#' 5.513333 cm, and the population's total sepal length to be 827 cm.