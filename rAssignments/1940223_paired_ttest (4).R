#' QUESTION 4
before = c(180, 125, 240, 150, 173.75, 49.56)
after = c(170, 130, 215, 152, 166.75, 36.09)
#' Here, we have two samples of weights taken from the same people.
#' One sample is the weights before the water diet, the other is the weights after 6 weeks of the water diet.
#' Since the samples are taken one the same entities, with the only differing factor being the water diet.
#' Hence, they are paired samples, and we perform a paired t-test.
#
#------------
#' AIM
#
#' We need to find out if the weights of the whole population of people (not just this sample) taking the water diet for 6 weeks actually differs from their weights before the water diet. 
#' In other words, we need to find out what effect the water diet has on the weight if people in general.
#-----------
#' CHECKING ASSUMPTIONS
#
#' Assumption 1...
shapiro.test(before)
#' We have that p = 0.8859 > 0.05, hence hence the sample may be said to be taken from a normal distribution.
#
shapiro.test(after)
#' We have that p = 0.1204 > 0.05, hence the sample may be said to be taken from a normal distribution.
#
#' Assumption 2...
#
#' Since the measurement of one persons's weight is independeny from the measurement of another person's weight, each samples is independently drawn.
#
#' Assumption 3...
#
#' Since the samples are taken from the same entities, and since both samples only differ due to the water diet and not in some quality affecting the entities, we may say that the variances of the populations of the samples are equal.
#' Verifying with f-test...
var.test(before, after)
#' We see that p = 0.9077 > 0.05, hence we can say that the variances of the sample populations are similar enough to be considered equal.
#
#' Assumption 4...
#
boxplot(before, after, names = c("Before", "After"), outline = TRUE)
#' There is only one outlier in "after" sample...
#' We may proceed.
#
#------------
#' PAIRED T-TEST
#
t.test(before, after, paired = TRUE)
#' Here we see that p = 0.1289 > 0.05, hence we can say that the samples are taken from equal or very similar population.
#' Hence, we can say that the water diet has little to no effect on the weights of people in general.