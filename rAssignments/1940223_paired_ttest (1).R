#' QUESTION 1
Vernier = c(2.265, 2.267, 2.264, 2.267, 2.268, 2.263, 2.264, 2.258)
Micrometer = c(2.270, 2.268, 2.269, 2.273, 2.270, 2.270, 2.268, 2.268)
#' Here, we have two measurements on the same entities (same cylinders), but with differing conditions (measurement tool).
#' Hence, these samples are paired samples.
#------------
#' AIM
#
#' We need to see if the change in the measurement tool causes significant change in the sample.
#' In other words, we want to use these samples to generalise (for the whole population of cylidrical rods) whether the measurements of vernier calliper and micrometer on the same entities are significantly different.
#-----------
#' CHECKING ASSUMPTIONS
#
#' Assumption 1...
shapiro.test(Vernier)
#' We have that p = 0.2393 > 0.05, hence hence the sample may be said to be taken from a normal distribution.
#
shapiro.test(Micrometer)
#' We have that p = 0.054 > 0.05, hence the sample may be said to be taken from a normal distribution.
#
#' Assumption 2...
#
#' Since the measurement of one cylindrical rod does not affect the measurement of another, we can say that each sample's each observation is independently taken.
#
#' Assumption 3...
#
#' Since the samples are taken from the same entities, and since both samples only differ in measurement tools and not in some quality affecting the entities, we may say that the variances of the populations of the samples are equal.
#' Verifying with f-test...
var.test(Vernier, Micrometer)
#' We see that p = 0.1204 > 0.05, hence we can say that the variances of the sample populations are similar enough to be considered equal.
#
#' Assumption 4...
#
boxplot(Vernier, Micrometer, names = c("Vernier", "Micrometer"), outline = TRUE)
#' There is only one outlier in Vernier sample...
#' We may proceed.
#
#------------
#'PAIRED T-TEST
t.test(Vernier, Micrometer, , paired = TRUE)
#' We see that p = 0.001565 < 0.05, hence we reject the null hypothesis that the sample populations are the same.
#' Hence we conclude that the sample populations are different.
#' Hence, we can say that the change in measurement tool from Vernier calliper to micrometer causes significant difference in measurement of cylidrical rods, in general.