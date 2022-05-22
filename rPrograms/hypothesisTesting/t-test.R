setwd("~/Downloads")
cs2m = read.csv("cs2m.csv")
grades = read.csv("grades.csv")
View(cs2m)
View(grades)
#' ------------------------
#' DESCRIPTION
#' t-test is a method of testing the equality of the population means of two random samples.
#' It is relevant in cases where the population is unknown and needs to be inferred from the sample.
#' It is relevant in cases where the populations need not be known, yet their equality needs to be checked.
#' ------------------------
#' ASSUMPTIONS
#' 1. Samples are independent
#' 2. Populations have the same variance
#' 3. Populations follow normal distribution
#' ___
#' Testing assumptions
#' 1. Use your sense
#' 2. F-test
#' 3. Shapiro-Wilk test
#' ------------------------
#' HYPOTHESES
#' Hypotheses that are tested in t-test are
#' 1. H_0: Difference between the two population means is 0
#' 2. H_1: Difference between the two population means is not 0
#' ========================
#' QUESTION 1
#' Test whether there is significant difference between mean scores of quiz1 and quiz3
quiz1 = grades$quiz1
quiz3 = grades$quiz3
#' Testing for assumptions...
#' 1. Are they independent?
#' There is no reason to state that quiz1 score affects quiz3 score
#' Hence, without further information, we may conclude they are independent
#' ___
#' 2. Are their variances equal (using F-test)?
#' H_0 for F-test: Difference of variances is 0
#' H_1 for F-test: Difference of variances is not 0
var.test(quiz1, quiz3, conf.level = 0.95)
#' The p-value of the F-test is 0.4623 > 0.05.
#' Hence, we accept the null hypothesis.
#' Hence, we conclude that the variances of quiz1 and quiz3 are equal.
#' ___
#' 3. Do their populations follow normal distribution?
#' H_0 for Shapiro-Wilk test: Population follows normal distribution
#' H_1 for Shapiro-Wilk tes: Population does not follow normal distribution
shapiro.test(quiz1)
#' p = 1.178e-07 < 0.05
shapiro.test(quiz3)
#' p = 1.414e-09 < 0.05
#' In both cases, p < 0.05.
#' Hence, we reject the null hypothesis that their populations do not follow normal distribution
#' ------------
#' The t-test
t.test(quiz1, quiz3, var.equal = TRUE)
#' The p-value = 0.1214 > 0.05
#' Hence we accept the null hypothesis.
#' Hence, we can conclude that the mean test scores between quiz1 and quiz2 are not significantly different.
#' Now, both sample populations may not follow normal distribution, but since the t-test is robust, the results of the t-test may be worth considering.
#' (In fact, their means are within 0.6 units of each other)
#' ========================
#' QUESTION 2
#' Test whether there is significance difference between Blood Pressure of women across Drug Reaction
BP = cs2m$BP
DrugR = cs2m$DrugR
#' Testing for assumptions...
#' 1. Are they independent?
#' Blood pressure and drug reaction may or may not be independent, since we have insufficient information about the nature of the drug
#' Hence, we assume them as independent.
#' ___
#' 2. Are their variances equal (using F-test)?
#' H_0 for F-test: Difference of variances is 0
#' H_1 for F-test: Difference of variances is not 0
var.test(BP, DrugR, conf.level = 0.95)
#' The p-value of the F-test < 2.2e-16 < 0.05.
#' Hence, we reject the null hypothesis.
#' Hence, we conclude that the variances of BP and DrugR are unequal.
#' ___
#' 3. Do their populations follow normal distribution?
#' H_0 for Shapiro-Wilk test: Population follows normal distribution
#' H_1 for Shapiro-Wilk tes: Population does not follow normal distribution
shapiro.test(BP)
#' p = 0.1337 > 0.05
shapiro.test(DrugR)
#' p = 2.211e-07 < 0.05
#' Hence, we accept the null hypothesis for BP, but reject it for DrugR
#' Hence, only BP's population can be concluded to follow normal distribution.
#' ------------
#' The t-test
t.test(BP, DrugR, var.equal = FALSE)
#' The p-value < 2.2e-16 < 0.05
#' Hence we reject the null hypothesis.
#' Hence, we can say that there is significant difference between blood pressure and drug reaction.
#' From this, we may conclude that these two measures are unrelated, or weakly related.
#' Now, one sample population may not follow normal distribution, but since the t-test is robust, the results of the t-test may be worth considering.
#'========================
#' QUESTION 3
#' Test whether Drug Reaction and Anxiety Levels were significantly associated
#' (Significantly associated => their populations are similar)
DrugR = cs2m$DrugR
AnxtyLH = cs2m$AnxtyLH
#' Testing for assumptions...
#' 1. Are they independent?
#' Anxiety level and drug reaction may or may not be independent, since we have insufficient information about the nature of the drug
#' Hence, we assume them as independent.
#' ___
#' 2. Are their variances equal (using F-test)?
#' H_0 for F-test: Difference of variances is 0
#' H_1 for F-test: Difference of variances is not 0
var.test(DrugR, AnxtyLH, conf.level = 0.95)
#' The p-value of the F-test is 0.9905 > 0.05.
#' Hence, we accept the null hypothesis.
#' Hence, we conclude that the variances of DrugR are AnxtyLH equal.
#' (In fact, the ratio of their variances is very close to 1)
#' ___
#' 3. Do their populations follow normal distribution?
#' H_0 for Shapiro-Wilk test: Population follows normal distribution
#' H_1 for Shapiro-Wilk tes: Population does not follow normal distribution
shapiro.test(DrugR)
#' p = 2.211e-07 < 0.05
shapiro.test(AnxtyLH)
#' p = 2.107e-07 < 0.05
#' For both cases, p < 0.05, hence we reject the null hypothesis for both samples.
#' Hence, none of the populations follow normal distribution.
#' ------------
#' The t-test
t.test(DrugR, AnxtyLH, var.equal = FALSE)
#' The p-value = 0.8003 > 0.05
#' Hence we accept the null hypothesis.
#' Hence, we can say that there is no significant difference between drug reaction and anxiety level.
#' From this, we may conclude that these two measures are significantly associated.
#' Now, both sample populations may not follow normal distribution, but since the t-test is robust, the results of the t-test may be worth considering.