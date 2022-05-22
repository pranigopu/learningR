#' CHI SQUARE TESTING
#
#' Chi square test can be applied to check if two attributes are independent or not.
#' The data here is the count of the observations with presence of both, either one or none of the attributes.
#
#
#' The hypotheses for such a test are as follows...
# H_0: The attributes are independent / not associated
# H_1: The attributes are dependent / associated
# (Unlike correlation, we do not check if the association is negative or positive)
#
#' Chi square test can also be applied to check if a sample can be said to be drawn from a population that follows a certain probability distribution.
#' The data here is the observed frequencies, from which we derive expected frequencies (assuming a certain population proportion or probability distribution of the population).
#' 
#
#
#' The hypotheses for such a test are as follows...
# H_0: The sample's population follows the hypothesised proportion or probability distribution
# H_1: Opposite of H_0, lol
# (The conclusion states whether the assumed probability distribution can apply to the sample or not)

#' QUESTION 1
#
#' Here, we test whether family type and anxiety level are associated.
#' The data matrix for the given sample is as follows...
q1_data = matrix(data = c(35, 42, 61, 48, 51, 68), ncol = 3, byrow = TRUE)
q1_data
#' The rows represent family type (joint, nuclear) and columns represent anxiety level (low, normal, high).
#' Performing the Chi square test on the matrix...
chisq.test(q1_data)
#' We get p = 0.7655 > 0.05. Hence, H_0 may be accepted.
#' In other words, we may say that family type and anxiety level are not associated.
#
#
#' QUESTION 2
#
#' Here, we test whether gender is associated with ice-cream preference.
#' The data matrix for the given sample is as follows...
q2_data = matrix(data = c(100, 120, 60, 350, 320, 150), ncol = 3, byrow = TRUE)
q2_data
#' The rows represent the gender (men, women) and the columns represent the ice-cream flavours (chocolate, vanilla, strawberry).
#' Performing the Chi square test on the matrix...
chisq.test(q2_data)
#' We get p = 0.1154 > 0.05. Hence, H_0 may be accepted
#' In other words, we may accept the hypothesis that gender and ice-cream preference are not associated, and accept that they are associated, at least in the sample.
#
#
#' QUESTION 3
#
#' Here, we check, in a sample of 5 children families, whether the probability of male births equals that of female births.
#' This question is to do with fitting of a probability distribution to a sample.
#' Here, we hypothesise that the probability of male births equals the that of female births.
#' In other words, we hypothesise that the probability of success (which could mean either male or female birth) is 0.5.
#
#
#' Hence, our H_0 and H_1 are
#
# H_0: The probability of male birthe quals 0.5
# H_1: The probability of male births is not equal to 0.5
# (Note that we assume that there are only male or female births)
#
#' Observed frequencies for male births out of 5 births in the following order: 5, 4, 3, 2, 1, 0
q3_obf = c(14, 56, 110, 88, 40, 12)
q3_obf
#' For expected frequencies, we use binomial distribution, because each trial (birth) has two possible outcomes (here, we classify male birth as success, female birth as failure, purely in a statistical context), with a fixed probability of success (at least, it is assumed so).
#' Hence, we have the following paramters...
q3_p = 0.5         # Probability of success
q3_N = sum(q3_obf) # Total trials
#' Expected frequencies for male births out of 5 births in the following order: 5, 4, 3, 2, 1, 0
q3_exf = dbinom(c(5, 4, 3, 2, 1, 0), size = 5, prob = q3_p) * q3_N
q3_exf
#' Checking of the sums of observed and expected frequencies are equal...
sum(q3_obf)
sum(q3_exf)
#' Calculated Chi square value is given by
# X^2 = sum((obf - exf)^2 / exf)
q3_X2 = sum((q3_obf - q3_exf)^2 / q3_exf)
q3_X2
#' To check the table X^2 value, we need the degrees of freedom, and significance level...
df = 6 - 1  # Degrees of freedom
a = 0.05    # SIgnificance level
qchisq(1-a, df)
#' We see that calculated X^2 < table X^2. Hence, we may accept H_0.
#' In other words, we may say that the probability of male births is 0.5.
#' Hence, we may say that the probability of female births equals that of male births.
#
#
#' QUESTION 4
#
#' Here, given the following data, we check if me winning one PvP Minecraft battle follows binonmial distribution.
#' Note that the order of number of wins in a row is 5, 4, 3, 2, 1, 0
q4_obf = c(2, 5, 8, 10, 20, 14)
q4_x = c(5, 4, 3, 2, 1, 0)
q4_sum_f = sum(q4_obf)
q4_sum_f                        # Winning streaks
q4_sum_xf = sum(q4_obf * q4_x)
q4_sum_xf                       # Total wins
q4_xbar = q4_sum_xf / q4_sum_f  # Average size of winning streak
# But, in binomial distribution, x_bar = np
# Here, n = max no. of trials = 5, p = probability of win = ?
# p = x_bar / n
q4_p = q4_xbar / 5
q4_p
#' Hence, the expected frequencies are...
q4_exf = dbinom(c(5, 4, 3, 2, 1, 0), size = 5, prob = q4_p) * q4_sum_f
q4_exf
#' Checking if sum of observed frequencies equals the sum of expected frequencies...
sum(q4_obf)
sum(q4_exf)
#' Calculated Chi square value is given by
# X^2 = sum((obf - exf)^2 / exf)
q4_X2 = sum((q4_obf - q4_exf)^2 / q4_exf)
q4_X2
#' To check the table X^2 value, we need the degrees of freedom, and significance level...
df = 6 - 1  # Degrees of freedom
a = 0.05    # SIgnificance level
qchisq(1-a, df)
#' We see that calculated X^2 > table X^2. Hence, we may reject H_0.
#' In other words, we may not say that me winning in PvP Minecraft battles follows binomial distribution.
#' Very sad times.