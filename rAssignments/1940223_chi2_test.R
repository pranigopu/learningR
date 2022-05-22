#' QUESTION 1
#
#' A market researcher firm wants to determine on the basis of the following information whether there exists a relationship between
#' the size of the tube of toothpaste, which a customer buys, and the number of persons in the customer’s household.
#' At significance level α = 0.01, inspect if there is a relationship.
q1_data = matrix(data = c(22, 107, 76, 45, 55, 23, 16, 12, 31, 69, 37, 0), ncol = 4, byrow = TRUE)
q1_data
#' Here, the columns represent the family size intervals (1-2, 3-4, 5-6, >7) and the rows represent the tube size (Giant, Large, Small).
# H_0: Tube size and family size are independent / not associated
# H_1: Tube size and family size are dependent / associated
chisq.test(q1_data)
#' p < 2.2e-16 < 0.05.
#' Hence, we may reject H_0.
#' Hence, we may say that there is a relationship between the toothpaste tube size bought by cutomers and the number of people in their family.
#
#' QUESTION 2
#
#' The contingency table below summarizes the results obtained in a study conducted by a research organization,
#' with respect to the performance of four competing brands of toothpaste among the users:
q2_data = matrix(data = c(9, 13, 17, 11, 63, 70, 85, 82, 28, 37, 48, 37), ncol = 4, byrow = TRUE)
q2_data
#' Here, the columns represent the toothpaste brand (Brands A, B, C and D), and the rows represent the incidence of cavities of the users of  different toothpaste (0, 1-5, >5).
# H_0: Brand and incidence of cavities are independent / not associated
# H_1: Brand and incidence of cavities are dependent / associated
chisq.test(q2_data)
#' p = 0.9278 > 0.05.
#' Hence, we may accept H_0.
#' Hence, we may conclude that the brand of the toothpaste and the incidence of cavities are not associated.
#
#' QUESTION 3
#
#' A person was checking the colors in a pack of “Gems” and found that the distribution was as follows.
#' Does it match with the company’s claims that all colors are equally likely?
#' 17.8% blue, 18.3% orange. 18.5% green, 16.9% yellow, 17.3% red (and 11.2% some other color, not given in the question).
#
# H_0: Each color is equally likely
# H_1: Each color is not equally likely
#' Observed frequencies (out of 100):
q3_obf = c(17.8, 18.3, 18.5, 16.9, 17.3, 11.2)
q3_obf
#' Expected frequencies (out of 100):
q3_exf = c(rep(100/6, 6))
q3_exf
#' The sum of both sets of frequencies is 100 each...
sum(q3_obf)
sum(q3_exf)
#' Calculating X^2:
# X^2 = sum((obf - exf)^2 / exf)
q3_X2 = sum((q3_obf - q3_exf)^2 / q3_exf)
q3_X2
#' Now,we have the following parameters:
q3_df = 6 - 1 # Degrees of freedom
q3_los = 0.05# Level of significance
#' X^2 table value:
qchisq(1 - q3_los, q3_df)
#' Hence we see that calculated X^2 < table X^2.
#' Hence, we may accept H_0.
#' Hence, we may confirm the claims of the company, that each color of 'Gems' is equally likely.