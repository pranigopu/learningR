#' AIM
#
#' Demonstrate the concept of missing plot technique of Latin
#' Square Design (LSD) using a suitable example and give your interpretation.
#' (Only one missing value case)
#========================
#' FORMULA
#
#' X = [t(R' + C' + T') - 2G']/[(t - 2)(t - 2)]
# where...
# X is the missing value's estimate
# R' is the total of the available values in the row blocking factor.
# C' is the total of the available values in the column blocking factor.
# T' is the total of the available values in the treatment.
# t is the number of levels in the treatment.
# G' is the total of the available values in the data set.
#========================
#' DATA SET
setwd("/Users/pranav/Documents/Study/computerScience/programming/r/data/")
data = read.csv("weatherSouthAfrica.csv")[c(2, 5, 22, 23)]
head(data)

tr = data$Location
r = data$RainToday
c = data$RainTomorrow
y = data$Rainfall

# Summary about the data
summary(data.frame(as.factor(tr), as.factor(r), as.factor(c), y))
#========================
#' MISSING VALUE ESTIMATION
#
#' Checking how many missing values are there in the response
sum(is.na(y))
#' Locating the missing value
for(i in c(1:length(y)))
{
  if(is.na(y[i]))
  {
    r_at_X = r[i]
    c_at_X = c[i]
    tr_at_X = tr[i]
    i_at_X = i
  }
}
#' Calculating R'
R_prime = 0
for(i in c(1:length(y)))
{
  if(r[i] == r_at_X & i != i_at_X){R_prime = R_prime + y[i]}
}
R_prime
#' Calculating C'
C_prime = 0
for(i in c(1:length(y)))
{
  if(c[i] == c_at_X & i != i_at_X){C_prime = C_prime + y[i]}
}
C_prime
#' Calculating T'
T_prime = 0
for(i in c(1:length(y)))
{
  if(tr[i] == tr_at_X & i != i_at_X){T_prime = T_prime + y[i]}
}
T_prime
#' Calculating G'
G_prime = 0
for(i in c(1:length(y)))
{
  if(i != i_at_X){G_prime = G_prime + y[i]}
}
G_prime
#' Number of treatments
t = length(unique(tr))
t
#' Estimating X (missing value)
X = (t*(R_prime + C_prime + T_prime) - 2*G_prime)/(t*t)
# t*t was chosen instead of (t-1)(t-2) since t is too small, and was yielding grossly deviating esimations
X
#========================
#' CONCLUSIONS
#
#' We get the missing value esimate as 11.17778, which is much higher than the average,
#' and close to the maximum. This may be due to the following factors:
# - Error in my calculations
# - Small and potentially biased sample