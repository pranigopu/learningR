#' Determine the required sample size for estimating the population proportion of the variable of your interest by assuming the bound on the error of estimation 0.05 and
#' give the estimates for population proportion and population total with 95% confidence limits by drawing a sample using SRSWOR.
#' Write a report on it.
setwd("~/Documents/Study/computerScience/programming/r/data/")
data = read.csv("weatherAustralia.csv")
head(data)

# Making sure the population has no NA values...
sum(is.na(data$RainTomorrow))
# Hence, the population has 3267 NA values.
# To remove them, we use the following loop...
popl = c()
popl_size = length(data$RainTomorrow)
for (i in c(1:popl_size))
{
  if(!is.na(data$RainTomorrow[i]))
  {
    popl = c(popl, data$RainTomorrow[i])
  }
}
# Checking if NA values are still there...
sum(is.na(popl))
# Setting population size...
N = length(popl)
# Population total and proportion...
A = sum(popl == 2)
print(A)
P = A / N
print(P)
# Population variance...
V = P*(1 - P)
print(V)
# Finding the required sample size, given the bound on error of estimation
B = 0.05
D = B^2 / 4
n = N * V / ((N - 1)*D + V)
n = round(n)
print(n)

# Drawing a sample with the required size...
sample = sample(popl, 278, replace = FALSE)
# Finding sample proportion, hence estimated population proportion...
n = length(sample)
a = sum(sample == 2)
p = a / n
print(p)
# Finding estimated population total...
N_est = p * N
N_est

# Checking if sample follows normal distribution...
shapiro.test(sample)
# p < 0.05, hence, considering the sample to be representative, we may say that the population does not follow normal distribution.

# Using Chebychev's inequality, we have that
# P((X - mu) ≥ k*sigma) ≤ 1/k^2 ... (1)
# where X is the variable, mu is its mean, sigma is its standard deviation, and k is the number of standard deviations from the mean.
# Applying to estimated population proportion and the given bound on error of estimation, we get
# # P((p - P) ≥ B) ≤ 1/k^2 ... (2)
# We do this because we want to find k, that is, the number of standard deviations required so that the error of estimation i.e. p - P is under B i.e. the bound on error of estimation.
# Comparing (1) and (2), we see that
# B = k * sigma, where sigma is the standard deviation of the variable.
# But the variable is estimated population proportion, i.e. p.
# Hence, sigma = sqrt(V(p))
# Hence, B = k * sqrt(V(p))
# Hence, k = B / sqrt(V(p))

# Variance of p is given by
# V(p) = ((N - n) / N) * S^2 / n
# where S^2 is the population mean squared
# S^2 = NP(1 - P) / (N - 1)
S2 = N*P*(1 - P) / (N - 1)
Vp = ((N - n) / N) * S2 / n
print(Vp)

# Finding k
k = B / sqrt(Vp)
print(k)

# Hence, the 95% confidence interval for estimate of P i.e. population proportion is
c(p - k*sqrt(Vp), p + k*sqrt(Vp))
# Now, variance of estimated population total is given by population size times variance of proportion.
# Hence, V(N_est) = N * V(p)
# Similarly the 95% confidence interval for estimate of A i.e. population total is
c(N_est - k*N*sqrt(Vp), N_est + k*N*sqrt(Vp))