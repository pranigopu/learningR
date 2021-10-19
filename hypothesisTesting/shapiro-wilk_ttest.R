#PART 1: Checking if a data set seems to follow normal distribution
#------------
#The Shapiro-Wilk test is used to check how closely a data set follows normal distribution.
#------------
#H_0: data follows normal distribution
#H_1: data does not follow normal distribution
#If p > 0.05, H_0 is accepted.
#If p < 0.05, H_0 is rejected.
#------------
#DATA SET 1
data1 = rbinom(100, 100, 0.4)
shapiro.test(data1)
#p > 0.05
#Hence, data1 can be said to follow normal distribution.
#Hence, H_0 is accepted.
#------------
#DATA SET 2
data2 = c(100, 99, 67, 1, 2, -2)
shapiro.test(data2)
#p = 0.04435 < 0.05
#Hence, data2 cannot be said to follow normal distribution.
#Hence, H_0 is rejected.
#========================
#PART 2: Estimating the mean of a population from which the data set is assumed to be derived
#------------
#The t-test is used to determine whether the means of two groups are equal to each other.
#It is also used to judge if a sample is derived from a normal distribution, which is the purpose for which it is used here.
#Hence, it can also be used to judge if a sample is representative of a normal population or not.
#The assumption for the test is the data is sampled from normal distributions with equal variances. 
#------------
#H_0: sample mean - population mean is 0
#H_1: sample mean - population mean is not 0
#If p > 0.05, H_0 is accepted.
#If p < 0.05, H_0 is rejected.
#------------
#DATA SET 3
data3 = rpois(100, 4)
val = mean(data3)
t.test(data3, mu = val)
#p = 1 > 0.05
#Hence, data3's mean is within the 95% confidence interval for the population mean.
#Hence, data3 may be a  representative sample.
#Hence, H_0 may accepted
#------------
#DATA SET 4
data4 = c(12, 23, 4, -1, -32, 122, -232, 32)
t.test(data4, mu = mean(data4))
#p = 1 > 0.05
#Hence, data4's mean is within the 95% confidence interval for the population mean.
#Hence, data3 may be considered as a representative sample.
#Hence, H_0 may be accepted.