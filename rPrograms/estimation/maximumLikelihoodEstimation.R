#Q1
#Poisson distribution is a distribution that models a the number of successes in a sequence of Bernoulli trials in a bounded time period, where the number of trials is unknown. It is generally used when the number of trials tends to infinity and the probability of success tends to zero. Hence, it is used for rare events that have two possible outcomes.
#Poisson distribution has one parameter, which is the mean number of successes in the given time period, and is denoted by lambda.
#------------------------
#Q2
#DATA SET 1
#Generating 100 random data points from a Poisson distribution.
data1 = rpois(100, lambda = 3)
#Saving the data points in a dataframe
data.frame(data1)
#------
#DATA SET 2
#Generating 100 random data points from a Poisson distribution.
data2 = rpois(100, lambda = 10)
#Saving the data points in a dataframe
data.frame(data2)
#------
#DATA SET 3
#Generating 100 random data points from a Poisson distribution.
data3 = rpois(100, lambda = 15)
#Saving the data points in a dataframe
data.frame(data3)
#------------------------
#Q3
#DATA SET 1
hist(data1, breaks = 10)
#------
#DATA SET 2
hist(data2, breaks = 10)
#------
#DATA SET 3
hist(data3, breaks = 10)
#------
#CONCLUSIONS ON THE ABOVE GRAPHS
#The above graphs show the distributions for the three samples, each taken from a separate Poisson distribution.
#Since the samples are drawn randomly, the distributions of the samples can be considered as good approximations of the actual distribution curve.
#The distribution curves are dissimilar in shape and position.
#------------------------
#ESTIMATING LAMBDA VALUES FOR THE SAMPLES
#------
#The maximum likelihood estimation (MLE) method is used to estimate the parameter(s) of the distributions that can produce a given sample.
#The was it is done is by finding out which value(s) of the parameter(s) are most likely to form a distribution that would produce the given sample.
#
#Hence, for a sample x1, x2... x100, we do
#P(X1 = x1, X2 = x2... X100 = x100) = P(x1) * P(x2)... P(x100)
#In the case of Poisson distribution, we have
#P(X1 = x1, X2 = x2... X100 = x100) = 
#e^(-lambda)lambda^x1/x1! * 
#e^(-lambda)lambda^x2/x2!... 
#e^(-lambda)lambda^x100/x100!
#Hence, we need to find the value for lambda for which P(X1 = x1, X2 = x2... X100 = x100) is maximised.
#------
#Function for performing MLE for a given sample "data"
mle = function(data)
{
  i = 1
  max = 0
  lambda = 0
  while(i < 21)
  {
    j = 1
    likelihood = 1
    while(j < 101)
    {
      likelihood = likelihood * dpois(data[j], lambda = i)
      j = j + 1
    }
    if(max < likelihood)
    {
      max = likelihood
      lambda = i
    }
    i = i + 1
  }
  return(lambda)
}
#DATA SET 1
lambda1 = mle(data1)
#------
#DATA SET 2
lambda2 = mle(data2)
#------
#DATA SET 3
lambda3 = mle(data3)
#------------------------
#CONCLUSIONS ON THE ESTIMATING LAMBDA VALUES FOR THE SAMPLES
#------
#Using the MLE method with brute calculation done through the loops, we get the estimated lambda values for each sample as follows...
lambda1
lambda2
lambda3
#The above values are either equal to or very close to the actual values of lambda given in the data point generation functions.