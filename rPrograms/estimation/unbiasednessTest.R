#Data set description
#The iris data set is a table of data about different observations of a flower called iris.
#The fields are
#1. Sepal length
#2. Sepal width
#3. Petal length
#4. Petal width
#5. Species
data = iris
#Assignment aim
#To check whether sample mean is an unbiased estimator of population mean, with different random samples of variables such as 50,100,200 and 400

#Population measures
#Mean
c(
  mean(data$Sepal.Length),
  mean(data$Sepal.Width),
  mean(data$Petal.Length),
  mean(data$Petal.Width)
)
#SD
c(
  sd(data$Sepal.Length),
  sd(data$Sepal.Width),
  sd(data$Petal.Length),
  sd(data$Petal.Width)
)

#Samples
#Sample 1
s1_1 = sample(data$Sepal.Length, 10)
s1_2 = sample(data$Sepal.Width, 10)
s1_3 = sample(data$Petal.Length, 10)
s1_4 = sample(data$Petal.Width, 10)
#Mean
c(mean(s1_1), mean(s1_2), mean(s1_3), mean(s1_4))
#SD
c(sd(s1_1), sd(s1_2), sd(s1_3), sd(s1_4))

#Sample 2
s2_1 = sample(data$Sepal.Length, 20)
s2_2 = sample(data$Sepal.Width, 20)
s2_3 = sample(data$Petal.Length, 20)
s2_4 = sample(data$Petal.Width, 20)
#Mean
c(mean(s2_1), mean(s2_2), mean(s2_3), mean(s2_4))
#SD
c(sd(s2_1), sd(s2_2), sd(s2_3), sd(s2_4))

#Sample 3
s3_1 = sample(data$Sepal.Length, 40)
s3_2 = sample(data$Sepal.Width, 40)
s3_3 = sample(data$Petal.Length, 40)
s3_4 = sample(data$Petal.Width, 40)
#Mean
c(mean(s3_1), mean(s3_2), mean(s3_3), mean(s3_4))
#SD
c(sd(s3_1), sd(s3_2), sd(s3_3), sd(s3_4))

#Sample 4
s4_1 = sample(data$Sepal.Length, 80)
s4_2 = sample(data$Sepal.Width, 80)
s4_3 = sample(data$Petal.Length, 80)
s4_4 = sample(data$Petal.Width, 80)
#Mean
c(mean(s4_1), mean(s4_2), mean(s4_3), mean(s4_4))
#SD
c(sd(s4_1), sd(s4_2), sd(s4_3), sd(s4_4))

#------------
m = c(mean(data$Sepal.Length), mean(data$Sepal.Width), mean(data$Petal.Length), mean(data$Petal.Width))
m1 = c(mean(s1_1), mean(s1_2), mean(s1_3), mean(s1_4))
m2 = c(mean(s2_1), mean(s2_2), mean(s2_3), mean(s2_4))
m3 = c(mean(s3_1), mean(s3_2), mean(s3_3), mean(s3_4))
m4 = c(mean(s4_1), mean(s4_2), mean(s4_3), mean(s4_4))
#------------
sd = c(sd(data$Sepal.Length), sd(data$Sepal.Width), sd(data$Petal.Length), sd(data$Petal.Width))
sd1 = c(sd(s1_1), sd(s1_2), sd(s1_3), sd(s1_4))
sd2 = c(sd(s2_1), sd(s2_2), sd(s2_3), sd(s2_4))
sd3 = c(sd(s3_1), sd(s3_2), sd(s3_3), sd(s3_4))
sd4 = c(sd(s4_1), sd(s4_2), sd(s4_3), sd(s4_4))
#------------
matrix(c(m, m1, m2, m3, m4), ncol = 4, byrow = TRUE)
matrix(c(sd, sd1, sd2, sd3, sd4), ncol = 4, byrow = TRUE)
#INTERPRETATION
#Means
#The sample means have some tendency to get closer to the population means and standard deviations as the sample size rises.
#However, this tendency is not strong, since the rise in sample size does not always lead to a rise in accuracy of the estimator.

#The standard deviation also behaves similarly to the sample means accross samples.
#The tendency for accuracy is not consistent.

#In conclusion, the bias of the estimators does not seem to change overall with the increase in sample size.
#This is because the population size itself is quite small, at 150.