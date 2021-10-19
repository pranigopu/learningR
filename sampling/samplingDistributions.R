#1940223, ASSIGNMENT 1
library(ggplot2)
x = iris$Sepal.Width
#------------------------
#Question 1
#Part a
#Sample mean distribution
sample_means = replicate(1000, sample(x, size = 10, replace = FALSE))
sample_means_plot = hist(sample_means, main = "Sample means plot")
#Part b
#Sample range distribution
sample_ranges = replicate(1000, diff(range(sample(x, size = 10, replace = FALSE))))
sample_ranges_plot = hist(sample_ranges, main = "Sample ranges plot")
#------------------------
#Question 2
#Std. error of sample mean is given by
#(population std. deviation)/sqrt(sample size) 

#Population std. deviation
x_mean = mean(x)
x_count = sum(x == x)
ppl_std_dev = sqrt(sum((x - x_mean)^2)/x_count)

#Std. error of sample mean
sample_mean_stderr = ppl_std_dev/sqrt(10)
print(sample_mean_stderr)

#Interpretation
#The sample mean deviates on average by 0.1373728 units from the population mean.
#(The expected sample mean is the population mean, since observations are independently and identically distributed)