#SAMPLING
x = mtcars$mpg
sample = sample(x, size = 4, replace = FALSE)
sample

#REPLICATE SAMPLING PROCESS
#Replicate sampling
samples = replicate(1000, sample(x, size = 4, replace = FALSE))
#Replicate the process of taking the mean of sample
sample_means = replicate(1000, mean(sample(x, size = 4, replace = FALSE)))
#Replicate the process of taking the ranges of sample
sample_ranges = replicate(1000, diff(range(sample(x, size = 4, replace = FALSE))))
#Plotting
hist(sample_means)
hist(sample_ranges)
