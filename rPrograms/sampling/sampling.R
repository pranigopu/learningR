library(dplyr)
View(mtcars)
mydata = mtcars
#Sampling random n observations
sample_n(mydata, 7)

#Sampling random n * 100 precent of observations
sample_frac(mydata, 0.45)