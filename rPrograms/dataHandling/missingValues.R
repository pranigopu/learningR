#For a single variable
x = NA
is.na(x)

#For a vector
y = c(1, NA, 2)

#Create a vector with 10 observations and a few missing values
#Check if there are any missing values
#Find the number of missing values
v = c(2, 4, NA, 7, 8, NA)
sum(is.na(v))