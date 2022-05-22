View(mtcars)
mtcars[7, 6]
mtcars[1:5, ]
#Seeing first few observations
head(mtcars, 10)
#Seeing last few observations
tail(mtcars, 10)
sum(mtcars$mpg)
#Getting the max of a column
max(mtcars$mpg)
#Getting the row having the maximum mpg
mtcars[mtcars$mpg == max(mtcars$mpg), ]
