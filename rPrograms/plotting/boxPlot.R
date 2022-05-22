attach(mtcars)
mtcars
summary(mtcars)
boxplot(mpg~cyl, main = "Car Mileage Data", xlab = "Number of cylinders", ylab = "Miles per gallon")
boxplot(mpg~cyl, main = "Car Mileage Data", xlab = "Number of cylinders", ylab = "Miles per gallon", notch = TRUE)
boxplot(mpg~cyl, main = "Car Mileage Data", xlab = "Number of cylinders", ylab = "Miles per gallon", col = c("green", "blue", "white"))
boxplot(mpg~cyl, main = "Car Mileage Data", xlab = "Number of cylinders", ylab = "Miles per gallon", horiz = TRUE)
