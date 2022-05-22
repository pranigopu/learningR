#lm => linear model
#NOTE: y ~ x means y is dependent, x is independent
lm(mpg~wt, data = mtcars)
#From the above, we see that model is estimated as
#mpg = 37.285 - 5.344*wt

#Plotting the regression curve
library(ggplot2)
#Points
fit = ggplot(mtcars, aes(x = wt, y = mpg))
fit + geom_point()
#Best fitting line
fit + geom_point() + geom_smooth(method = "lm", se = TRUE)
fit + geom_point() + geom_smooth(method = "lm", se = FALSE)
