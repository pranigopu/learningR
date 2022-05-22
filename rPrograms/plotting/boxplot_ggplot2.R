library(ggplot2)
mtc = mtcars
attach(mtc)

#Trialboxplot
data1 = c(3, 7, 8, 5, 12, 14, 21, 13, 18)
boxplot(data1)

#Using ggplot2
ggplot(mtc, aes(y = wt)) + geom_boxplot()

#Comparitive
ggplot(mtc, aes(x = as.factor(am), y = wt, fill = as.factor(am))) + geom_boxplot()
#Verification
summary(mtc[am, c(6, 9)])
summary(mtc[am == 1, c(6, 9)])

#More comparitive
ggplot(mtc, aes(x = as.factor(am), y = mpg, fill = as.factor(cyl))) + geom_boxplot()