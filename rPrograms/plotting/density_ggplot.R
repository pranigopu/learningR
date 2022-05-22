mtc = mtcars
attach(mtc)
library(ggplot2)

ggplot(data = mtc, mapping = aes(x = mpg))
#No graph as geometric object is not specified

ggplot(data = mtc, mapping = aes(x = mpg)) + geom_density()
#Single density graph

sort(mpg, decreasing = FALSE)

ggplot(data = mtc, mapping = aes(x = mpg, fill = as.factor(cyl))) + geom_density(alpha = 1)
#Density plots for each category of cyl

f1 = ggplot(data = mtc, mapping = aes(x = mpg, fill = as.factor(cyl))) + geom_density(alpha = 1)

#Draw the density plt on hp variable in relation to am
f2 = ggplot(mtc, aes(hp, fill = as.factor(am))) + geom_density(alpha = 1)

#Fill manually specified colours
f1 + scale_fill_manual(values = c("#FF0000", "#00FF00", "#0000FF"))
