#install.packages("ggplot2")
#gg => grammar of graphics
library("ggplot2")
help.search("geom_", package = "ggplot2")
mtc = mtcars
attach(mtc)
cyl = as.factor(cyl)
am = as.factor(am)
vs = as.factor(vs)
gear = as.factor(gear)
carb = as.factor(carb)
summary(mtc)
str(mtc)

#NOTE: ggplot2 allows you to add features and parameters after the plot function

g1 = ggplot(mtc, aes(mpg, disp))
g1
#... aes defines axes

g2 = g1 + geom_point()
g2
#... geom_ is used to specify geometric object
#geom_point() specifies that the data values must be represented as points
#We thus get a scatter plot

g3 = g2 + geom_point(aes(color = factor(cyl)))
g3
g3 = g2 + geom_point(aes(color = factor(am)))
g3
#... Here, we view the graph with respect to a factor variable, denoting that factor using colors

g3 = g2 + geom_text(aes(label = cyl))
g3
#... Here, we view the graph with respect to another variable,denoting that variable using its values as labels

g3 = g2 + geom_text(aes(label = am))
g3 = g3 + geom_point(aes(color = factor(cyl)))
g3
#... Here, we view the graph with respect to 2 variables

g3 = g2 + geom_point(aes(size = factor(gear)))
g3


g4 = g2 + geom_smooth()
g4
#... Here, we view a smooth curve passing through the data points