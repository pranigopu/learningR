library(ggplot2)
?scale_color_brewer
mtc = mtcars
attach(mtc)  
f1 = ggplot(mtc, aes(x = as.factor(am), y = wt, fill = as.factor(am))) + geom_boxplot()
f1 + scale_fill_brewer(palette = "Dark2")
f1 + scale_fill_brewer(palette = "Set1")

f2 = ggplot(mtc, aes(x = mpg, fill = as.factor(cyl))) + geom_density()
f2 + scale_fill_brewer(palette = "PuBu")

#Changing colour sequence order
#Method 1
f3 = ggplot(mtc, aes(x = as.factor(cyl), fill = as.factor(cyl))) + geom_bar() 
f3 + scale_fill_brewer(palette = "Greys", direction = -1)
#Method 2
f3 + scale_fill_grey(start = 0.25, end = 0.75)
