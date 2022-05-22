library(ggplot2)
mtc = mtcars
attach(mtc) 
#Coordindate flipping
f1 = ggplot(mtc, aes(x = as.factor(am), y = wt, fill = as.factor(am)))
f1 + coord_flip() + geom_boxplot()

#Notch (boxplot)
f1 + geom_boxplot(notch = TRUE)

#Changing outlier plot character, size and colour (boxplot)
f1 + geom_boxplot(outlier.colour = "red", outlier.shape = 15, outlier.size = 0.7)

#Trim (violin)
f2 = ggplot(data = mtc, aes(x = as.factor(cyl), y = wt))
f2 + geom_violin(trim = FALSE)