library("ggplot2")
library("dplyr")
mtc = mtcars
attach(mtc)

ggplot(data = mtc, mapping = aes(x = cyl)) + geom_bar()

#Verifying frequencies
#Method 1
sum(cyl == 4)
sum(cyl == 6)
sum(cyl == 8)
#Method 2
mtc$cyl = as.factor(mtc$cyl)
summary(mtc)
#Method 3
nrow(filter(mtc, cyl == 4))
nrow(filter(mtc, cyl == 6))
nrow(filter(mtc, cyl == 8))

f1 = ggplot(data = mtc, mapping = aes(x = cyl, fill = factor(cyl))) + geom_bar()
f1
f1 + scale_fill_manual(values = c("#0000FF", "#00FF00", "#FF0000"))

#Segmented bar graph
f2 = ggplot(data = mtc, mapping = aes(x = cyl, fill = factor(am))) + geom_bar()
f2

sum(cyl == 4 & am == 1)

#Proportional semgmented bar graph
f3 = ggplot(data = mtc, mapping = aes(x = cyl, fill = factor(am)))
f3 + geom_bar(position = "fill")

#Multiple bar graph
f4 = ggplot(data = mtc, mapping = aes(x = cyl, fill = factor(am)))
f4 + geom_bar(position = "dodge")

#Horizontal bar graph
f5 = ggplot(data = mtc, mapping = aes(x = cyl, fill = factor(am)))
f5 + geom_bar() + coord_flip()

#Manual colour entry again
f6 = ggplot(data = mtc, mapping = aes(x = cyl, fill = factor(am))) + geom_bar()
f6 + scale_fill_manual(values = c("aquamarine", "magenta"))

#Bar plot with 2 variables
f7 = ggplot(data = mtc, mapping = aes(x = cyl, y = wt, fill = factor(am)))
f7
f7 + geom_bar(position = "dodge", stat = "identity")
