library(ggplot2)
mtc = mtcars
attach(mtc)

f0 = ggplot(data = mtc, aes(x = as.factor(0), y = wt))
f0 + geom_violin()

#Compartive
f1 = ggplot(data = mtc, aes(x = as.factor(cyl), y = wt))
f1 + geom_violin()
f2 = ggplot(data = mtc, aes(x = as.factor(am), y = wt))
f2 + geom_violin()

#More comparitive
f2 + geom_violin(aes(fill = as.factor(cyl)))
View(mtc)
sum(cyl == 8)
nrow(filter(mtc, cyl == 8 & am == 1))

#Coordinate flip
f1 + geom_violin() + coord_flip()
f2 + geom_violin() + coord_flip()

#Colouring
f1 + geom_violin(aes(fill = as.factor(cyl)))
f1 + geom_violin(aes(fill = as.factor(cyl))) + scale_fill_manual(values = c("Red", "Blue", "Sienna"))
f1 + geom_violin(aes(fill = as.factor(cyl))) + scale_fill_brewer(palette = "Set1")
f1 + geom_violin(aes(fill = as.factor(cyl))) + scale_fill_brewer(palette = "Set1", direction = -1)

f1 + geom_violin(aes(fill = cyl))
f2 + geom_violin(aes(fill = am))

#Trim
f3 = ggplot(data = mtc, aes(x = as.factor(cyl), y = wt))
f3 + geom_violin(trim = FALSE)

#Marking quartiles
f2 + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), trim = FALSE)
f2 + geom_violin(draw_quantiles = c(seq(from = 0.25, to = 0.75, by = 0.25)), trim = FALSE)

#Statistical summary
#Marking mean
?stat_summary
f1 + geom_violin(trim = FALSE) + stat_summary(fun.y = mean, geom = "point", na.rm = TRUE, shape = "diamond", size  = 3)
f2 + geom_violin(trim = FALSE) + stat_summary(fun.y = max, geom = "point", na.rm = TRUE)

#Boxplot & violin plot together
f3 = ggplot(data = mtc, aes(x = as.factor(am), y = wt))
f3 + geom_violin(trim = FALSE)  + geom_boxplot(width = 0.1, aes(fill = as.factor(am)), outlier.size = 0.2)

                                   