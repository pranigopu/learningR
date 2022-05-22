library(lattice)
?lattice


ToothGrowth
?ToothGrowth

tab1 = ToothGrowth
tab1$dose = as.factor(tab1$dose)

stripplot(len~dose, data = tab1, jitter.data = FALSE, pch = 19)
stripplot(len~dose, data = tab1, jitter.data = TRUE, pch = 19)

#-------Producing graphs 3 variables at a time
bwplot(len~dose|supp, data = tab1, layout = c(3, 1))
#layout = c(3, 1) specifies the column and row in the plot screen until which the graph must plot

bwplot(len~dose|supp, data = tab1, layout = c(2, 1), panel = panel.violin)
       
stripplot(len~dose|supp, data = tab1, jitter.data = TRUE, pch = 19)

#--------Density plot
densityplot(~len, groups = dose, data = tab1, plot.points = TRUE, auto.key = TRUE)
densityplot(~len, data = tab1, plot.points = FALSE)

#--------Lattice package histogram function
histogram(~len, data = tab1, breaks = 100)

#--------Exercises
Titanic
apply(Titanic, 2, sum)

barplot(Titanic$Survived~Titanic$Class)
