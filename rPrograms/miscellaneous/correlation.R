#Simple correlation
cor(mtcars$mpg, mtcars$drat)

#Specifying method
cor(mtcars$mpg, mtcars$drat, method = "pearson")
cor(mtcars$mpg, mtcars$drat, method = "spearman")
cor(mtcars$mpg, mtcars$drat, method = "kendall")

#Correlation table
cor(mtcars)
#1st 6 columns rounded to nearest 10th
round(cor(mtcars[ , 1:6]), 1)

library("ggcorrplot")
#Computing p-values
cor_pmat(mtcars)

#Graphical representation
corr = cor(mtcars)
ggcorrplot(corr)

#Plot shape
ggcorrplot(corr, method = "circle")

#Ordering the fields to make a neater graph
ggcorrplot(corr, hc.order = TRUE)

#Excluding triangles of data
ggcorrplot(corr, type = "lower")
ggcorrplot(corr, type = "upper")

#Labels
#This prints the correlation coefficients of each square
ggcorrplot(corr, lab = TRUE)

#Shape outline colour
ggcorrplot(corr, outline.color = "magenta")

#Excluding insignificant values
ggcorrplot(corr, hc.order = TRUE, insig = "blank")

#
ggcorrplot(corr, type = "lower", outline.color = "blue", lab = TRUE, method = "circle")
