#Classify cars as gas guzzlers, normal and economic
mtc = mtcars
mean(mtc$mpg)
#1st attempt
mutate(mtc, economy.class = factor(mpg > mean(mpg), labels = c("Economic", "Gas guzzler")))
??classification
