library(MASS)
summary(Cars93)
str(Cars93)
attach(Cars93)

newPrice = cut(Price, c(0, 12, 20, max(Price)))
levels(newPrice) = c("Cheap", "Okay", "Expensive")
newMPG = cut(MPG.highway, c(0, 20, 30, max(MPG.highway)))
levels(newMPG) = c("Gas Guzzler", "Okay", "Economic")

table(Type)
table(newPrice, Type)
table(newPrice, Type, newMPG)

#Barplot: Price levels per type
barplot(table(newPrice, Type), beside = T)

#Barplot: Types per price level
barplot(table(Type, newPrice), beside = T)

#Boxplot: Types per price level
boxplot(Price~Type, col = rainbow(6))
summary(Price)

#Multiple boxplots
pairs(data.frame(Price, Type, MPG.highway))
