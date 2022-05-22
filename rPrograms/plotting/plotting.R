H = c(1, 4, 9, 4, 2, 4, 9, 4, 1)
barplot(H, xlab = "Boohoo", ylab = "Hehehe", col = "red")
colors(distinct = TRUE)

barplot(H, xlab = "Boohoo", ylab = "Hehehe", col = "red", space = 1)
barplot(H, xlab = "Boohoo", ylab = "Hehehe", col = "red", space = 2)
barplot(H, xlab = "Boohoo", ylab = "Hehehe", col = "red", space = 50)
barplot(H, xlab = "Boohoo", ylab = "Hehehe", col = "red", space = 100)

J = c(TRUE, TRUE, FALSE, FALSE, TRUE, TRUE)
barplot(J, xlab = "Boom", ylab = "Bam", col = "red")

mtcars
summary(mtcars)
str(mtcars)

unique(mtcars$gear)
sum(!is.na(unique(mtcars$gear)))

mtcars1 = mtcars
#Converting numeric data into factor type (i.e. category) variable
mtcars1$gear = as.factor(mtcars$gear)
summary(mtcars1)

mtcars1$vs = as.factor(mtcars$vs)
summary(mtcars1)

#------------------------Barplots again
#For mtcars1$gear
K = table(mtcars1$gear)
barplot(main = "Gear frequencies", xlab = "Gears", ylab = "Frequency", K)
barplot(main = "Gear frequencies", xlab = "Gears", ylab = "Frequency", K, horiz = TRUE)

#Stacked bar charts
L = table(mtcars1$vs, mtcars1$gear)
barplot(main = "Gear frequencies along with VS frequencies", xlab = "Gears", ylab = "Frequency", L)
barplot(main = "Gear frequencies along with VS frequencies", xlab = "Gears", ylab = "Frequency", L, horiz = TRUE)
barplot(main = "Gear frequencies along with VS frequencies", xlab = "Gears", ylab = "Frequency", L, col = c("red", "magenta"), horiz = TRUE, legend = rownames(L))
barplot(main = "Gear frequencies along with VS frequencies", xlab = "Gears", ylab = "Frequency", L, col = c("red", "magenta"), horiz = TRUE, legend = rownames(L))

#Grouped bar charts
barplot(main = "Gear frequencies along with VS frequencies", xlab = "Gears", ylab = "Frequency", L, col = c("red", "magenta"), beside = TRUE, legend = rownames(L))

#------------------------Scatterplots
plot(mtcars$mpg, mtcars$hp)
plot(mtcars$mpg, mtcars$hp, col = "magenta")

#------------------------Piechart
M1 = c(10, 20, 40, 80)
M2 = c("Boom", "Shaka", "Laka", "Bam")
pie(M1, labels = M2, main = "Boomshakalakabam")
pM1 = round(M1 / sum(M1) * 100)
pie(pM1, labels = M2, main = "Boomshakalakabam")
pie(pM1, labels = M2, main = "Boomshakalakabam", col = rainbow(length(M2)))

#------------------------Histograms
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 12, col = "orangered")
hist(mtcars$hp, breaks = 20, col = "salmon2")

#------------------------Changing point shape and size in scatterplot
#cex = character expansion
#pch = plotting character
plot(mtcars$hp, pch = 3)
plot(mtcars$hp, pch = 5, cex = 50)

