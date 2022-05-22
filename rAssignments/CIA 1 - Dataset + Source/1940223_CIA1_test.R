#Question 2
library(lattice)
data = read.csv("sports.csv", header = TRUE, sep = ';')
attach(data)
data

#Question 3
table = table(as.factor(team), gd)
table
barplot(main = "Goals per team", table, ylab = "Goals done")

#Question 4
densityplot(main = "Density plot of goals received per team", ~gs, groups = team, legend = rownames(team))

#Question 5
histogram(main = "Win matches per team", win, groups = team)

#Question 6
?cut
xyplot(main = "Goals received per team", gs~team, cut = TRUE)

#Question 7
qqplot(main = "Wins in relation to injuries per match", inj, win, groups = team)

#Question 8
cloud(gs~gd*team, data = data)
