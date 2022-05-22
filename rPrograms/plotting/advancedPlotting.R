lapply(mtcars,min)
?list

######18-01-2020

library(lattice)
?lattice


ToothGrowth
?ToothGrowth

str(ToothGrowth)

tab1 = ToothGrowth
tab1
tab1$dose<-as.factor(tab1$dose)
tab1
#--------To get the first few observation
head(ToothGrowth)

#--------Default plot
xyplot(len~dose,data=tab1, groups = supp, auto.key = TRUE)
xyplot(len~supp,data=tab1, groups = dose, auto.key = TRUE)
xyplot(len~dose,data=tab1, groups = supp,type=c("p","smooth"), scales = "free")
xyplot(len~dose|supp, groups = supp, data=tab1, type=c("p","smooth"), scales = "free")  

#--------3D box plot
cloud(len~len*dose, groups = supp, data = tab1, auto.key = TRUE)
cloud(len~len*dose, groups = supp, data = tab1, auto.key = TRUE)

bwplot(len~dose, data=tab1, xlab = "DOSE", ylab = "LENGTH")
bwplot(dose~len, data=tab1, xlab = "DOSE", ylab = "LENGTH")
bwplot(len~dose, data=tab1, panel=panel.violin, xlab="DOSE", ylab = "LENGTH")


