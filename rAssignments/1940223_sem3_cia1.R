setwd("~/rPrograms")
data = read.csv("datamor.csv", header = TRUE)
data

#Number of variables
ncol(data)

#Number of observtions
nrow(data)

#Extracting variables 2 and 3 and assigning to vectors
height = data[ , 2]

weight = data[ , 3]
 
#Number of blood groups
bloodgroups = unique(data[ , 6], incomparables = FALSE)
sum(!is.na(bloodgroups))

#Unique smoke categories
smokecats = unique(data[ , 5])
smokecats

#Cholestrol > 300
sum(data$CHOL > 300)

#Mean height for ones with MORT = alive
noo = NROW(data$MORT == "alive")
s = sum(data$HEIGHT, data$MORT == "alive")
s / noo

#Tallest guy with O-blood group
max(data$HEIGHT, data$MORT == "alive")

#Number of nonsmokers alive below 40 years
sum(data$MORT == "alive" & data$AGE < "alive")