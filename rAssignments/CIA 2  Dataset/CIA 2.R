setwd("~/rPrograms/CIA 2  Dataset")
mydata1 = read.csv("NTCA - TIGERNET.csv", header = TRUE)


#THE DATA SET
head(mydata1, 6)


#INTRODUCTORY INFORMATION
#This data set is about the observed mortalities and seizures of tigers
#in the different stats of India, in the year 2019.

sum(!is.na(unique(mydata1$Location)))
sum(!is.na(unique(mydata1$State)))

#The data set takes its data from 
#59 different locations
#16 different states


#AGE RELATED
total = sum(!is.na(mydata1$Date))
i = 1
tenPlusCount = 0
fivePlusCount = 0
onePlusCount = 0
oneMinusCount = 0

while(i <= total)
{
  if(!is.na(mydata1$Age[i]))
  {
    if(mydata1$Age[i] >= 10 && mydata1$Mortality...Seizure == "Mortality")
    {
      tenPlusCount = tenPlusCount + 1
    } else if(mydata1$Age[i] >= 5 && mydata1$Mortality...Seizure == "Mortality")
    {
      fivePlusCount = fivePlusCount + 1
    } else if(mydata1$Age[i] >= 1 && mydata1$Mortality...Seizure == "Mortality")
    {
      onePlusCount = onePlusCount + 1
    } else if(mydata1$Age[i] < 1 && mydata1$Mortality...Seizure == "Mortality")
    {
      oneMinusCount = oneMinusCount + 1
    }
  }
  i = i + 1
}
#Number of tigers 10 years or older'
tenPlusCount

#Number of tigers 5 years or older but less than 10 years
fivePlusCount

#Number of tigers 1 year or older but less than 5 years
onePlusCount

#Number of tigers less than 1 year old
oneMinusCount

#Number of tigers with age unknown
sum(is.na(mydata1$Age))


#With the above results, we see that a majority of the tigers' ages are unknown.
#However, going with what is known, we see maximum mortality in adult tigers from 1 to 5 years old.
#The high mortality among adult tigers may suggest...

#1. Higher fatal confrontations between tigers of 
#   these ages or between humans and tigers of these ages
#2. Greater number of tigers in this age group


#GRAPHS
#--------Mortality
#Number of mortalities per state
MP_m = sum(mydata1$Mortality...Seizure == "Mortality" & mydata1$State == "Madhya Pradesh")
M_m = sum(mydata1$Mortality...Seizure == "Mortality" & mydata1$State == "Maharashtra")
KA_m = sum(mydata1$Mortality...Seizure == "Mortality" & mydata1$State == "Karnataka")
TN_m = sum(mydata1$Mortality...Seizure == "Mortality" & mydata1$State == "Tamil Nadu")
U_m = sum(mydata1$Mortality...Seizure == "Mortality" & mydata1$State == "Uttarakhand")
AS_m = sum(mydata1$Mortality...Seizure == "Mortality" & mydata1$State == "Assam")

data1 = c(MP_m, M_m, KA_m, TN_m, U_m, AS_m)
data1
states = c("MP", "Maharashtra", "Karnataka", "Tamil Nadu", "Uttarakhand", "Assam")

MP_s = sum(mydata1$Mortality...Seizure == "Seizure" & mydata1$State == "Madhya Pradesh")
M_s = sum(mydata1$Mortality...Seizure == "Seizure" & mydata1$State == "Maharashtra")
KA_s = sum(mydata1$Mortality...Seizure == "Seizure" & mydata1$State == "Karnataka")
TN_s = sum(mydata1$Mortality...Seizure == "Seizure" & mydata1$State == "Tamil Nadu")
U_s = sum(mydata1$Mortality...Seizure == "Seizure" & mydata1$State == "Uttarakhand")
AS_s = sum(mydata1$Mortality...Seizure == "Seizure" & mydata1$State == "Assam")

data2 = c(MP_s, M_s, KA_s, TN_s, U_s, AS_s)
data2

#Mortalities per state
barplot(main = "Mortalities per state", data1, xlab = "MP        M         KA         TN          U         AS", ylab = "No. of mortalities")

#Seizures per state
barplot(main = "Seizures per state", data2, xlab = "MP        M         KA         TN          U         AS", ylab = "No. of seizures")

#Seizures are very few overall.
#They are primarily in MP and Assam

#Seizures and mortalities per state
X = matrix(c(data1, data2), nrow = 2, byrow = TRUE)
barplot(main = "Total tiger mortalities and seizures", X, xlab = "MP        M         KA         TN          U         AS")

#With this graph, we can see that
#1. Maximum observations, mortalities and seizures are in MP
#2. Assam has the highest seizure to mortality ratio, 
#   meaning a larger portion of observed tigers were 
#   seized rather than found dead
#3. No other state has records of seizures

#--------Inside / outside reserve
a = sum(mydata1$Inside...Outside == "Inside Tiger Reserve" | mydata1$Inside...Outside == "Inside Tiger  Reserve")
b = sum(mydata1$Inside...Outside == "Outside Tiger Reserve" | mydata1$Inside...Outside == "Outside tiger Reserve")
pie(main = "No. of observations inside and outside tiger reserves", c(a, b), labels = c("Inside Tiger Reserve", "Outside Tiger Reserve"))

#A majority of observations were taken inside tiger reserves
#However, this majority is not significant, as a considerable number of observations are from outside tiger reserves as well


#STRUCTURE
str(mydata1)

#This data set is largely comprised of non-numeric fields.
#There are large numbers of unknown values in the fields 'Sex' and 'Age'.
#There is a considerable amount of repetition in the data, since thought the data set is large, the unique values are much lesser


#SUMMARY
summary(mydata1)

#There are 90 observations and 7 variables
#The data set includes a very diverse range of ages
#Maximum observations are from Madhya Pradesh
#Most observed tigers are male
#Out of the 16 states in the data set, only 6 have a significant amount of data
#There is a considerable amount of unavailable data in the fields 'Sex' and 'Age'



