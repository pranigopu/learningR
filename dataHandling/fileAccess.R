#READING TABLES
#Read table
#----Using url
mydata1 = read.table("http://assets.datacamp.com/blog_assets/chol.txt", header = TRUE)
mydata1
#----Using path (or file name if the file is in the same folder)
mydata2 = read.table("iris.txt", header = TRUE, sep = ',')
mydata2

#Read comma separate values (csv)
mydata3 = read.csv("churn.csv", header = TRUE)
mydata3

#NOTES
#header = TRUE means the first row is to be taken as field names
#sep defines the character by which the values are to be differentiated

#Summary
summary(mydata2)
dim(mydata2)
summary(mydata3)
dim(mydata3) 