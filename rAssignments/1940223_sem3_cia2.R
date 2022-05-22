#Create a dataset with 10 values and 3 variables.
#The variables are to be Name, Marks 1, Marks 2

#Q1: Create a barplot for Marks 1
#Q2: Create a stack bar for students 1-5
#Q3: Create a pie chart for students 6-10
#Q4: Create a histogram for Marks 2
#Q5: Create a scatter plot for Marks 1 and Marks 2
#Q6: Change the display from dots to squares

name = c("ABC", "BCD", "CDF", "DEF", "EFG", "FGH", "GHJ", "HJK", "JKL", "KLM")
mark1 = c(60, 70, 80, 69, 78, 89, 67, 45, 89, 99)
mark2 = c(45, 56, 56, 78, 79, 66, 99, 100, 100, 77)
data = data.frame(name, mark1, mark2)

#Q1
barplot(main = "Subject 1 marks", data$mark1, xlab = "Students", ylab = "Marks")

#Q2
X1 = c(data$mark1)
X2 = c(data$mark2)
A = matrix(c(X1[1:5], X2[1:5]), nrow = 2, ncol = 5, byrow = TRUE)
barplot(main = "Marks per student, for students 1-5", A, xlab = "Students", ylab = "Marks")
#CONCLUSION: mark2 for students 1-5does not rise even as total marks is larger

#Q3
X3 = c(data$mark1)
X3 = X3[6:10]
X4 = c(data$mark2)
X4 = X4[6:10]
B = matrix(c(X3) + c(X4))
pie(B, labels = name[6:10])
#CONCLUSION: Students 6-10 received similar total marks

#Q4
hist(main = "Frequency of subject 2 marks", xlab = "Marks", data$mark2)
#CONCLUSION: Greatest proportions of students received grades between 70-80 and 90-100

#Q5
plot(main = "Correlation between subject 1 and 2 marks", xlab = "Subject 1 marks", ylab = "Subject 2 marks", data$mark1, data$mark2)
#CONCLUSION: There does not seem to be much correlation between mark1 and mark2

#Q6
plot(main = "Correlation between subject 1 and 2 marks", xlab = "Subject 1 marks", ylab = "Subject 2 marks", data$mark1, data$mark2, pch = 0)
