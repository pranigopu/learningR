#Create a table with student no., gender, marks (coded)
#Make a bivariate table
#Margin table for (y, 1)
#Margin table for (y, 2)

student_no = c(1, 2, 3, 4, 5, 6, 7, 8)
gender = c('m', 'f', 'f', 'm', 'f', 'm', 'f', 'f')
marks = c(1, 2, 2, 3, 3, 3, 2, 2)
as.factor(marks)
data = data.frame(student_no, gender, marks)
data
tbl = table(gender, marks)
tbl

#--------MARGINAL FREQUENCIES
#No. of males and females
margin.table(tbl, 1)

#No. of people per marks range
margin.table(tbl, 2)

#--------PROPORTIONS
#Proportion of people in each group out of total
prop.table(tbl)

#Proportion of people in each group out of each row (e.g. proportion of males with certain marks)
prop.table(tbl, 1)

#Proportion of people in each group out of each column (e.g. proportion of 1 mark people who are males)
prop.table(tbl, 2)

#Changing the number significant decimal digits
options(digits = 3)
prop.table(tbl)
prop.table(tbl, 1)
prop.table(tbl, 2)
?prop.table

barplot(main = "Marks", tbl, beside = TRUE, xlab = "Marks", ylab = "No. of students", legend(10, 10, legend))

