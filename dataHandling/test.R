#Question 1------------
name = c("Alex", "Lily", "Mark", "Oliver", "Martha", "Lucas", "Caroline")
age = c(25, 31, 23, 52, 76, 49, 26)
height = c(177, 163, 190, 179, 163, 183, 164)
weight = c(57, 69, 83, 75, 70, 83, 53)
sex = c('f', 'f', 'm', 'm', 'f', 'm', 'f')
print("Original dataframe")
name
age
height
weight
sex 
df = data.frame(name, age, height, weight, sex)
df
print(str(df))

#Question 2------------
working = c("yes", "no", "no", "yes", "yes", "no", "yes")
df1 = data.frame(df$name, working)
df2 = data.frame(df, working = df1$working)
df2
nrow(df2)
ncol(df2)

#Question 3------------
summary(df2)

#Question 4------------
df2$height

#Question 6
head(df2, 2)

#Question 7
df2[3:5, 1:3]

#Question 9
sum(df2$weight, df2$female == 'f') / nrow(df2)