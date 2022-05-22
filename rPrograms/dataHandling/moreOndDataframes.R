#Creating dataframes
name = c("ABC", "CDE", "DEF", "EFG", "FGH", "GHI", "HIJ", "IJK", "JKL", "KLM")
score = c(6, 8, 4, 7, 8, 3, 9, 10, 7, 8)
attempts = c(2, 3, 4, 5, 4, 6, 1, 1, 2, 23)
qualify = c("yes", "yes", "no", "yes", "yes", "no", "yes", "yes", "yes", "yes")
print("Original dataframe: ")
print(name)
print(score)
print(attempts)
print(qualify)
df = data.frame(name, score, attempts, qualify)
df

#Printing dataframe's structure
print(str(df))

#Statistical summary
print(summary(df))

#Extracting stuff from the dataframe
#1
result = data.frame(df$name, df$score)
result

#2: 1st 3 rows
result1 = df[1:3, ]
result1

#3: 3rd and 5th row, 1st and 3rd column
result2 = df[c(3,5), c(1,3)]
result2

#Adding stuff to the dataframe
#1: Adding column
df$country = c("AA", "BB", "CC", "DD", "EE", "FF", "GG", "HH", "II", "JJ")
df

#2: Adding row
new_row_df = data.frame(name = c("XYZ", "ERQ"), score = c(6, 1), attempts = c(3, 4), qualify = c("yes", "no"), country = c("FF", "QQ"))
newdf = rbind(new_row_df, df)
newdf
