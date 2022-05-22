#Create a vector containing numbers, and insert some characters in between
x = c(3, 5, 'c', '^', '6', 4, 65, 23)
is.nan(x)
y = c(1, 3, NaN, 5, NaN)
is.nan(y)

#Create a vector containing NaN
#Find the positions in the vector where this occurs
#Also find the number of such occurences
z = c(1, 3, NaN, 5, NaN, 6, NaN)

sum(is.nan(z))
