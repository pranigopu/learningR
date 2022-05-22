#Syntax:
#apply(matrix, margin, function)
#Inputs matrices or dataframes and outputs vector
#margin = 1 => function is applied to each row
#margin = 2 => function is applied to each column

M1 = matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)
M1
apply(M1, 1, sum)
apply(M1, 2, sum)

#VARIATIONS OF APPLY

#lapply(list, function)
#Inputs lists and outputs lists
M2 = c("aaa", "bbb", "ccc")
lapply(M2, toupper)

#sapply(list, function)
#Inputs lists and outputs vectors
#Otherwise same functionality as lapply
sapply(M2,toupper)

#tapply(array1, array2, function)
#Splits the array1 data based on array2 data and applies function
M3 = c('a', 'b', 'c', 'd', 'e', 'e', 'e', 'f', 'f')
tapply(M1, M3, sum)

#mapply(function, array1, array2...)
#Performs function on multiple arrays
M4 = c(2, 4, 6, 8, 10, 12, 14, 16, 18)
mapply(sum, M1, M4)

mapply(rep, c('A', 'F'), 8:1)
