x <- matrix(nrow = 4, ncol = 2, data = c(1, 2, 3, 4, 5, 6, 7, 8))
x

#One can access a signle element of the matrix with x[i, j]
x[3, 2]
#Data can be entered by rows
x <- matrix(nrow = 4, ncol = 2, data = c(1, 2, 3, 4, 5, 6, 7, 8), byrow = TRUE)
x

#Getting properties of a matrix
dim(x) #dimensions of x
attributes(x) #dimensions of x
nrow(x) #No. of rows in x
ncol(x) #No. of columns in x
mode(x) #Type of storage

#Assigning one number to all the elements in a matrix
y <- matrix(nrow = 2, ncol = 3, data = 4)
y

#Constructing a diagonal matrix of dimension 2
d <- diag(1, nrow = 2, ncol = 2)
d

#------------------------------------------------------------

#Transposing a matrix
x
t(x)

#Matrix multiplication with a constant
4 * x

#Matrix multiplication
#Method 1
xt <- t(x) %*% x
xt

#Matrix multiplication with its transpose
xt <- crossprod(x)
xt

#------------------------------------------------------------

#Access to rows, columns and submatrices
x

#Row no. 2
x[2, ]

#Column no. 1
x[ , 1]

#If you go out of bounds
x[ , 3]

#Third to fouth row, and first to second column
x[3:4, 1:2]

#------------------------------------------------------------

#Finding inverse of a matrix
#Error 1: matrix must be square
solve(x)
#Error 2: determinant must not be zero
z <- matrix(nrow = 2, ncol = 2, data = c(1, 0, 3, 0))
solve(z)
#Proper
z <- matrix(nrow = 2, ncol = 2, data = c(1, 2, 3, 4))
solve(z)

#------------------------------------------------------------

#Solve command

#A matrix can be used to hold the coefficients and constant sums of simultaneous linear equations

#E.g. 3x + 2y = 8, x + y = 2
a <- matrix(c(3, 1, 2, 1), nrow = 2, ncol = 2)
b <- matrix(c(8, 2), nrow = 2, ncol = 1)
solve(a, b)
#We get x = 4 and y = -2

#E.g. 
#8x + 14y = 27
#24x + 36y = 58
#3x + 4y = 29
a <- matrix(nrow = 3, ncol = 3, data = c(8, 14, 4, 24, 36, 5, 3, 4, 2), byrow = TRUE)
a
b <- matrix(nrow = 3, ncol = 1, data = c(27, 58, 29))
b
solve(a, b)

#------------------------------------------------------------

#New ways of entering data, and concatenating rows and columns

m <- matrix(1:6, 2, 3, byrow = TRUE)
#       data | row | col
m
m <- rbind(m, 7:9)
m
m <- cbind(m, 6:8)
m

#------------------------------------------------------------

#Changing and deleting rows and columns
m
#Changing column
m[ , 2] <- c(10, 10, 10)
m
#Changing row
m[3, ] <- c(9, 8, 9, 10)
m
#Deleting column
m <- m[ , -2]
m
#Deleting row
m <- m[-3, ]
m

#------------------------------------------------------------

#Live questions

#Question 1: Create the given matrix
mat <- matrix(c(2, 1, 1, 9, 5, 2, 2, 10, 6, 3, 3, 11, 7, 4, 4, 12, 8, 5, 5, 13), 4, 5)
mat

#Question 2: Create mat1 by extracting values from mat
mat1 <- mat[2:4, 3:5]
mat1

#Question 3: Create mat2 by doubling mat1
mat2 = 2 * mat1
mat2

#Question 4: Add a new row then column to mat2
mat3 <- rbind(mat2, 1:3)
mat3
mat3 <- cbind(mat3, 1:4)
mat3

#Question 5: Multiplication of mat3
mat4 = mat3 %*% mat3
mat4

#Question 6: Remove one row then one column from mat4
mat5 = mat4[-3, ]
mat5
mat5 = mat5[ , -3]
mat5

#Question 7: Inverse of mat1
mat6 = solve(mat1)

#Question 8: Transpose of mat2
mat2
mat7 = t(mat2)
mat7
