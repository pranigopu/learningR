#FUNCTIONS
#f = function(<arguments>) {<definition>}
#Definitions
f1 = function(a, b)
{
  a + b
}
#Call
f1(1, 3)
#------------------------
#Example
mean = function(a, b, c)
{
  (a + b + c) / 3 
}

mean(1, 2, 3)

#APPLYING FUNCTIONS ON NON-PRIMITIVE DATATYPES
#Apply function can be used as a substitute to loops
#1. apply(matrix or dataframe, margin, function)
#2. lapply(object, function)
#3. sapply(object, function)
#4. tapply(object, index, function)
#5. mapply(function, object1, object2...)

#1. 
#Inputs matrices or dataframes and outputs vector, list or array
#margin = 1 => function is applied to each row
#margin = 2 => function is applied to each column
#margin = c(1, 2) => function is applied to both rows and columns
#Vector output
apply(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ), 2, sum)
#List output
apply(mtcars, 2, median)

#apply does not work for vectors or lists, only matrices and dataframes
#It requires the object to have positive dimensions
#Below, you can see vectors and plain values have no dimensions
dim(2)
dim(c(1, 2))
#While matrices and dataframes have dimensions
dim(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ))
dim(mtcars)

#2.
#Inputs as lists and outputs lists of the same length
#(Inputs like vectors, matrices and dataframes are also read as lists)
lapply(matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ), sqrt)
#Function is independently applied on each element of the list / vector
lapply(c(1, 2, 3, 4), sum)
#Output was merely a list of 1, 2, 3, 4
#Below is a better usage
lapply(c("cat", "bat", "but", "run"), toupper)

#3. 
#Inputs as lists and outputs vectors or matrices
#Otherwise same functionality as lapply
sapply(c(1, 2, 3, 4), sum)
#Vector output
sapply(c(1, 2, 3, 4), sqrt)
#Matrix output
sapply(c("cat", "bat", "but", "run"), toupper)

#4.
#tapply makes subsets of the object where each subset is a group made based on the index
#For example, it groups iris$Sepal.Width according to iris$Species and makes each group a subset
#Then, the function is performed on each subset, and the ouptut is a dataframe
#Demonstration
data(iris)
iris
#Finding median width for each species
tapply(iris$Sepal.Width, iris$Species, median)

#Counting specimens in each species
tapply(iris$Species, iris$Species, length)

#Closer look at the grouping mechanism 
ch = c('a', 'b', 'b', 'c', 'e')
nm = c(1, 3, 4, 5, 7)
tapply(nm, ch, sum)
#(Order of the elements determines the grouping)
#Grouping was done accoring to ch
#1 fell under 'a', 3 and 4 fell under 'b' and so on
#sum was applied on each subset

#On the flip side
tapply(ch, nm, toupper)
#Grouping was done accoring to nm
#'A' fell under 1, 'B'fell under 3, another 'B' fell under 4 and so on
#toupper was applied on each subset

#Last example
ch = c('a', 'a', 'b', 'b', 'c', 'c')
st = c("ac", "aas", "efs", "asfw", "wefws", "adafrutt")
tapply(st, ch, length)
#Number of strings in each group were counted
tapply(st, ch, toupper)

#5. mapply(function, array1, array2...)
#   Performs function on multiple arrays