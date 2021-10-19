mtcars[1:5, c("mpg", "vs", "gear")]
attach(mtcars)
#SUBSETS
mtcars[mtcars$mpg > 20, ]
subset(mtcars, mpg > 20)         
filter(mtcars, mpg > 20)

#FILTER COMMAND
#Is used to extract data according to a given condition

#Question 1: List out all values where the number of cylinders equals 6
#Question 2: List all the automatic cars
#Question 3: List all the automatic cars with more than 4 cylinders
#Question 4: List all the cars with mpg < 25

#Answer 1:
filter(mtcars, cyl == 6)
mtcars[mtcars$cyl == 6, ]
subset(mtcars, mtcars$cyl == 6)

#Answer 2:
filter(mtcars, am == 1)
mtcars[mtcars$am == 1, ]
subset(mtcars, am == 1)

#Answer 3:
filter(mtcars, cyl >  4)
mtcars[mtcars$cyl > 4, ]
subset(mtcars, cyl > 4)

#Answer 4:
filter(mtcars, mpg < 25)
mtcars[mtcars$mpg < 25, ]
subset(mtcars, mpg < 25)

#SELECT COMMAND
#Is used when we must extract columns by name and discard the rest
select(mtcars, cyl, mpg)

#Question 1: Select and display the 1st 3 columns
#Question 2: Display the 1st and 3rd column
#Question 3: Display all column names containing 'c'
#Question 4: Display the heaviest car's details

#Answer 1:
select(mtcars, 1:3)

#Answer 2:
select(mtcars, c(1, 3))

#Answer 3:
colnames(select(mtcars, contains('c')))

#Answer 4:
subset(mtcars, mtcars$wt == max(mtcars$wt))

#Select all cars starting with c and ending with p
select(mtcars, starts_with('c') & ends_with('p'))

#Selecting negative conditions
select(mtcars, -contains('a'))

#Find heaviest automatic and manual vehicle
attach(mtcars)
filter(mtcars, am == 1) %>% subset(wt == max(wt))
filter(mtcars, am == 0) %>% subset(wt == max(wt))
