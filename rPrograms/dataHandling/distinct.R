library(dplyr)
mydata = mtcars

#Removing duplicate rows
distinct(mtcars)

#Using particular variable
distinct(mtcars, carb, .keep_all = TRUE)
distinct(mtcars, carb, .keep_all = FALSE)

#Using combination of variables
distinct(mtcars, carb, drat, .keep_all = TRUE)
distinct(mtcars, carb, drat, .keep_all = FALSE)

#.keep_all
#This option, when set TRUE, chooses distinct rows according to the variables while showing all other fields
#When set FALSE, it only shows the selected variables