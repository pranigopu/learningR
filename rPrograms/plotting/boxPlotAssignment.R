#Masking the data set's fields
attach(airquality)

#----Structure of data set
str(airquality)
#----Boxplot for ozone readings
boxplot(Ozone~(!is.na(Month)), xlab = " ")

#More data values are above the median

#----How many outliers observed and where
#2

#----Make this box plot horizontal
boxplot(Ozone~Month, horizontal = TRUE)

#----Make it in orange colour with brown border
boxplot(Ozone~Month, col = "orange", border = "brown")

#----Make different box plots for each month
boxplot(Ozone~Month)

#----Which month is hotter
#8th month, whatever that is

