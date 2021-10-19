#Quartile, decile and percentile can be calculated using ntile()
#ntile() is used to divide the data into n bins
mydata = mtcars
ntile(mydata$mpg, 4)

#Here, we get the quartile ranks for each data points
mutate(mydata, quartile.rank = ntile(mydata$cyl, 4))
mutate(mydata, percentile.rank = ntile(mydata$cyl, 100))
