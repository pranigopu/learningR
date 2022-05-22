#' DATA SET
data = read.csv("~/Documents/Study/computerScience/programming/r/data/weatherAustralia.csv")[c(1, 2, 3, 5, 9, 14)]
head(data)
#' This data is a time-series data, with many possible observation fields, from which we will focus on MinTemp
#' (minimum daily temperature). Other columns have been omitted in thie above display for convenience.
#' Also, since the data is taken from different locations across Australia,
#' we will focus on data taken for Albury alone.
#' Hence, will take measures for Albury alone...
#------------
# MINIMUM TEMPERATURE
mintemp = data$MinTemp
mintemp = mintemp[data$Location == "Albury"]
#------------
# RAINFALL
rainfall = data$Rainfall
rainfall = rainfall[data$Location == "Albury"]
#------------
# WIND GUST SPEED
windgustspeed = data$WindGustSpeed
windgustspeed = windgustspeed[data$Location == "Albury"]
#------------
# HUMIDITY AT 9AM
humidity = data$Humidity9am
humidity = humidity[data$Location == "Albury"]
#========================
#' OBTAINING TIME-SERIES OBJECTS
#------------
#' Inspecting the dates (useful when defining the time-series end points)...
dates = as.Date(data$Date, "%d/%m/%y")
summary(dates)
#' Hence, the data is taken daily from 2008-12-01 to 2017-06-25.
#' (Default format is yyy-mm-dd, so to read the dates appropriately from the data set, I have specified the format
#' dd/mm/yyyy, since this is how it is present in the data set)
#------------
#' We already have the various measures for Albury as a numerical vectors,
#' so we must simply convert it to a time-series objects (i.e. objects of class 'ts')...
mintemp = ts(mintemp, start = c(2008, 12, 1), end = c(2017, 6, 25), frequency = 365)
rainfall = ts(rainfall, start = c(2008, 12, 1), end = c(2017, 6, 25), frequency = 365)
windgustspeed = ts(windgustspeed, start = c(2008, 12, 1), end = c(2017, 6, 25), frequency = 365)
humidity = ts(humidity, start = c(2008, 12, 1), end = c(2017, 6, 25), frequency = 365)
#______
#' NOTE:
# c(2008, 12, 1) => 2008-12-01
# c(2017, 6, 25) => 2017-06-25
# frequency = 365 => the data has daily observations
# (12 => monthly, 4 => quarterly, 1 => annually)
#========================
#' TIME PLOTS
#------------
#' GENERAL FUNCTION TO MAKE TIME PLOT
timeplot = function(timeseries, name){
  ts.plot(timeseries,
          main = paste(name, "(Australia, Albury, 2008-2017)"),
          ylab = name,
          xlab = "Year",
          col = "purple")}
#------------
#' PLOTS
timeplot(mintemp, "Minimum temperature")
timeplot(rainfall, "Rainfall")
timeplot(windgustspeed, "Wind gust speed")
timeplot(humidity, "Humidity at 9AM")