setwd("/Users/pranav/Documents/Study/computerScience/programming/r/data")

# A HELPFUL FUNCTION
naPresent = function(values)
{
  flag = 0
  for(i in values)
  {
    if(is.na(i))
    {
      flag = 1
      break
    }
  }
  if(flag == 0){FALSE}else if(flag > 0){TRUE}
}

# DATA PROCESSING
myData = read.csv("weatherAustralia.csv")[c(1, 2, 4, 7, 23)]
head(myData)
summary(myData)
max = length(myData$Date)
x1 = x2 = y = c()
# REMOVING ROWS WITH NA VALUES
for(i in c(1:(max + 1)))
{
  x1_data = myData$RainTomorrow[i]
  x2_data = myData$MaxTemp[i]
  y_data = myData$Sunshine[i]
  if(!naPresent(c(x1_data, x2_data, y_data)))
  {
    if(x1_data == "Yes"){x1_data = 1}else{x1_data = 0}
    x1 = c(x1, x1_data)
    x2 = c(x2, x2_data)
    y = c(y, y_data)
  }
}
myData = data.frame(x1, x2, y)
head(myData)

# PLOTS (TO VISUALISE DATA) AND CORRELATION
plot(x2, y,
     type = "p",
     col = "maroon",
     pch = 16,
     xlab = "Maximum temperature",
     ylab = "Sunshine",
     main = "Sunshine w.r.t. maximum temperature")
cor(y, x2, method = "pearson")

boxplot(y~x1,
     type = "p",
     col = "maroon",
     xlab = "Rainfall tomorrow?",
     ylab = "Sunshine",
     main = "Sunshine w.r.t. whether rain falls tomorrow")
cor(y, x1, method = "pearson")

cor(x1, x2, method = "pearson")

# LINEAR REGRESSION MODEL
model = lm(y~x1 + x2, data = myData)
model
summary(model)
