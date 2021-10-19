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
completeData = read.csv("weatherAustralia.csv")[c(5, 14, 18)]
head(completeData)
max = length(completeData$Rainfall)
x1 = x2 = y = c()
# REMOVING ROWS WITH NA VALUES
for(i in c(1:(max + 1)))
{
  x1_data = completeData$Cloud9am[i]
  x2_data = completeData$Humidity9am[i]
  y_data = completeData$Rainfall[i]
  if(!naPresent(c(x1_data, x2_data, y_data)))
  {
    x1 = c(x1, x1_data)
    x2 = c(x2, x2_data)
    y = c(y, y_data)
  }
}
myData = data.frame(x1, x2, y)

# CHECKING CORRELATION BETWEEN VARIABLES
cor(myData, method = "pearson")
cor.test(x1, y)
cor.test(x2, y)
cor.test(x1, x2)
# GRAPHICALLY CHECKING CORRELATION
plot(x1, y,
     type = "p",
     main = "Cloud at 9AM and Rainfall",
     xlab = "Cloud",
     ylab = "Rainfall",
     col = "blue",
     pch = 16,
     las = 1)
plot(x2, y,
     type = "p",
     main = "Humidity at 9AM and Rainfall",
     xlab = "Humidity",
     ylab = "Rainfall",
     col = "blue",
     pch = 16,
     las = 1)

# FITTING LINEAR REGRESSION MODEL TO THE DATA
model = lm(y~., myData)
# Alternatively you could do
# model = lm(y~x1 + x2)

# SCATTERPLOTS WITH ESTIMATED LINEAR REGRESSION LINE
plot(x1, y,
     type = "p",
     main = "Cloud at 9AM and Rainfall",
     xlab = "Cloud",
     ylab = "Rainfall",
     col = "blue",
     pch = 16,
     las = 1)
# Adding estimated regression line
abline(model, lwd = 2)
plot(x2, y,
     type = "p",
     main = "Humidity at 9AM and Rainfall",
     xlab = "Humidity",
     ylab = "Rainfall",
     col = "blue",
     pch = 16,
     las = 1)
# Adding estimated regression line
abline(model, lwd = 2)

summary(model)
anova(model)

# 95& confidence interval
confint(model, level = 0.95)