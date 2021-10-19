#SUMMARISE
library(dplyr)
#Calculate the mean temperature and temperature's standard deviation for all months in the airquality dataset
aq = tbl_df(airquality)
#----NOTE: na.rm => remove NA values
summarise(aq, mean(Temp, na.rm = TRUE), sd(Temp, na.rm = TRUE))

#Calculate the mean temperature for each month separately and display it
tapply(aq$Temp, aq$Month, mean, na.rm = TRUE)

#Classification of temperatures as hot and cold
tbl1 = aq %>% mutate(Hot.Cold = factor(Temp - mean(Temp) > 0, labels = c("Cold", "Hot")))
View(tbl1)     